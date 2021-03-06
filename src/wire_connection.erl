-module(wire_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock, buffer = <<>>,
		mode, step = handshake, info_hash,
		choked = true, interested = false,
		queue = [], lte_ids = [], peer_supports_lte = false}). 

%% LTE = Libtorrent extension protocol http://www.rasterbar.com/products/libtorrent/extension_protocol.html
-record(queued, {piece, offset, length}).

-define(CHOKE, 0).
-define(UNCHOKE, 1).
-define(INTERESTED, 2).
-define(NOT_INTERESTED, 3).
-define(HAVE, 4).
-define(BITFIELD, 5).
-define(REQUEST, 6).
-define(PIECE, 7).
-define(CANCEL, 8).
-define(EXTENDED, 20).
-define(BITTORRENT_PROTOCOL, 19, "BitTorrent protocol").
-define(LTE_HANDSHAKE_ID, 0).
-define(LTE_UT_PEX_NAME, "ut_pex").
-define(LTE_UT_PEX_ID, 1).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Param) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Param) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Param], []),
    {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%% Outgoing connections with IP/Port
init([{InfoHash, IP, Port}]) ->
    logger:log(wire, info,
	       "Connecting to ~s :~b for ~s~n", [inet_parse:ntoa(IP), Port, prit_util:info_hash_representation(InfoHash)]),
    Opts = case IP of
	       {_, _, _, _, _, _, _, _} -> [inet6];
	       _ -> []
	   end,
    {ok, Sock} = gen_tcp:connect(IP, Port, [binary,
					    {active, true}
					    | Opts]),
    State = #state{sock = Sock, info_hash = InfoHash, mode = client},
	send_full_handshake(State),
    {ok, State};

%% Incoming connections, InfoHash to be received
init([Sock]) ->
    logger:log(wire, info,
	       "Connection from ~p~n", [Sock]),
    link(Sock),
    {ok, #state{sock = Sock, mode = server}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(go, #state{sock = Sock} = State) ->
    ok = inet:setopts(Sock, [{active, true}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Sock, Data}, #state{sock = Sock,
				      buffer = Buffer} = State1) ->
    State2 = State1#state{buffer = list_to_binary([Buffer, Data])},
	% logger:log(wire, info,"Trying to match input of ~w~n~s~n", [State2#state.buffer, prit_util:to_printable_binary(State2#state.buffer)]),

    State3 = process_input(State2),

    State4 = case {State3#state.step,
		   State3#state.choked} of
		 {run, true} ->
		     send_message(Sock, <<?UNCHOKE>>),
		     State3#state{choked = false};
		 _ ->
		     State3
	     end,
%	logger:log(wire, debug,"Queue length of ~p~n", [length(State4#state.queue)]),	
	
    Queue =
	lists:filter(fun(Queued) ->
			     case (catch send_queued(Queued, State4)) of
				 {'EXIT', Reason} ->
				     logger:log(wire, warn,
						"Cannot send ~p: ~p",
						[Queued, Reason]),
				     %% Keep:
				     true;
				 _ ->
				     %% Ok, remove from queue
				     false
			     end
		     end, State4#state.queue),
    {noreply, State4#state{queue = Queue}};
handle_info({tcp_closed, Sock}, #state{sock = Sock} = State) ->
%	logger:log(wire, info,"Socket closed~n", []),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{info_hash = InfoHash, sock = Sock}) ->
    peerdb:peer_died(InfoHash),
    gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Server mode
%%%===================================================================

%% Waiting for handshake
process_input(#state{mode = server,
		     step = handshake,
		     buffer = << ?BITTORRENT_PROTOCOL, ExtensionBytes:8/binary , InfoHash:20/binary, PeerId:20/binary, Rest/binary>>,
		     sock = Sock} = State) ->
	% TODO: check if we serve that infohash, and only if we do send out the handshake - otherwise terminate
    {ok, MyPeerId} = torrentdb:peer_id(),
    if 
	    PeerId =:= MyPeerId -> exit(normal); % die - don't connect to self
	    
	    true ->
		    %register with peerdb 
			{ok, {IP, Port}} = inet:peername(Sock),
			peerdb:register_peer(InfoHash,PeerId,IP,Port),
			NewState = State#state{step = run,
						  buffer = Rest,
						  info_hash = InfoHash,
						  peer_supports_lte = extensions_supports_lte(ExtensionBytes)}	,	
			send_full_handshake(NewState),
			send_lte_handshake(NewState),
			send_bitfield(NewState),
%			logger:log(wire, debug,"Completed server-side handshake on socket ~p with ~s:~b~n", [Sock,inet_parse:ntoa(IP),Port]),
			process_input(NewState)
	end;

%% received header length, but not in correct format
process_input(#state{mode = server, step = handshake, buffer = <<Ignore:49/binary, _/binary>>} = _State) ->
	io:format("unknown handshake in server state: ~w ~s~n",[Ignore, prit_util:to_printable_binary(Ignore)]),
	exit(malformed_handshake);

%% wait for more enough bytes to parse header
process_input(#state{mode = server, step = handshake} = State) ->
    State;

%%%===================================================================
%%% Client mode
%%%===================================================================

%% Waiting for handshake

process_input(#state{mode = client,
		     step = handshake,
		     info_hash = InfoHash,
		     buffer = << ?BITTORRENT_PROTOCOL, ExtensionBytes:8/binary , InfoHash:20/binary, PeerId:20/binary, Rest/binary>>,
		     sock = Sock} = State) ->
    {ok, MyPeerId} = torrentdb:peer_id(),
    if 
	    PeerId =:= MyPeerId -> exit(normal); % die - don't connect to self
	    
	    true ->
		    %register with peerdb 
			{ok, {IP, Port}} = inet:peername(Sock),
			peerdb:register_peer(InfoHash,PeerId,IP,Port),
			NewState = State#state{step = run, buffer = Rest, peer_supports_lte = extensions_supports_lte(ExtensionBytes) },	
			send_lte_handshake(NewState),
			send_bitfield(NewState),
%			logger:log(wire, debug,"Completed client-side handshake on socket ~p with ~s:~b~n", [Sock,inet_parse:ntoa(IP),Port]),
			process_input(NewState)
	end;

%% received header length, but not in correct format
process_input(#state{mode = server, step = handshake, buffer = <<Ignore:49/binary, _/binary>>} = _State) ->
	io:format("unknown server handshake in client state: ~w ~s\n",[Ignore, prit_util:to_printable_binary(Ignore)]),
	exit(malformed_handshake);

% wait for more bytes
process_input(#state{mode = client, step = handshake} = State) ->
    State;

%%%===================================================================
%%% Server & client mode
%%%===================================================================

%% Waiting for any message

process_input(#state{step = run,
		     buffer = <<Len:32/big, Message:Len/binary, Rest/binary>>
		    } = State1) ->
    State2 = State1#state{buffer = Rest},
    State3 = process_message(Message, State2),
    process_input(State3);

process_input(#state{step = run} = State) ->
    %% Read not enough or processed all so far
    State.

%% Handling messages

process_message(<<>>, State) ->
    %% Keep-alive
    State;

process_message(<<?INTERESTED>>, State) ->
%	logger:log(wire, debug,"Got interest", []),	
    State#state{interested = true};

process_message(<<?NOT_INTERESTED>>, State) ->
%	logger:log(wire, debug,"No interest", []),	
    State#state{interested = false};

process_message(<<?REQUEST, Piece:32/big,
		  Offset:32/big, Length:32/big>>,
		#state{queue = Queue} = State) ->
%	logger:log(wire, debug,"Received Request", []),	
    State#state{queue = Queue ++ [#queued{piece = Piece,
					  offset = Offset,
					  length = Length}]};

% Puzzled: should this work the other way round? e.g. not() the result? as filter keeps everything where the result is true
process_message(<<?CANCEL, Piece:32/big,
		  Offset:32/big, Length:32/big>>,
		#state{queue = Queue} = State) ->
    State#state{queue = lists:filter(fun(Queued) ->
					     Piece == Queued#queued.piece andalso
						 Offset == Queued#queued.offset andalso
						 Length == Queued#queued.length
				     end, Queue)};

% LTE
% Handshake
process_message(<<?EXTENDED,?LTE_HANDSHAKE_ID, BencDict/binary>>, State) ->
	Dict = benc:parse(BencDict),
	logger:log(wire, debug,"Got LibTorrent Extension Handshake (LTE) ~p~n", [Dict]),
	State;	

% Peer Exchange (ut_pex)
% https://trac.transmissionbt.com/browser/trunk/extras/extended-messaging.txt
% added -> compact ipv4+port string for all peers
% added.f -> one byte flags for the peers above, 1:encrypt, 2:seed, 4:uTP Support, 8: holepunch support, 16:outgoing connection
% dropped -> same format as added
% added6 -> compact ipv6 + port string
% added6.f -> corresponding flags
process_message(<<?EXTENDED,?LTE_UT_PEX_ID, BencDict/binary>>, State) ->
	Dict = benc:parse(BencDict),
	logger:log(wire, debug,"Got PEX hosts (LTE) ~p~n", [Dict]),
	case lists:keysearch(<<"added">>,1,Dict) of
		{value, {<<"added">>,CompactIPv4s}} ->
			{value, {<<"added.f">>,Flags}} = lists:keysearch(<<"added.f">>,1,Dict),
			case non_pex_seeders(CompactIPv4s,Flags) of
				[] -> dont_connect;
				PeerList -> peerdb:add_peers(State#state.info_hash,PeerList)
			end;
		_ -> nothing_new 
	end,
	State;

process_message(_Msg, State) ->
    State.


non_pex_seeders(<<A,B,C,D,Port:16/big,IPs/binary>>,<<_:6,0:1,_:1,Flags/binary>>, Acc) -> 
	non_pex_seeders(IPs, Flags, [{{A,B,C,D},Port} | Acc]);
non_pex_seeders(<<_:6/binary,IPs/binary>>,<<_:8,Flags/binary>>, Acc) -> non_pex_seeders(IPs,Flags,Acc);
non_pex_seeders(_,_, Acc) -> Acc.
non_pex_seeders(IPs,Flags) -> non_pex_seeders(IPs,Flags,[]).

send_message(Sock, Msg) ->
    Len = size(Msg),
    ok = gen_tcp:send(Sock,
		      <<Len:32/big,
			Msg/binary>>).

% extensions_field is 8x8 bytes = 64bit
extensions_field() ->
		<<0:43,1:1,0:20>>. % LTE support
%		<<0:64>>. % noextensions

extensions_supports_lte(<<_:43,LTE_Bit:1,_:20>>) -> 
	case LTE_Bit of 1 -> true; _ -> false end.

% see http://wiki.theory.org/BitTorrentSpecification
% handshake: <pstrlen><pstr><reserved><info_hash><peer_id>
send_full_handshake(#state{sock = Sock, info_hash = InfoHash}) ->
    {ok, MyPeerId} = torrentdb:peer_id(),
	Handshake = << ?BITTORRENT_PROTOCOL, (extensions_field())/binary, InfoHash/binary, MyPeerId/binary>>,
	gen_tcp:send(Sock, Handshake).

% see http://www.rasterbar.com/products/libtorrent/extension_protocol.html
send_lte_handshake(#state{sock = Sock, peer_supports_lte = true}) ->
	{ok, Port} = wire_listener:get_port(),
	Handshake = benc:to_binary([{<<"e">>,0}, % prefer encryption (0|1)
								{<<"m">>,[{<< ?LTE_UT_PEX_NAME >>, ?LTE_UT_PEX_ID }]},
								{<<"p">>,Port},
								{<<"v">>,<<"PritTorrent (PodHoster) 0.01">>}]),
	logger:log(wire, debug,"Sending LTE Handshake (LTE) ~p~n", [benc:parse(Handshake)]),
	gen_tcp:send(Sock, << (size(Handshake)+2):32/big, ?EXTENDED, ?LTE_HANDSHAKE_ID, Handshake/binary>>);
send_lte_handshake(#state{peer_supports_lte = false}) -> no_peer_support.

send_bitfield(#state{sock = Sock,
		     info_hash = InfoHash}) ->
    PieceCount = piecesdb:piece_count(InfoHash),
    Msg = list_to_binary(
	    [?BITFIELD |
	     build_bitfield(PieceCount)]),
    send_message(Sock, Msg).

build_bitfield(0) ->
    %% case for byte alignedness
    [];
build_bitfield(N) when N >= 8 ->
    [16#FF | build_bitfield(N - 8)];
build_bitfield(N) when N < 8 ->
    [lists:foldl(fun(I, R) ->
			 (R bsl 1) bor
			     (if N >= I -> 1;
				 true -> 0
			      end)
		 end, 0, lists:seq(1, 8))].

send_queued(#queued{piece = Piece,
		    offset = Offset,
		    length = Length},
	    #state{sock = Sock,
		   info_hash = InfoHash} = State) ->
    FileRanges = piecesdb:map_files(InfoHash, Piece, Offset, Length),
    MessageLength = 1 + 4 + 4 + Length,
    %logger:log(wire, info, "~s Sending out piece ~p offset ~p length ~p~n", [prit_util:info_hash_representation(InfoHash),Piece, Offset, Length]),
    gen_tcp:send(Sock, <<MessageLength:32/big, ?PIECE,
			 Piece:32/big, Offset:32/big>>),
    send_piece(FileRanges, State).

send_piece(FileRanges, #state{sock = Sock,
			      info_hash = InfoHash}) ->
    lists:foreach(
      fun({Path, Offset, Length}) ->
	      %logger:log(wire, debug, "Sending ~B bytes of ~p to socket ~p~n", [Length, Path, Sock]),
	      backend:fold_file(Path, Offset, Length,
				fun(Data, _) ->
					%logger:log(wire, debug, "actually seding ~B bytes~n", [size(Data)]),
					gen_tcp:send(Sock, Data),
					torrentdb:inc_uploaded(InfoHash, size(Data))
				end, nil)
      end, FileRanges).
