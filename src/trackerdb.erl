-module(trackerdb).

-export([init/0, announce/7, remove/3, unix_seconds_since_epoch/0, remove_peers_with_timeout_in_seconds/1, json_statistics/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(pirate, { id, info_hash, ip, port, peer_id, 
					uploaded, downloaded, left,
					first_seen, last_seen}).

-record(swarm_status, { info_hash, complete=0, incomplete=0}).

% -record(struct, { entries = [] }). 

init() ->
    mnesia:create_table(pirate,
			[{attributes, record_info(fields, pirate)}, {type, set}, {index, [info_hash]}]).

updated_statistics(#pirate{info_hash = InfoHash, left = Left}, [], Result) ->
	NewStatus = case Left of 
		0 -> #swarm_status{info_hash=InfoHash,   complete=1};
		_ -> #swarm_status{info_hash=InfoHash, incomplete=1}
	end,
	[NewStatus | Result];
updated_statistics(#pirate{info_hash = InfoHash, left = Left}, [#swarm_status{info_hash = InfoHash} = Status | Tail], Result) ->
	UpdatedStatus = case Left of
		0 -> Status#swarm_status{info_hash=InfoHash,   complete = Status#swarm_status.complete + 1};
		_ -> Status#swarm_status{info_hash=InfoHash, incomplete = Status#swarm_status.incomplete + 1}
	end,
	[UpdatedStatus | (Tail ++ Result)];
updated_statistics(Pirate, [Status | Tail], Result) ->
	updated_statistics(Pirate, Tail, [Status | Result]).
	
	

updated_statistics(Pirate, List) ->
	updated_statistics(Pirate, List, []).
	

statistics() ->
	F = fun() ->
		mnesia:foldl( fun(Pirate, Acc) ->
				updated_statistics(Pirate, Acc)
			end,
			[],
			pirate)		
	end,
	graceful_transaction(F).

json_statistics() ->
	{struct, lists:map(fun(#swarm_status{info_hash = InfoHash, complete = Complete, incomplete = Incomplete}) ->
			HashBinary = prit_util:to_hex(InfoHash),
			{HashBinary, {struct,[{complete,Complete},{incomplete,Incomplete},{name,torrentdb:torrent_name(InfoHash)}]}}
		end, statistics())}.


% using astro's _t to mean needs to be inside of an transaction
swarm_status_t(InfoHash) ->
		{ Complete, Incomplete } = qlc:fold(
			fun(#pirate{left = 0}, { CompleteAcc, IncompleteAcc} ) ->
				{ CompleteAcc + 1, IncompleteAcc };
			(_, { CompleteAcc, IncompleteAcc} ) ->
				{ CompleteAcc , IncompleteAcc + 1 }
			end, 
			{0, 0}, 
			qlc:q([Pirate || Pirate = #pirate{}  <- mnesia:table(pirate), Pirate#pirate.info_hash =:= InfoHash])
		),
		{ok, #swarm_status{info_hash = InfoHash, complete = Complete, incomplete = Incomplete}}.
	

announce(InfoHash, Ip, Port, PeerId, Uploaded, Downloaded, Left) ->
	PrimaryPeerKey = { InfoHash, Ip, Port },
	{atomic, Result} = mnesia:transaction(fun() -> 
		AllPeers = case Left of 
			0 -> % we are seeder
				qlc:e(qlc:q([Pirate || Pirate <- mnesia:table(pirate), Pirate#pirate.left =/= 0, Pirate#pirate.info_hash =:= InfoHash]));
			_ -> % we are leecher
				mnesia:index_read(pirate, InfoHash, #pirate.info_hash)
		end,
		Now = unix_seconds_since_epoch(),
		PeerUpdate = case mnesia:read(pirate, PrimaryPeerKey) of
			[Peer = #pirate{ }] -> Peer#pirate { peer_id = PeerId, uploaded = Uploaded,
												 	downloaded = Downloaded, left = Left, last_seen = Now };
			[] -> #pirate{ id = PrimaryPeerKey,
							info_hash = InfoHash, ip = Ip, port = Port, peer_id = PeerId,
							uploaded = Uploaded, downloaded = Downloaded, left = Left,
							first_seen = Now, last_seen = Now }
		end,
		mnesia:write(PeerUpdate),

		{ok, #swarm_status{complete = Complete, incomplete = Incomplete}} = swarm_status_t(InfoHash),

		AvailablePeers = [ { TmpPeerId, TmpIp, TmpPort } ||
							Peer = #pirate{ peer_id = TmpPeerId, ip = TmpIp, port = TmpPort } <- AllPeers, Peer#pirate.id =/= PrimaryPeerKey],
		{ ok, AvailablePeers, Complete, Incomplete }
		end),
	Result.

remove(InfoHash, Ip, Port) ->
	{atomic, Result} = mnesia:transaction(
		fun() -> 
			mnesia:delete({pirate, { InfoHash, Ip, Port } })
		end),
	Result.
	
remove_peers_with_timeout_in_seconds(Seconds) ->
	KillTime = unix_seconds_since_epoch() - Seconds, % all pirates iwth an update date of this and below need to go away
	Q = qlc:q([Pirate || Pirate <- mnesia:table(pirate), Pirate#pirate.last_seen < KillTime]),
	F = fun () ->
		PiratesToKill = qlc:e(Q),
		lists:foreach( fun(Pirate) -> mnesia:delete_object(Pirate) end, PiratesToKill) 
%		io:format("Killing Pirates: ~p \n",[PiratesToKill]) % should not do this in a transaction…
	end,
	graceful_transaction(F).

graceful_transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, Reason} ->
			io:format("transaction abort: ~p~n",[Reason]),
			[]
	end.	


% find(Q) ->
% 	F = fun() ->
% 			qlc:e(Q)
% 	end,
% 	graceful_transaction(F).
% 
% read_all(Table) ->
% 	Q = qlc:q([X || X <- mnesia:table(Table)]),
% 	graceful_transaction(Q). 


unix_seconds_since_epoch() ->
    LocalDateTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    LocalDateTime - UnixEpoch.


% make_timestamp(now) ->
% 	{ MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
% 	(MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.