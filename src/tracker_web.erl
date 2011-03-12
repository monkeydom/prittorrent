%% @author {{author}}
%% @copyright {{year}} {{author}}

%% @doc Web server for tracker.

-module(tracker_web).
-author("The PritTorrent Committee").

-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, loop}} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

handle_announcement(Req) ->
    ParsedQuery = Req:parse_qs(),
    io:format("query: ~p\n",[ParsedQuery]),
    case lists:keyfind("compact",1,ParsedQuery) of 
    	{"compact","1"} ->
    		Req:ok({"text/plain",[],[benc:to_binary([{<<"peers">>, <<192,168,178,47,6881:16/big>> }])]});
    	_ -> 
    		Req:ok({"text/plain",benc:to_binary([{
    			<<"peers">>, [ [% {<<"peer id">>,<<"idon'tcareatall">>},
    			                {<<"ip">>,<<"127.0.0.1">>},
    			                {<<"port">>,6881}] ] }])})
    end.


loop(Req) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "announce" ++ _ ->
                        handle_announcement(Req);
                    _ ->
                      	Req:ok({"text/plain", ["Hello, world!\n", io_lib:format("~p - ~p - ~p",[Req:get(peer),Req:parse_qs(),Path])]})
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \\ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
