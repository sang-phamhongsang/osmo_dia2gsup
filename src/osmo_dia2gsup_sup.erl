-module(osmo_dia2gsup_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
        DiaServer = {osmo_dia2gsup,{osmo_dia2gsup,start_link,[]},
                     permanent,
                     5000,
                     worker,
                     [server_cb]},
        {ok, { {one_for_one, 5, 10}, [DiaServer]} }.
