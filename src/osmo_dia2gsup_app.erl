-module(osmo_dia2gsup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	osmo_dia2gsup_sup:start_link().

stop(_State) ->
	ok.
