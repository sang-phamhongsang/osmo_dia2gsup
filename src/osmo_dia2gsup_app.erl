-module(osmo_dia2gsup_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	case somo_dia2gsup_sup:start_link() of
		{ok, _} = Net ->
			osmo_dia2gsup_sup:start_childs(SrvSupSpecs),
			ret;
		Other ->
			Other
	end.

stop(_State) ->
	ok.

