-module(server_cb).


-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("diameter_3gpp_ts29_272.hrl").
%-include_lib("diameter_settings.hrl").


%% diameter callbacks
-export([peer_up/3, peer_down/3, pick_peer/4, prepare_request/3, prepare_retransmit/3,
	 handle_answer/4, handle_error/4, handle_request/3]).

-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

peer_up(_SvcName, {PeerRef, Caps}, State) ->
	lager:info("Peer up ~p - ~p~n", [PeerRef, lager:pr(Caps, ?MODULE)]),
	State.

peer_down(_SvcName, {PeerRef, Caps}, State) ->
	lager:info("Peer down ~p - ~p~n", [PeerRef, lager:pr(Caps, ?MODULE)]),
	State.

pick_peer(_, _, _SvcName, _State) ->
	?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
	?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
	?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
	?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
	lager:error("Request error: ~p~n", [_Reason]),
	?UNEXPECTED.

handle_request(#diameter_packet{}, _SvcName, {_,_}) ->
	lager:error("Unsuppoerted message.~n"),
	discard.
