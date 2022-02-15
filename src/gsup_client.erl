% simple, blocking/synchronous GSUP client

% (C) 2019 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(gsup_client).

-behaviour(gen_server).

-include_lib("osmo_gsup/include/gsup_protocol.hrl").
-include_lib("osmo_ss7/include/ipa.hrl").

-define(IPAC_PROTO_EXT_GSUP,	{osmo, 5}).

-record(gsupc_state, {
	  	socket,
		ipa_pid
	 }).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

%% ------------------------------------------------------------------
%% our exported API
%% ------------------------------------------------------------------

start_link(ServerAddr, ServerPort, Options) ->
	gen_server:start_link(?MODULE, [ServerAddr, ServerPort, Options], [{debug, [trace]}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Address, Port, Options]) ->
	ipa_proto:init(),
	% register the GSUP codec with the IPA core; ignore result as we mgiht be doing this multiple times
	ipa_proto:register_codec(?IPAC_PROTO_EXT_GSUP, fun gsup_protocol:encode/1, fun gsup_protocol:decode/1),
	lager:info("Connecting to GSUP HLR on IP ~s port ~p~n", [Address, Port]),
        CcmOptions = #ipa_ccm_options{
                serial_number="HSS-00-00-00-00-00-00",
                unit_id="0/0/0",
                mac_address="00:00:00:00:00:00",
                location="00:00:00:00:00:00",
                unit_type="00:00:00:00:00:00",
                equipment_version="00:00:00:00:00:00",
                sw_version="00:00:00:00:00:00",
                unit_name="HSS-00-00-00-00-00-00"
                },
	case ipa_proto:connect(Address, Port, Options) of
		{ok, {Socket, IpaPid}} ->
			ipa_proto:set_ccm_options(Socket, CcmOptions),
			lager:info("connected!~n", []),
			true = ipa_proto:register_stream(Socket, ?IPAC_PROTO_EXT_GSUP, {process_id, self()}),
			ipa_proto:unblock(Socket),
			{ok, #gsupc_state{socket=Socket, ipa_pid=IpaPid}};
		{error, econnrefused} ->
			timer:sleep(5000),
			{stop, connrefused}
	end.


% send a given GSUP message and synchronously wait for message type ExpRes or ExpErr
handle_call({transceive_gsup, GsupMsgTx, ExpRes, ExpErr}, _From, State) ->
	Socket = State#gsupc_state.socket,
	{ok, Imsi} = maps:find(imsi, GsupMsgTx),
	ipa_proto:send(Socket, ?IPAC_PROTO_EXT_GSUP, GsupMsgTx),
	% selective receive for only those GSUP responses we expect
	receive
		{ipa, Socket, ?IPAC_PROTO_EXT_GSUP, GsupMsgRx = #{message_type := ExpRes, imsi := Imsi}} ->
			{reply, GsupMsgRx, State};

		{ipa, Socket, ?IPAC_PROTO_EXT_GSUP, GsupMsgRx = #{message_type := ExpErr, imsi := Imsi}} ->
			{reply, GsupMsgRx, State}
	after 5000 ->
		{reply, timeout, State}
	end.

handle_cast(Info, S) ->
	error_logger:error_report(["unknown handle_cast", {module, ?MODULE}, {info, Info}, {state, S}]),
	{noreply, S}.

handle_info({ipa_closed, _}, S) ->
	lager:error("GSUP connection has been closed, supervisor should reconnect us"),
	{stop, ipa_closed, S};
handle_info(Info, S) ->
	error_logger:error_report(["unknown handle_info", {module, ?MODULE}, {info, Info}, {state, S}]),
	{noreply, S}.

terminate(Reason, _S) ->
	lager:info("terminating ~p with reason ~p~n", [?MODULE, Reason]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
