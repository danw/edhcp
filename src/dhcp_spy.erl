%%%-------------------------------------------------------------------
%%% File    : dhcp_spy.erl
%%% Author  : Dan Willemsen <dan@csh.rit.edu>
%%% Purpose : 
%%%
%%% edhcp, Copyright (C) 2009 Dan Willemsen
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module (dhcp_spy).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1]).
-export ([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("dhcp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%
% Gen_Server Callbacks %
%%%%%%%%%%%%%%%%%%%%%%%%
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init ([]) ->
  {ok, ServerSocket} = gen_udp:open(67, [binary, inet, {reuseaddr, true}]),
  {ok, ClientSocket} = gen_udp:open(68, [binary, inet, {reuseaddr, true}]),
  {ok, {ServerSocket, ClientSocket}}.
 
handle_call (_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.
 
handle_cast (_Request, State) ->
  {noreply, State}.

handle_info ({udp, ServerSocket, _IP, 68, Packet}, State = {ServerSocket, _}) ->
  {ok, DecodedPacket} = dhcp:decode_packet(Packet),
  io:format("From Server Socket: "),
  dhcp:print_packet(DecodedPacket),
  {noreply, State}; 
handle_info ({udp, ClientSocket, _IP, 67, Packet}, State = {_, ClientSocket}) ->
  {ok, DecodedPacket} = dhcp:decode_packet(Packet),
  io:format("From Client Socket: "),
  dhcp:print_packet(DecodedPacket),
  {noreply, State};
handle_info (_Info, State) ->
  {noreply, State}.
 
terminate (_Reason, _State) ->
  ok.
 
code_change (_OldVsn, State, _Extra) ->
  io:format("Got codechange!~n"),
  {ok, State}.
