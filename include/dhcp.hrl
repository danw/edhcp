%%%-------------------------------------------------------------------
%%% File    : dhcp.hrl
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

-record (dhcp_packet, {
  msg_type,
  requested_ip = {0, 0, 0, 0},
  op = 0,
  htype = 0,
  hlen = 0,
  hops = 0,
  xid = 0,
  secs = 0,
  flags = 0,
  ciaddr = {0, 0, 0, 0},
  yiaddr = {0, 0, 0, 0},
  siaddr = {0, 0, 0, 0},
  giaddr = {0, 0, 0, 0},
  chaddr = {0, 0, 0, 0, 0, 0},
  sname = 0,
  file = 0,
  options = []}).

-record (dhcp_lease, {
  ip_addr = {0, 0, 0, 0},
  chaddr = {0, 0, 0, 0, 0, 0},
  options = []}).