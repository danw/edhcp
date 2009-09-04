
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