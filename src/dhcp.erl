-module (dhcp).

-include ("dhcp.hrl").
-export ([decode_packet/1, encode_packet/1, print_packet/1]).

decode_packet(<<
    Op:8,
    HType:8,
    HLen:8,
    Hops:8,
    Xid:32,
    Secs:16,
    Flags:16/big,
    CIAddr:4/binary,
    YIAddr:4/binary,
    SIAddr:4/binary,
    GIAddr:4/binary,
    CHAddr:16/binary,
    SName:512,
    File:1024,
    99:8, 130:8, 83:8, 99:8, % Magic Number
    Options/binary>>) ->
  DecodedOptions = decode_options(Options),
  {value, {message_type, MessageType}} = lists:keysearch(message_type, 1, DecodedOptions),
  RequestedIp = case lists:keysearch(requested_ip_address, 1, DecodedOptions) of
                  {value, {requested_ip_address, ReqIp}} -> ReqIp;
                  _ -> {0, 0, 0, 0}
                end,
  Packet = #dhcp_packet{
    msg_type = MessageType,
    requested_ip = RequestedIp,
    op = Op,
    htype = HType,
    hlen = HLen,
    hops = Hops,
    xid = Xid,
    secs = Secs,
    flags = Flags,
    ciaddr = ip_address(ip_address, CIAddr),
    yiaddr = ip_address(ip_address, YIAddr),
    siaddr = ip_address(ip_address, SIAddr),
    giaddr = ip_address(ip_address, GIAddr),
    chaddr = hw_address(tuple, CHAddr),
    sname = SName,
    file = File,
    options = DecodedOptions},
  {ok, Packet};
decode_packet(Unknown) ->
  io:format("Unknown DHCP Packet: ~p~n", [Unknown]),
  {error, unknown_packet}.

% Padding
decode_options(<<0:8, Rest/binary>>) ->
  decode_options(Rest);
% End marker - ignore rest, though rest should be zero
decode_options(<<255:8, _/binary>>) ->
  [];
% Default
decode_options(<<Op:8, Len:8, Value:Len/binary, Rest/binary>>) ->
  Name = opt_name(Op),
  [{Name, dec_opt_val(Name, Value)}] ++ decode_options(Rest).

encode_packet(Packet) when is_record(Packet, dhcp_packet) ->
  EncodedOptions = encode_options(Packet#dhcp_packet.options),
  EncodedPacket = <<(Packet#dhcp_packet.op):8,
                    (Packet#dhcp_packet.htype):8,
                    (Packet#dhcp_packet.hlen):8,
                    (Packet#dhcp_packet.hops):8,
                    (Packet#dhcp_packet.xid):32,
                    (Packet#dhcp_packet.secs):16,
                    (Packet#dhcp_packet.flags):16/big,
                    (ip_address(binary, Packet#dhcp_packet.ciaddr))/binary,
                    (ip_address(binary, Packet#dhcp_packet.yiaddr))/binary,
                    (ip_address(binary, Packet#dhcp_packet.siaddr))/binary,
                    (ip_address(binary, Packet#dhcp_packet.giaddr))/binary,
                    (hw_address(binary, Packet#dhcp_packet.chaddr))/binary,
                    (Packet#dhcp_packet.sname):512,
                    (Packet#dhcp_packet.file):1024,
                    <<99:8, 130:8, 83:8, 99:8>>/binary,
                    EncodedOptions/binary>>,
  {ok, EncodedPacket}.

encode_options([{Name, Val} | Rest]) ->
  OptName = opt_name(Name),
  OptVal = enc_opt_val(Name, Val),
  <<OptName:8, (size(OptVal)):8, OptVal/binary, (encode_options(Rest))/binary>>;
encode_options([]) ->
  <<255:8>>.

print_packet(P) ->
  io:format("Packet:~n  Op: ~p~n  HType: ~p~n  HLen: ~p~n  Hops: ~p~n  Xid: ~p~n  Secs: ~p~n  Flags: ~p~n  CIAddr: ~p~n  YIAddr: ~p~n  SIAddr: ~p~n  GIAddr: ~p~n  CHAddr: ~p~n  SName: ~p~n  File: ~p~n  Options: ~p~n~n", 
   [P#dhcp_packet.op,
    P#dhcp_packet.htype,
    P#dhcp_packet.hlen,
    P#dhcp_packet.hops,
    P#dhcp_packet.xid,
    P#dhcp_packet.secs,
    P#dhcp_packet.flags,
    P#dhcp_packet.ciaddr,
    P#dhcp_packet.yiaddr,
    P#dhcp_packet.siaddr,
    P#dhcp_packet.giaddr,
    P#dhcp_packet.chaddr,
    P#dhcp_packet.sname,
    P#dhcp_packet.file,
    P#dhcp_packet.options]).

ip_address(tuple, B) ->
  ip_address(ip_address, B);
ip_address(ip_address, <<A:8, B:8, C:8, D:8>>) ->
  {A, B, C, D};
ip_address(binary, {A,B,C,D}) ->
  <<A:8, B:8, C:8, D:8>>.

hw_address(tuple, <<A:8, B:8, C:8, D:8, E:8, F:8, _/binary>>) ->
  {A, B, C, D, E, F};
hw_address(binary, {A, B, C, D, E, F}) ->
  <<A:8, B:8, C:8, D:8, E:8, F:8, 0:80>>.

opt_name(1) ->  subnet_mask;
opt_name(2) ->  time_offset;
opt_name(3) ->  router;
opt_name(6) ->  dns_server;
opt_name(12) -> host_name;
opt_name(15) -> domain_name;
opt_name(28) -> broadcast_address;
opt_name(50) -> requested_ip_address;
opt_name(51) -> lease_time;
opt_name(53) -> message_type;
opt_name(54) -> server_id;
opt_name(55) -> parameter_request;
opt_name(57) -> max_message_size;
opt_name(58) -> renewal_time;
opt_name(59) -> rebinding_time;
opt_name(61) -> client_id;
opt_name(subnet_mask)          -> 1;
opt_name(time_offset)          -> 2;
opt_name(router)               -> 3;
opt_name(dns_server)           -> 6;
opt_name(host_name)            -> 12;
opt_name(domain_name)          -> 15;
opt_name(broadcast_address)    -> 28;
opt_name(requested_ip_address) -> 50;
opt_name(lease_time)           -> 51;
opt_name(message_type)         -> 53;
opt_name(server_id)            -> 54;
opt_name(parameter_request)    -> 55;
opt_name(max_message_size)     -> 57;
opt_name(renewal_time)         -> 58;
opt_name(rebinding_time)       -> 59;
opt_name(client_id)            -> 61;
opt_name(Op) ->
  io:format("Unknown DHCP Option: ~p~n", [Op]),
  Op.

opt_type(subnet_mask)          -> ip_address;
opt_type(router)               -> ip_address;
opt_type(dns_server)           -> ip_address;
opt_type(broadcast_address)    -> ip_address;
opt_type(requested_ip_address) -> ip_address;
opt_type(server_id)            -> ip_address;

opt_type(time_offset)    -> quad_int;
opt_type(lease_time)     -> quad_int;
opt_type(renewal_time)   -> quad_int;
opt_type(rebinding_time) -> quad_int;

opt_type(max_message_size) -> word_int;

opt_type(host_name)   -> string;
opt_type(domain_name) -> string;

opt_type(message_type) -> message_type;

opt_type(_) -> binary.

dec_opt_val(Name, Val) ->
  case opt_type(Name) of
    ip_address ->
      ip_address(ip_address, Val);
    quad_int ->
      <<Quad:32/big>> = Val,
      Quad;
    word_int ->
      <<Word:16/big>> = Val,
      Word;
    string ->
      binary_to_list(Val);
    message_type ->
      <<Type:8>> = Val,
      case Type of
        1 -> discover;
        2 -> offer;
        3 -> request;
        4 -> decline;
        5 -> ack;
        6 -> nak;
        7 -> release;
        8 -> inform
      end;
    binary ->
      Val
  end.

enc_opt_val(Name, Val) ->
  case opt_type(Name) of
    ip_address ->
      ip_address(binary, Val);
    quad_int ->
      <<Val:32/big>>;
    word_int ->
      <<Val:16/big>>;
    string ->
      list_to_binary(Val);
    message_type ->
      case Val of
        discover -> <<1:8>>;
        offer    -> <<2:8>>;
        request  -> <<3:8>>;
        decline  -> <<4:8>>;
        ack      -> <<5:8>>;
        nak      -> <<6:8>>;
        release  -> <<7:8>>;
        inform   -> <<8:8>>
      end;
    binary ->
      Val
  end.
