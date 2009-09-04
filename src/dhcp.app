{application, dhcp,
 [{description, "DHCP Tools"},
  {vsn, "1.0"},
  {modules, [dhcp, dhcp_server, dhcp_spy]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {start_phases, []}
]}.
