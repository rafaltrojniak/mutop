{application, mutop,
 [{description, "Top-like application using munin nodes as sensors"},
  {vsn, "0.0.0"},
  {modules, [munin_con, munin_client, munin_client_pool, munin_manager, munin_client_proxy]},
  {env, []},
  {mod, {mutop, []}}]}.

