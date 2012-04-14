{application, mutop,
 [{description, "Top-like application using munin nodes as sensors"},
  {vsn, "0.0.0"},
  {modules, [munin_con, munin_client]},
  {env, []},
  {mod, {mutop, []}}]}.

