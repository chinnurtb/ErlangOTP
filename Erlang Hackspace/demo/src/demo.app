{application, demo,
 [{description, "demo"},
  {vsn, "0.01"},
  {modules, [
    demo,
    demo_app,
    demo_sup,
    demo_web,
    demo_deps
  ]},
  {registered, []},
  {mod, {demo_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
