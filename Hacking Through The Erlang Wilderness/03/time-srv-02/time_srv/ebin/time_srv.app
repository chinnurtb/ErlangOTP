%% This is the application resource file (.app file) for the time_srv,
%% application.
{application, time_srv,
  [{description, "Your Desc HERE"},
   {vsn, "0.0.1"},
   {modules, [  time_srv,
                time_srv_app,
                time_srv_sup]},
   {registered,[time_srv_sup]},
   {applications, [kernel, stdlib]},
   {mod, {time_srv_app,[]}},
   {start_phases, []}]}.

