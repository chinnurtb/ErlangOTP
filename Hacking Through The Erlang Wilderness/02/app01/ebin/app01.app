%% This is the application resource file (.app file) for the app01,
%% application.
{application, app01,
  [{description, "Your Desc HERE"},
   {vsn, "0.0.1"},
   {modules, [  app01,
                app01_app,
                app01_sup]},
   {registered,[app01_sup]},
   {applications, [kernel, stdlib]},
   {mod, {app01_app,[]}},
   {start_phases, []}]}.

