%% This is the application resource file (.app file) for the web_layout,
%% application.
{application, web_layout,
  [{description, "Layout application to be used with web_router"},
   {vsn, "0.1.0"},
   {modules, [web_layout_app,
              web_layout_sup,
              web_layout]},
   {registered, [web_layout_sup]},
   {applications, [kernel, stdlib]},
   {mod, {web_layout_app, []}},
   {start_phases, []}]}.

