-module(test_plugin).
-compile(export_all).

%% hooking into `clean' proves that we're not dependant on any other steps
clean(Config, _) ->
    rebar_log:log(debug, "~p:~p~n", [test_plugin, clean]),
    file:delete("fwibble.test").

