-module('v.main').
-export([main/0]).

main() ->
    set_level(debug),
    debug(<<"simple debug message">>),
    warn(<<"simple warning message">>),
    info(<<"simple information message">>),
    error(<<"simple error message">>),
    L = #{{vbeam, type} => 'Log'},
    'Log.set_level'(L, info),
    'Log.set_full_logpath'(L, <<"./info.log">>),
    'Log.log_to_console_too'(L),
    vbeam_io:println(<<"Please check the file: ", (maps:get(output_file_name, L))/binary, " after this example crashes.">>),
    'Log.info'(L, <<"info">>),
    'Log.warn'(L, <<"warn">>),
    'Log.error'(L, <<"error">>),
    'Log.debug'(L, <<"no output for debug">>),
    'Log.set_level'(L, debug),
    'Log.debug'(L, <<"debug now">>),
    'Log.set_level'(L, level_from_tag(<<"INFO">>)),
    'Log.info'(L, <<"info again">>),
    'Log.set_level'(L, level_from_tag(<<"">>)),
    'Log.error'(L, <<"no output anymore">>),
    'Log.fatal'(L, <<"fatal">>),
    ok.
