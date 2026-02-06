-module('v.main').
-export([main/0, greet_func/1, greet_pre_func/1, greet_post_func/1]).

main() ->
    Cmd = #{name => <<"cli">>, description => <<"An example of the cli library.">>, version => <<"1.0.0">>, {vbeam, type} => 'Command'},
    Greet_cmd = #{name => <<"greet">>, description => <<"Prints greeting in different languages.">>, usage => <<"<name>">>, required_args => 1, pre_execute => Main.greet_pre_func, execute => Main.greet_func, post_execute => Main.greet_post_func, {vbeam, type} => 'Command'},
    'Command.add_flag'(Greet_cmd, #{flag => string, required => true, name => <<"language">>, abbrev => <<"l">>, description => <<"Language of the message.">>, {vbeam, type} => 'Flag'}),
    'Command.add_flag'(Greet_cmd, #{flag => int, name => <<"times">>, default_value => [<<"3">>], description => <<"Number of times the message gets printed.">>, {vbeam, type} => 'Flag'}),
    'Command.add_flag'(Greet_cmd, #{flag => string_array, name => <<"fun">>, description => <<"Just a dumby flags to show multiple.">>, {vbeam, type} => 'Flag'}),
    'Command.add_command'(Cmd, Greet_cmd),
    'Command.setup'(Cmd),
    'Command.parse'(Cmd, init:get_plain_arguments()),
    ok.

greet_func(Cmd) ->
    Language = 'Flag.get_string'(maps:get(flags, Cmd), <<"language">>),
    Times = 'Flag.get_int'(maps:get(flags, Cmd), <<"times">>),
    Name = lists:nth(1, maps:get(args, Cmd)),
    lists:foreach(fun(_) ->
        case Language of
            <<"english">>; <<"en">> -> vbeam_io:println(<<"Welcome ", (Name)/binary>>);
            <<"german">>; <<"de">> -> vbeam_io:println(<<"Willkommen ", (Name)/binary>>);
            <<"dutch">>; <<"nl">> -> vbeam_io:println(<<"Welkom ", (Name)/binary>>);
            _ -> begin
                io:format("~s~n", [<<"Unsupported language">>]),
                io:format("~s~n", [<<"Supported languages are `english`, `german` and `dutch`.">>]),
                % TODO: unhandled stmt type
            end
        end,
        ok
    end, lists:seq(0, Times - 1)),
    Fun = 'Flag.get_strings'(maps:get(flags, Cmd), <<"fun">>),
    lists:foreach(fun(F) ->
        vbeam_io:println(<<"fun: ", (F)/binary>>),
        ok
    end, Fun),
    ok.

greet_pre_func(_cmd) ->
    io:format("~s~n", [<<"This is a function running before the main function.\\n">>]),
    ok.

greet_post_func(_cmd) ->
    io:format("~s~n", [<<"\\nThis is a function running after the main function.">>]),
    ok.
