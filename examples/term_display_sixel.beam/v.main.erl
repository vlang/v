-module('v.main').
-export([main/0]).

main() ->
    io:format("~s~n", [<<"If your terminal supports the sixel graphics format,">>]),
    io:format("~s~n", [<<"you should see a small green \"HI\" and a V logo below this text.\\n">>]),
    case supports_sixel() of
        true -> begin
            io:format("~s~n", [<<"\\ePq\n#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0\n#1~~@@vv@@~~@@~~$\n#2??}}GG}}??}}??-\n#1!14@\n\\e\\\\">>]),
            Bytes = read_bytes(resource_abs_path(<<"assets/v.six">>)),
            vbeam_io:println('[]u8.bytestr'(Bytes)),
            todo,
            todo
        end;
        false -> ok
    end.
