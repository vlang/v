-module('v.builtin').
-export(['ChanState__static__from'/1]).

'ChanState__static__from'(Input) ->
    case Input is todo of
        true -> begin
            Val = todo,
            case Val of
                success -> success;
                not_ready -> not_ready;
                closed -> closed
            end
        end;
        false -> ok
    end,
    case Input is todo of
        true -> begin
            Val1 = 'unknown.str'(Input),
            case Val1 of
                <<"success">> -> success;
                <<"not_ready">> -> not_ready;
                <<"closed">> -> closed;
                _ -> ok
            end
        end;
        false -> ok
    end,
    error(<<"invalid value">>).
