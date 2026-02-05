-module('v.toml.ast').
-export(['Key.str'/1, 'Value.str'/1, 'DateTimeType.str'/1, 'Comment.str'/1, 'Null.str'/1, 'Quoted.str'/1, 'Bare.str'/1, 'Bool.str'/1, 'Number.str'/1, 'Number.i64'/1, 'Number.f64'/1, 'Date.str'/1, 'Time.str'/1, 'DateTime.str'/1, 'EOF.str'/1]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

'Key.str'(K) ->
    maps:get(text, K).

'Value.str'(V) ->
    case V of
        todo; todo; todo; todo -> <<"\"", (maps:get(text, V))/binary, "\"">>;
        todo; todo; todo -> maps:get(text, V);
        todo -> begin
            Str = <<"{">>,
            lists:foreach(fun(Val) ->
                Str1 = <<" \"", (Key)/binary, "\": ", (Val)/binary, ",">>,
                ok
            end, V),
            Str2 = 'string.trim_right'(Str1, <<",">>),
            Str3 = <<" }">>,
            Str3
        end;
        todo -> begin
            Str4 = <<"[">>,
            lists:foreach(fun(Val) ->
                Str5 = <<" ", (Val)/binary, ",">>,
                ok
            end, V),
            Str6 = 'string.trim_right'(Str5, <<",">>),
            Str7 = <<" ]">>,
            Str7
        end
    end.

'DateTimeType.str'(Dtt) ->
    maps:get(text, Dtt).

'Comment.str'(C) ->
    S = maps:get(name, todo) + <<"{\\n">>,
    S1 = <<"  text:  \\'", (maps:get(text, C))/binary, "\\'\\n">>,
    S2 = <<"  pos:  ", (maps:get(pos, C))/binary, "\\n">>,
    S3 = <<"}">>,
    S3.

'Null.str'(N) ->
    maps:get(text, N).

'Quoted.str'(Q) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, Q))/binary, "\\'\\n">>,
    Str2 = <<"  pos:  ", (maps:get(pos, Q))/binary, "\\n">>,
    Str3 = <<"  is_multiline:  ", (atom_to_binary(maps:get(is_multiline, Q)))/binary, "\\n">>,
    Str4 = <<"  quote: \\'", (integer_to_binary(maps:get(quote, Q)))/binary, "\\'\\n">>,
    Str5 = <<"}">>,
    Str5.

'Bare.str'(B) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, B))/binary, "\\'\\n">>,
    Str2 = <<"  pos:  ", (maps:get(pos, B))/binary, "\\n">>,
    Str3 = <<"}">>,
    Str3.

'Bool.str'(B) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, B))/binary, "\\'\\n">>,
    Str2 = <<"  pos:  ", (maps:get(pos, B))/binary, "\\n">>,
    Str3 = <<"}">>,
    Str3.

'Number.str'(N) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, N))/binary, "\\'\\n">>,
    Str2 = <<"  pos:  ", (maps:get(pos, N))/binary, "\\n">>,
    Str3 = <<"}">>,
    Str3.

'Number.i64'(N) ->
    case 'string.starts_with'(maps:get(text, N), <<"0x">>) of
        true -> begin
            Hex = 'string.replace'('string.to_upper'('string.all_after'(maps:get(text, N), <<"0x">>)), <<"_">>, <<"">>),
            parse_int(Hex, 16, 64)
        end;
        false -> case 'string.starts_with'(maps:get(text, N), <<"0o">>) of
            true -> begin
                Oct = 'string.replace'('string.all_after'(maps:get(text, N), <<"0o">>), <<"_">>, <<"">>),
                parse_int(Oct, 8, 64)
            end;
            false -> case 'string.starts_with'(maps:get(text, N), <<"0b">>) of
                true -> begin
                    Bin = 'string.replace'('string.all_after'(maps:get(text, N), <<"0b">>), <<"_">>, <<"">>),
                    parse_int(Bin, 2, 64)
                end;
                false -> ok
            end
        end
    end,
    parse_int(maps:get(text, N), 0, 64).

'Number.f64'(N) ->
    'string.f64'('string.replace'(maps:get(text, N), <<"_">>, <<"">>)).

'Date.str'(D) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, D))/binary, "\\'\\n">>,
    Str2 = <<"  pos:  ", (maps:get(pos, D))/binary, "\\n">>,
    Str3 = <<"}">>,
    Str3.

'Time.str'(T) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, T))/binary, "\\'\\n">>,
    Str2 = <<"  offset:  \\'", (integer_to_binary(maps:get(offset, T)))/binary, "\\'\\n">>,
    Str3 = <<"  pos:  ", (maps:get(pos, T))/binary, "\\n">>,
    Str4 = <<"}">>,
    Str4.

'DateTime.str'(Dt) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  text:  \\'", (maps:get(text, Dt))/binary, "\\'\\n">>,
    Str2 = <<"  date:  \\'", (maps:get(date, Dt))/binary, "\\'\\n">>,
    Str3 = <<"  time:  \\'", (maps:get(time, Dt))/binary, "\\'\\n">>,
    Str4 = <<"  pos:  ", (maps:get(pos, Dt))/binary, "\\n">>,
    Str5 = <<"}">>,
    Str5.

'EOF.str'(E) ->
    Str = maps:get(name, todo) + <<"{\\n">>,
    Str1 = <<"  pos:  ", (maps:get(pos, E))/binary, "\\n">>,
    Str2 = <<"}">>,
    Str2.
