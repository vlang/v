-module('v.toml.parser').
-export(['DottedKey.str'/1, 'DottedKey.starts_with'/2, 'DottedKey.has'/2, new_parser/1, 'Parser.init'/1, 'Parser.run_checker'/1, 'Parser.run_decoder'/1, 'Parser.parse'/1, 'Parser.next'/1, 'Parser.peek'/2, 'Parser.check'/2, 'Parser.peek_for_correct_line_ending_or_fail'/1, 'Parser.check_one_of'/2, 'Parser.ignore_while'/2, 'Parser.ignore_while_peek'/2, 'Parser.peek_over'/3, 'Parser.is_at'/2, 'Parser.expect'/2, 'Parser.build_abs_dotted_key'/2, todo_msvc_astring2dkey/1, 'Parser.check_immutable'/2, 'Parser.check_explicitly_declared'/2, 'Parser.check_explicitly_declared_array_of_tables'/2, 'Parser.check_implicitly_declared'/2, 'Parser.find_table'/1, 'Parser.allocate_table'/2, 'Parser.sub_table_key'/2, 'Parser.find_sub_table'/2, 'Parser.find_in_table'/3, is_all_tables/2, 'Parser.find_array_of_tables'/1, 'Parser.allocate_in_table'/3, 'Parser.dotted_key'/1, 'Parser.root_table'/1, 'Parser.excerpt'/1, 'Parser.table_contents'/2, 'Parser.inline_table'/2, 'Parser.array_of_tables'/2, 'Parser.array_of_tables_contents'/1, 'Parser.double_array_of_tables'/2, 'Parser.double_array_of_tables_contents'/2, 'Parser.array'/1, 'Parser.comment'/1, 'Parser.key'/1, 'Parser.key_value'/1, 'Parser.dotted_key_value'/1, 'Parser.value'/1, 'Parser.number_or_date'/1, 'Parser.bare'/1, 'Parser.quoted'/1, 'Parser.boolean'/1, 'Parser.number'/1, 'Parser.date_time'/1, 'Parser.date'/1, 'Parser.time'/1, 'Parser.undo_special_case_01'/2, 'Parser.eof'/1]).

'DottedKey.str'(Dk) ->
    'DottedKey.join'(Dk, <<".">>).

'DottedKey.starts_with'(Dk, Target) ->
    case length(Dk) >= length(Target) of
        true -> true;
        false -> false
        end.

'DottedKey.has'(A, Target) ->
    lists:foreach(fun(Dk) ->
        case Dk == Target of
            true -> true;
            false -> ok
        end,
        ok
    end, A),
    false.

new_parser(Config) ->
    #{config => Config, scanner => maps:get(scanner, Config), {vbeam, type} => 'Parser'}.

'Parser.init'(P) ->
    maps:get(tokens, P) bsl 'Scanner.scan'(maps:get(scanner, P)),
    'Parser.next'(P),
    ok.

'Parser.run_checker'(P) ->
    case maps:get(run_checks, maps:get(config, P)) of
        true -> begin
            Chckr = #{scanner => maps:get(scanner, P), {vbeam, type} => 'Checker'},
            'Checker.check'(Chckr, maps:get(root_map, P)),
            lists:foreach(fun(Comment) ->
                'Checker.check_comment'(Chckr, Comment),
                ok
            end, maps:get(comments, maps:get(ast_root, P))),
        end;
        false -> ok
    end,
    ok.

'Parser.run_decoder'(P) ->
    case maps:get(decode_values, maps:get(config, P)) of
        true -> begin
            Dcoder = #{scanner => maps:get(scanner, P), {vbeam, type} => 'Decoder'},
            'Decoder.decode'(Dcoder, maps:get(root_map, P))
        end;
        false -> ok
    end,
    ok.

'Parser.parse'(P) ->
    'Parser.init'(P),
    'Parser.root_table'(P),
    'Parser.run_checker'(P),
    'Parser.run_decoder'(P),
    maps:get(ast_root, P).

'Parser.next'(P) ->
    case length(maps:get(tokens, P)) > 0 of
        true -> begin
            'Token.delete'(maps:get(tokens, P), 0),
            'Parser.peek'(P, 1)
        end;
        false -> begin
            'Parser.peek'(P, 1),
            'Token.delete'(maps:get(tokens, P), 0)
        end
    end,
    ok.

'Parser.peek'(P, N) ->
    case N < 0 of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" peeking backwards is not supported.">>)/binary>>);
        false -> case N == 0 of
            true -> maps:get(peek_tok, P);
            false -> case N =< length(maps:get(tokens, P)) of
                true -> lists:nth(N - 1 + 1, maps:get(tokens, P));
                false -> begin
                    Token_ = #{{vbeam, type} => 'Token'},
                    Count = N - length(maps:get(tokens, P)),
                    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"buffering ", (integer_to_binary(Count))/binary, " tokens...">>),
                    % TODO: unhandled stmt type
                    ok                    Token_
                end
            end
        end
        end.

'Parser.check'(P, Check_token) ->
    case maps:get(kind, maps:get(tok, P)) == Check_token of
        true -> 'Parser.next'(P);
        false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" expected token \"", (Check_token)/binary, "\" but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>)
    end,
    ok.

'Parser.peek_for_correct_line_ending_or_fail'(P) ->
    Peek_tok = element(1, 'Parser.peek_over'(P, 1, [whitespace, tab])),
    case (not lists:member(maps:get(kind, Peek_tok), [cr, nl, hash, eof])) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" unexpected EOL \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" expected one of [.cr, .nl, .hash, .eof] at this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> ok
        end.

'Parser.check_one_of'(P, Tokens) ->
    case lists:member(maps:get(kind, maps:get(tok, P)), Tokens) of
        true -> 'Parser.next'(P);
        false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" expected one of ", (Tokens)/binary, " but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>)
    end,
    ok.

'Parser.ignore_while'(P, Tokens) ->
    case lists:member(maps:get(kind, maps:get(tok, P)), Tokens) of
        true -> begin
            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"ignoring \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" ...">>),
            'Parser.next'(P),
            'Parser.ignore_while'(P, Tokens)
        end;
        false -> ok
    end.

'Parser.ignore_while_peek'(P, Tokens) ->
    % TODO: unhandled stmt type
    ok
'Parser.peek_over'(P, I, Tokens) ->
    Peek_tok = maps:get(peek_tok, P),
    Peek_i = I,
    % TODO: unhandled stmt type
    ok    Peek_tok.

'Parser.is_at'(P, Expected_token) ->
    maps:get(kind, maps:get(tok, P)) == Expected_token.

'Parser.expect'(P, Expected_token) ->
    case maps:get(kind, maps:get(tok, P)) == Expected_token of
        true -> ok;
        false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" expected token \"", (Expected_token)/binary, "\" but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this text \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>)
    end,
    ok.

'Parser.build_abs_dotted_key'(P, Key) ->
    case length(maps:get(root_map_key, P)) > 0 of
        true -> Abs_dotted_key;
        false -> Key
        end.

todo_msvc_astring2dkey(S) ->
    S.

'Parser.check_immutable'(P, Key) ->
    case length(maps:get(immutable, P)) > 0 andalso 'DottedKey.has'(maps:get(immutable, P), Key) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key `", ('DottedKey.str'(Key))/binary, "` is immutable. Unexpected mutation at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> ok
        end.

'Parser.check_explicitly_declared'(P, Key) ->
    case length(maps:get(explicit_declared, P)) > 0 andalso 'DottedKey.has'(maps:get(explicit_declared, P), Key) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key `", ('DottedKey.str'(Key))/binary, "` is already explicitly declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> ok
        end.

'Parser.check_explicitly_declared_array_of_tables'(P, Key) ->
    case length(maps:get(explicit_declared_array_of_tables, P)) > 0 andalso 'DottedKey.has'(maps:get(explicit_declared_array_of_tables, P), Key) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key `", ('DottedKey.str'(Key))/binary, "` is already an explicitly declared array of tables. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> ok
        end.

'Parser.check_implicitly_declared'(P, Key) ->
    case length(maps:get(implicit_declared, P)) > 0 andalso 'DottedKey.has'(maps:get(implicit_declared, P), Key) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key `", ('DottedKey.str'(Key))/binary, "` is already implicitly declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> ok
        end.

'Parser.find_table'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"locating \"", (maps:get(root_map_key, P))/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(maps:get(root_map_key, P)) == 0 of
        true -> T;
        false -> 'Parser.find_in_table'(P, T, maps:get(root_map_key, P))
        end.

'Parser.allocate_table'(P, Key) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"allocating \"", (Key)/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(Key) == 0 of
        true -> ok;
        false -> begin
            'Parser.allocate_in_table'(P, T, Key),
            ok
        end
        end.

'Parser.sub_table_key'(P, Key) ->
    Last = ['DottedKey.last'(Key)],
    First = lists:nth(todo + 1, Key),
    First.

'Parser.find_sub_table'(P, Key) ->
    Ky = todo,
    Ky bsl maps:get(root_map_key, P),
    Ky bsl Key,
    case length(maps:get(root_map_key, P)) == 0 of
        true -> ok;
        false -> ok
    end,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"locating \"", (Ky)/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(Ky) == 0 of
        true -> T;
        false -> 'Parser.find_in_table'(P, T, Ky)
        end.

'Parser.find_in_table'(P, Table, Key) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"locating \"", (Key)/binary, "\" in map ", (ptr_str(Table))/binary>>),
    T = todo,
    % TODO: unhandled stmt type
    ok    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"returning map ", (ptr_str(T))/binary, "\"">>),
    T.

is_all_tables(Table, Dotted_key) ->
    case length(Dotted_key) == 0 of
        true -> false;
        false -> begin
            % TODO: unhandled stmt type
            ok            true
        end
        end.

'Parser.find_array_of_tables'(P) ->
    T = todo,
    Key = maps:get(last_aot, P),
    case length(Key) > 1 of
        true -> ok;
        false -> ok
    end,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"locating \"", (Key)/binary, "\" in map ", (ptr_str(T))/binary>>),
    % TODO: unhandled stmt type
    ok    error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<"no key `", (Key)/binary, "` found in map ", (ptr_str(T))/binary, "\"">>)/binary>>).

'Parser.allocate_in_table'(P, Table, Key) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"allocating \"", (Key)/binary, "\" in map ", (ptr_str(Table))/binary>>),
    T = todo,
    % TODO: unhandled stmt type
    ok    ok.

'Parser.dotted_key'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing dotted key...">>),
    Dotted_key = todo,
    Key = 'Parser.key'(P),
    'Parser.ignore_while_peek'(P, [whitespace, tab]),
    Dotted_key bsl 'Key.str'(Key),
    % TODO: unhandled stmt type
    ok    'Parser.next'(P),
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed dotted key `", (Dotted_key)/binary, "` now at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Dotted_key.

'Parser.root_table'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing root table...">>),
    % TODO: unhandled stmt type
    ok    ok.

'Parser.excerpt'(P) ->
    'Scanner.excerpt'(maps:get(scanner, P), maps:get(pos, maps:get(tok, P)), 10).

'Parser.table_contents'(P, Tbl) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing table contents...">>),
    % TODO: unhandled stmt type
    ok    ok.

'Parser.inline_table'(P, Tbl) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing inline table into ", (ptr_str(Tbl))/binary, "...">>),
    % TODO: unhandled stmt type
    ok    Previous_token_was_value = false,
    % TODO: unhandled stmt type
    ok    error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" unexpected end of inline-table \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" at this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>).

'Parser.array_of_tables'(P, Table) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing array of tables \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    'Parser.check'(P, lsbr),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Peek_tok = element(1, 'Parser.peek_over'(P, 1, [whitespace, tab])),
    'Parser.ignore_while'(P, [whitespace, tab]),
    case maps:get(kind, Peek_tok) == period of
        true -> ok;
        false -> begin
            Key = 'Parser.key'(P),
            'Parser.next'(P),
            'Parser.ignore_while'(P, [whitespace, tab]),
            'Parser.check'(P, rsbr),
            'Parser.peek_for_correct_line_ending_or_fail'(P),
            'Parser.expect'(P, rsbr),
            'Parser.ignore_while'(P, [whitespace, tab, cr, nl]),
            Dotted_key = todo,
            Dotted_key_str = 'DottedKey.str'(Dotted_key),
            'Parser.check_explicitly_declared'(P, Dotted_key),
            % TODO: unhandled stmt type
            ok            % TODO: unhandled stmt type
            ok            ok
        end
        end.

'Parser.array_of_tables_contents'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing contents from \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Tbl = #{},
    'Parser.table_contents'(P, Tbl),
    Arr = [],
    Arr bsl Tbl,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed array of tables ", (todo)/binary, ". leaving at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Arr.

'Parser.double_array_of_tables'(P, Table) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing nested array of tables \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Dotted_key = 'Parser.dotted_key'(P),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, rsbr),
    'Parser.expect'(P, rsbr),
    'Parser.ignore_while'(P, [whitespace, tab, cr, nl]),
    'Parser.check_explicitly_declared'(P, Dotted_key),
    case is_all_tables(maps:get(root_map, P), Dotted_key) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key `", ('DottedKey.str'(Dotted_key))/binary, "` is already declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
        false -> begin
            case not 'DottedKey.has'(maps:get(explicit_declared_array_of_tables, P), Dotted_key) of
                true -> maps:get(explicit_declared_array_of_tables, P) bsl Dotted_key;
                false -> ok
            end,
            First = todo,
            Last = todo,
            T_arr = todo,
            T_map = todo,
            % TODO: unhandled stmt type
            ok            ok
        end
        end.

'Parser.double_array_of_tables_contents'(P, Target_key) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing contents from \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Tbl = #{},
    Implicit_allocation_key = todo,
    Peeked_over = 0,
    Peek_tok = maps:get(peek_tok, P),
    % TODO: unhandled stmt type
    ok    Arr = [],
    Arr bsl Tbl,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed array of tables ", (todo)/binary, ". leaving at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Arr.

'Parser.array'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing array...">>),
    Arr = [],
    'Parser.expect'(P, lsbr),
    Previous_token_was_value = false,
    % TODO: unhandled stmt type
    ok    'Parser.expect'(P, rsbr),
    % TODO: unhandled stmt type
    ok    Arr.

'Parser.comment'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed hash comment \"#", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    #{text => maps:get(lit, maps:get(tok, P)), pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'Comment'}.

'Parser.key'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing key from \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" ...">>),
    Key = todo,
    case maps:get(kind, maps:get(tok, P)) == number of
        true -> begin
            case maps:get(kind, maps:get(peek_tok, P)) == minus of
                true -> begin
                    Lits = maps:get(lit, maps:get(tok, P)),
                    Pos = 'Token.pos'(maps:get(tok, P)),
                    % TODO: unhandled stmt type
                    ok                    todo
                end;
                false -> ok
            end,
            Num = 'Parser.number'(P),
            case lists:member(maps:get(kind, maps:get(peek_tok, P)), [bare, underscore, minus]) of
                true -> begin
                    Bare = 'Parser.bare'(P),
                    Bare
                end;
                false -> ok
            end,
            Key1 = todo,
        end;
        false -> ok
    end,
    case Key1 is todo of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" key expected .bare, .underscore, .number, .quoted or .boolean but got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\"">>)/binary>>);
        false -> begin
            case Key1 is todo of
                true -> begin
                    case maps:get(run_checks, maps:get(config, P)) of
                        true -> begin
                            Quoted = todo,
                            case maps:get(is_multiline, Quoted) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" multiline string as key is not allowed. (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
                                false -> ok
                            end,
                            Chckr = #{scanner => maps:get(scanner, P), {vbeam, type} => 'Checker'},
                            'Checker.check_quoted'(Chckr, Quoted)
                        end;
                        false -> ok
                    end,
                    case maps:get(decode_values, maps:get(config, P)) of
                        true -> begin
                            Quoted1 = todo,
                            decode_quoted_escapes(Quoted1),
                            Key2 = todo,
                        end;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            Key2
        end
        end.

'Parser.key_value'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing key value pair...">>),
    Key = 'Parser.key'(P),
    Dotted_key = todo,
    maps:get(explicit_declared, P) bsl 'Parser.build_abs_dotted_key'(P, Dotted_key),
    'Parser.next'(P),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, assign),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Value = 'Parser.value'(P),
    case maps:get(value_is_immutable, P) of
        true -> begin
            case not 'DottedKey.has'(maps:get(immutable, P), Dotted_key) of
                true -> maps:get(immutable, P) bsl 'Parser.build_abs_dotted_key'(P, Dotted_key);
                false -> ok
            end,
        end;
        false -> ok
    end,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed key value pair. `", (Key)/binary, " = ", (Value)/binary, "`">>),
    Key.

'Parser.dotted_key_value'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing dotted key value pair...">>),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Dotted_key = 'Parser.dotted_key'(P),
    maps:get(explicit_declared, P) bsl 'Parser.build_abs_dotted_key'(P, Dotted_key),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, assign),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Value = 'Parser.value'(P),
    case maps:get(value_is_immutable, P) of
        true -> begin
            case not 'DottedKey.has'(maps:get(immutable, P), Dotted_key) of
                true -> maps:get(immutable, P) bsl 'Parser.build_abs_dotted_key'(P, Dotted_key);
                false -> ok
            end,
        end;
        false -> ok
    end,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed dotted key value pair `", (Dotted_key)/binary, " = ", (Value)/binary, "`...">>),
    Dotted_key.

'Parser.value'(P) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsing value from token \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"...">>),
    Value = todo,
    case maps:get(kind, maps:get(tok, P)) == number of
        true -> begin
            Number_or_date = 'Parser.number_or_date'(P),
            Value1 = Number_or_date,
        end;
        false -> begin
            Value2 = case maps:get(kind, maps:get(tok, P)) of
                quoted -> todo;
                boolean -> todo;
                lsbr -> todo;
                lcbr -> begin
                    'Parser.ignore_while'(P, [whitespace, tab]),
                    T = #{},
                    'Parser.inline_table'(P, T),
                    todo
                end;
                _ -> todo
            end,
            case Value2 is todo of
                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" value expected .boolean, .quoted, .lsbr, .lcbr or .number got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)/binary>>);
                false -> ok
            end
        end
    end,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" as value ", (Value2)/binary>>),
    Value2.

'Parser.number_or_date'(P) ->
    case maps:get(kind, maps:get(peek_tok, P)) == minus orelse maps:get(kind, maps:get(peek_tok, P)) == colon of
        true -> begin
            Date_time_type = 'Parser.date_time'(P),
            case Date_time_type of
                todo -> todo;
                todo -> todo;
                todo -> todo
            end
        end;
        false -> ok
    end,
    todo.

'Parser.bare'(P) ->
    Lits = maps:get(lit, maps:get(tok, P)),
    Pos = 'Token.pos'(maps:get(tok, P)),
    % TODO: unhandled stmt type
    ok    #{text => Lits, pos => Pos, {vbeam, type} => 'Bare'}.

'Parser.quoted'(P) ->
    Quote = lists:nth(1, maps:get(lit, maps:get(tok, P))),
    Is_multiline = length(maps:get(lit, maps:get(tok, P))) >= 6 andalso lists:nth(2, maps:get(lit, maps:get(tok, P))) == Quote andalso lists:nth(3, maps:get(lit, maps:get(tok, P))) == Quote,
    Lit = lists:nth(todo + 1, maps:get(lit, maps:get(tok, P))),
    case Is_multiline of
        true -> begin
            Lit1 = lists:nth(todo + 1, maps:get(lit, maps:get(tok, P))),
            case length(Lit1) > 0 andalso lists:nth(1, Lit1) == todo of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    #{text => Lit1, pos => 'Token.pos'(maps:get(tok, P)), quote => Quote, is_multiline => Is_multiline, {vbeam, type} => 'Quoted'}.

'Parser.boolean'(P) ->
    case (not lists:member(maps:get(lit, maps:get(tok, P)), [<<"true">>, <<"false">>])) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" expected literal to be either `true` or `false` got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\"">>)/binary>>);
        false -> #{text => maps:get(lit, maps:get(tok, P)), pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'Bool'}
        end.

'Parser.number'(P) ->
    #{text => maps:get(lit, maps:get(tok, P)), pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'Number'}.

'Parser.date_time'(P) ->
    Lit = <<"">>,
    Pos = 'Token.pos'(maps:get(tok, P)),
    Date = #{{vbeam, type} => 'Date'},
    Time = #{{vbeam, type} => 'Time'},
    case maps:get(kind, maps:get(peek_tok, P)) == minus of
        true -> begin
            Date1 = 'Parser.date'(P),
            Lit1 = maps:get(text, Date1),
            case (maps:get(kind, maps:get(peek_tok, P)) == bare andalso ('string.starts_with'(maps:get(lit, maps:get(peek_tok, P)), <<"T">>) orelse 'string.starts_with'(maps:get(lit, maps:get(peek_tok, P)), <<"t">>))) orelse maps:get(kind, maps:get(peek_tok, P)) == whitespace of
                true -> begin
                    'Parser.next'(P),
                    case 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"T">>) orelse 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"t">>) of
                        true -> ok;
                        false -> begin
                            Peek = 'Parser.peek'(P, 0),
                            case maps:get(kind, Peek) /= number of
                                true -> #{text => Lit1, pos => Pos, {vbeam, type} => 'Date'};
                                false -> ok
                            end,
                            Lit2 = maps:get(lit, maps:get(tok, P)),
                            'Parser.next'(P)
                        end
                    end,
                    Time1 = 'Parser.time'(P),
                    Lit3 = maps:get(text, Time1),
                    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed date-time: \"", (Lit3)/binary, "\"">>),
                    #{text => Lit3, pos => Pos, date => Date1, time => Time1, {vbeam, type} => 'DateTime'}
                end;
                false -> ok
            end
        end;
        false -> case maps:get(kind, maps:get(peek_tok, P)) == colon of
            true -> begin
                Time2 = 'Parser.time'(P),
                Time2
            end;
            false -> ok
        end
    end,
    #{text => Lit3, pos => Pos, {vbeam, type} => 'Date'}.

'Parser.date'(P) ->
    Lit = maps:get(lit, maps:get(tok, P)),
    Pos = 'Token.pos'(maps:get(tok, P)),
    'Parser.check'(P, number),
    Lit1 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, minus),
    Lit2 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, number),
    Lit3 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, minus),
    Lit4 = maps:get(lit, maps:get(tok, P)),
    'Parser.expect'(P, number),
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed date: \"", (Lit4)/binary, "\"">>),
    #{text => Lit4, pos => Pos, {vbeam, type} => 'Date'}.

'Parser.time'(P) ->
    Lit = maps:get(lit, maps:get(tok, P)),
    Pos = 'Token.pos'(maps:get(tok, P)),
    case 'Parser.is_at'(P, bare) andalso ('string.starts_with'(Lit, <<"T">>) orelse 'string.starts_with'(Lit, <<"t">>)) of
        true -> begin
            case 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"T">>) of
                true -> ok;
                false -> case 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"t">>) of
                    true -> ok;
                    false -> ok
                end
            end,
            'Parser.next'(P)
        end;
        false -> 'Parser.check'(P, number)
    end,
    Lit1 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, colon),
    Lit2 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, number),
    Lit3 = maps:get(lit, maps:get(tok, P)),
    'Parser.check'(P, colon),
    Lit4 = maps:get(lit, maps:get(tok, P)),
    'Parser.expect'(P, number),
    case maps:get(kind, maps:get(peek_tok, P)) == period of
        true -> begin
            'Parser.next'(P),
            Lit5 = maps:get(lit, maps:get(tok, P)),
            'Parser.check'(P, period),
            Lit6 = maps:get(lit, maps:get(tok, P)),
            'Parser.expect'(P, number)
        end;
        false -> ok
    end,
    case not 'u8.is_digit'(lists:nth(length(Lit6) - 1 + 1, Lit6)) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" expected a number as last occurrence in \"", (Lit6)/binary, "\" got \"", ('u8.ascii_str'(lists:nth(length(Lit6) - 1 + 1, Lit6)))/binary, "\"">>)/binary>>);
        false -> begin
            case maps:get(kind, maps:get(peek_tok, P)) == minus orelse maps:get(kind, maps:get(peek_tok, P)) == plus of
                true -> begin
                    'Parser.next'(P),
                    Lit7 = maps:get(lit, maps:get(tok, P)),
                    'Parser.check_one_of'(P, [minus, plus]),
                    Lit8 = maps:get(lit, maps:get(tok, P)),
                    'Parser.check'(P, number),
                    Lit9 = maps:get(lit, maps:get(tok, P)),
                    'Parser.check'(P, colon),
                    Lit10 = maps:get(lit, maps:get(tok, P)),
                    'Parser.expect'(P, number)
                end;
                false -> case maps:get(kind, maps:get(peek_tok, P)) == bare andalso (maps:get(lit, maps:get(peek_tok, P)) == <<"Z">> orelse maps:get(lit, maps:get(peek_tok, P)) == <<"z">>) of
                    true -> begin
                        'Parser.next'(P),
                        Lit11 = maps:get(lit, maps:get(tok, P)),
                        'Parser.expect'(P, bare)
                    end;
                    false -> ok
                end
            end,
            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"parsed time: \"", (Lit11)/binary, "\"">>),
            #{text => Lit11, pos => Pos, {vbeam, type} => 'Time'}
        end
        end.

'Parser.undo_special_case_01'(P, Dotted_key) ->
    Exd_i = 'DottedKey.index'(maps:get(explicit_declared, P), Dotted_key),
    case Exd_i > -1 of
        true -> begin
            'DottedKey.delete'(maps:get(explicit_declared, P), Exd_i),
            'DottedKey.clear'(maps:get(last_aot, P))
        end;
        false -> ok
    end.

'Parser.eof'(P) ->
    #{pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'EOF'}.
