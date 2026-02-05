-module('v.toml.parser').
-export(['DottedKey.str'/1, 'DottedKey.starts_with'/2, 'DottedKey.has'/2, new_parser/1, 'Parser.init'/1, 'Parser.run_checker'/1, 'Parser.run_decoder'/1, 'Parser.parse'/1, 'Parser.next'/1, 'Parser.peek'/2, 'Parser.check'/2, 'Parser.peek_for_correct_line_ending_or_fail'/1, 'Parser.check_one_of'/2, 'Parser.ignore_while'/2, 'Parser.ignore_while_peek'/2, 'Parser.peek_over'/3, 'Parser.is_at'/2, 'Parser.expect'/2, 'Parser.build_abs_dotted_key'/2, todo_msvc_astring2dkey/1, 'Parser.check_immutable'/2, 'Parser.check_explicitly_declared'/2, 'Parser.check_explicitly_declared_array_of_tables'/2, 'Parser.check_implicitly_declared'/2, 'Parser.find_table'/1, 'Parser.allocate_table'/2, 'Parser.sub_table_key'/2, 'Parser.find_sub_table'/2, 'Parser.find_in_table'/3, is_all_tables/2, 'Parser.find_array_of_tables'/1, 'Parser.allocate_in_table'/3, 'Parser.dotted_key'/1, 'Parser.root_table'/1, 'Parser.excerpt'/1, 'Parser.table_contents'/2, 'Parser.inline_table'/2, 'Parser.array_of_tables'/2, 'Parser.array_of_tables_contents'/1, 'Parser.double_array_of_tables'/2, 'Parser.double_array_of_tables_contents'/2, 'Parser.array'/1, 'Parser.comment'/1, 'Parser.key'/1, 'Parser.key_value'/1, 'Parser.dotted_key_value'/1, 'Parser.value'/1, 'Parser.number_or_date'/1, 'Parser.bare'/1, 'Parser.quoted'/1, 'Parser.boolean'/1, 'Parser.number'/1, 'Parser.date_time'/1, 'Parser.date'/1, 'Parser.time'/1, 'Parser.undo_special_case_01'/2, 'Parser.eof'/1]).
% TODO: const all_formatting = [.whitespace, .tab, .cr, .nl];
% TODO: const space_formatting = [.whitespace, .tab];
% TODO: const keys_and_space_formatting = [.whitespace, .tab, .minus, .bare, .quoted, .boolean, .number, .underscore];
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

'DottedKey.str'(Dk) ->
    'DottedKey.join'(Dk, <<".">>).

'DottedKey.starts_with'(Dk, Target) ->
    case length(Dk) >= length(Target) of
        true -> begin
            % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
            true
        end;
        false -> ok
    end,
    false.

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
    maps:get(tokens, P) << 'Scanner.scan'(maps:get(scanner, P)),
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
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" peeking backwards is not supported.">>);
        false -> ok
    end,
    case N == 0 of
        true -> maps:get(peek_tok, P);
        false -> case N <= length(maps:get(tokens, P)) of
            true -> lists:nth(N - 1 + 1, maps:get(tokens, P));
            false -> begin
                Token_ = #{{vbeam, type} => 'Token'},
                Count = N - length(maps:get(tokens, P)),
                printdbg(todo + <<".">> + todo + <<".">> + todo, <<"buffering ", (integer_to_binary(Count))/binary, " tokens...">>),
                % TODO: for token_.kind != .eof && count != 0 {
                Token_
            end
        end
    end.

'Parser.check'(P, Check_token) ->
    case maps:get(kind, maps:get(tok, P)) == Check_token of
        true -> 'Parser.next'(P);
        false -> error(todo + <<".">> + todo + <<".">> + todo + <<" expected token \"", (Check_token)/binary, "\" but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)
    end,
    ok.

'Parser.peek_for_correct_line_ending_or_fail'(P) ->
    Peek_tok = element(1, 'Parser.peek_over'(P, 1, [whitespace, tab])),
    case maps:get(kind, Peek_tok) !in [cr, nl, hash, eof] of
        true -> begin
            'Parser.next'(P),
            error(todo + <<".">> + todo + <<".">> + todo + <<" unexpected EOL \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" expected one of [.cr, .nl, .hash, .eof] at this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)
        end;
        false -> ok
    end,
    ok.

'Parser.check_one_of'(P, Tokens) ->
    case maps:get(kind, maps:get(tok, P)) in Tokens of
        true -> 'Parser.next'(P);
        false -> error(todo + <<".">> + todo + <<".">> + todo + <<" expected one of ", (Tokens)/binary, " but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>)
    end,
    ok.

'Parser.ignore_while'(P, Tokens) ->
    case maps:get(kind, maps:get(tok, P)) in Tokens of
        true -> begin
            printdbg(todo + <<".">> + todo + <<".">> + todo, <<"ignoring \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" ...">>),
            'Parser.next'(P),
            'Parser.ignore_while'(P, Tokens)
        end;
        false -> ok
    end.

'Parser.ignore_while_peek'(P, Tokens) ->
    % TODO: for p.peek_tok.kind in tokens {

'Parser.peek_over'(P, I, Tokens) ->
    Peek_tok = maps:get(peek_tok, P),
    Peek_i = I,
    % TODO: for peek_tok.kind in tokens {
    Peek_tok.

'Parser.is_at'(P, Expected_token) ->
    maps:get(kind, maps:get(tok, P)) == Expected_token.

'Parser.expect'(P, Expected_token) ->
    case maps:get(kind, maps:get(tok, P)) == Expected_token of
        true -> ok;
        false -> error(todo + <<".">> + todo + <<".">> + todo + <<" expected token \"", (Expected_token)/binary, "\" but found \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" in this text \"...", ('Parser.excerpt'(P))/binary, "...\"">>)
    end,
    ok.

'Parser.build_abs_dotted_key'(P, Key) ->
    case length(maps:get(root_map_key, P)) > 0 of
        true -> begin
            Abs_dotted_key = todo,
            Abs_dotted_key << maps:get(root_map_key, P),
            Abs_dotted_key << Key,
            Abs_dotted_key
        end;
        false -> ok
    end,
    Key.

todo_msvc_astring2dkey(S) ->
    S.

'Parser.check_immutable'(P, Key) ->
    case length(maps:get(immutable, P)) > 0 && 'DottedKey.has'(maps:get(immutable, P), Key) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key `", ('DottedKey.str'(Key))/binary, "` is immutable. Unexpected mutation at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
        false -> ok
    end,
    ok.

'Parser.check_explicitly_declared'(P, Key) ->
    case length(maps:get(explicit_declared, P)) > 0 && 'DottedKey.has'(maps:get(explicit_declared, P), Key) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key `", ('DottedKey.str'(Key))/binary, "` is already explicitly declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
        false -> ok
    end,
    ok.

'Parser.check_explicitly_declared_array_of_tables'(P, Key) ->
    case length(maps:get(explicit_declared_array_of_tables, P)) > 0 && 'DottedKey.has'(maps:get(explicit_declared_array_of_tables, P), Key) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key `", ('DottedKey.str'(Key))/binary, "` is already an explicitly declared array of tables. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
        false -> ok
    end,
    ok.

'Parser.check_implicitly_declared'(P, Key) ->
    case length(maps:get(implicit_declared, P)) > 0 && 'DottedKey.has'(maps:get(implicit_declared, P), Key) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key `", ('DottedKey.str'(Key))/binary, "` is already implicitly declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
        false -> ok
    end,
    ok.

'Parser.find_table'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"locating \"", (maps:get(root_map_key, P))/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(maps:get(root_map_key, P)) == 0 of
        true -> T;
        false -> ok
    end,
    'Parser.find_in_table'(P, T, maps:get(root_map_key, P)).

'Parser.allocate_table'(P, Key) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"allocating \"", (Key)/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(Key) == 0 of
        true -> ok;
        false -> ok
    end,
    'Parser.allocate_in_table'(P, T, Key),
    ok.

'Parser.sub_table_key'(P, Key) ->
    Last = ['DottedKey.last'(Key)],
    First = lists:nth(todo + 1, Key),
    First.

'Parser.find_sub_table'(P, Key) ->
    Ky = todo,
    Ky << maps:get(root_map_key, P),
    Ky << Key,
    case length(maps:get(root_map_key, P)) == 0 of
        true -> ok;
        false -> ok
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"locating \"", (Ky)/binary, "\" in map ", (ptr_str(maps:get(root_map, P)))/binary>>),
    T = todo,
    case length(Ky) == 0 of
        true -> T;
        false -> ok
    end,
    'Parser.find_in_table'(P, T, Ky).

'Parser.find_in_table'(P, Table, Key) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"locating \"", (Key)/binary, "\" in map ", (ptr_str(Table))/binary>>),
    T = todo,
    % TODO: {for k in key {;}
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"returning map ", (ptr_str(T))/binary, "\"">>),
    T.

is_all_tables(Table, Dotted_key) ->
    case length(Dotted_key) == 0 of
        true -> false;
        false -> ok
    end,
    % TODO: {mut t := &table;for key in dotted_key {;}
    true.

'Parser.find_array_of_tables'(P) ->
    T = todo,
    Key = maps:get(last_aot, P),
    case length(Key) > 1 of
        true -> ok;
        false -> ok
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"locating \"", (Key)/binary, "\" in map ", (ptr_str(T))/binary>>),
    % TODO: { if val := t[key.str()] { toml.util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'found key "${key}" in ${t.keys()}') if val is TypeNode([]toml.ast.Value) { return val } };}
    error(todo + <<".">> + todo + <<".">> + todo + <<"no key `", (Key)/binary, "` found in map ", (ptr_str(T))/binary, "\"">>).

'Parser.allocate_in_table'(P, Table, Key) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"allocating \"", (Key)/binary, "\" in map ", (ptr_str(Table))/binary>>),
    T = todo,
    % TODO: {for k in key {;}
    ok.

'Parser.dotted_key'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing dotted key...">>),
    Dotted_key = todo,
    Key = 'Parser.key'(P),
    'Parser.ignore_while_peek'(P, [whitespace, tab]),
    Dotted_key << 'Key.str'(Key),
    % TODO: for p.peek_tok.kind == .period {
    'Parser.next'(P),
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed dotted key `", (Dotted_key)/binary, "` now at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Dotted_key.

'Parser.root_table'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing root table...">>),
    % TODO: for p.tok.kind != .eof {
    ok.

'Parser.excerpt'(P) ->
    'Scanner.excerpt'(maps:get(scanner, P), maps:get(pos, maps:get(tok, P)), 10).

'Parser.table_contents'(P, Tbl) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing table contents...">>),
    % TODO: for p.tok.kind != .eof {
    ok.

'Parser.inline_table'(P, Tbl) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing inline table into ", (ptr_str(Tbl))/binary, "...">>),
    % TODO: defer {p.value_is_immutable = true;}
    Previous_token_was_value = false,
    % TODO: for p.tok.kind != .eof {
    error(todo + <<".">> + todo + <<".">> + todo + <<" unexpected end of inline-table \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" at this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>).

'Parser.array_of_tables'(P, Table) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing array of tables \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    'Parser.check'(P, lsbr),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Peek_tok = element(1, 'Parser.peek_over'(P, 1, [whitespace, tab])),
    'Parser.ignore_while'(P, [whitespace, tab]),
    case maps:get(kind, Peek_tok) == period of
        true -> begin
            'Parser.double_array_of_tables'(P, Table),
        end;
        false -> ok
    end,
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
    % TODO: { if val := table[dotted_key_str] {  if val is TypeNode([]toml.ast.Value) { arr := &(table[dotted_key_str] as []ast.Value)arr << p.array_of_tables_contents()!table[dotted_key_str] = arr } else { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' table[${dotted_key_str}] is not an array. (excerpt): "...${p.excerpt()}..."') } } else { table[dotted_key_str] = p.array_of_tables_contents()! };}
    % TODO: {arr := &(table[p.last_aot.str()] as []ast.Value);p.last_aot_index = arr.len - 1;}
    ok.

'Parser.array_of_tables_contents'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing contents from \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Tbl = #{},
    'Parser.table_contents'(P, Tbl),
    Arr = [],
    Arr << Tbl,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed array of tables ", (todo)/binary, ". leaving at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Arr.

'Parser.double_array_of_tables'(P, Table) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing nested array of tables \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Dotted_key = 'Parser.dotted_key'(P),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, rsbr),
    'Parser.expect'(P, rsbr),
    'Parser.ignore_while'(P, [whitespace, tab, cr, nl]),
    'Parser.check_explicitly_declared'(P, Dotted_key),
    case is_all_tables(maps:get(root_map, P), Dotted_key) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key `", ('DottedKey.str'(Dotted_key))/binary, "` is already declared. Unexpected redeclaration at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
        false -> ok
    end,
    case !'DottedKey.has'(maps:get(explicit_declared_array_of_tables, P), Dotted_key) of
        true -> maps:get(explicit_declared_array_of_tables, P) << Dotted_key;
        false -> ok
    end,
    First = todo,
    Last = todo,
    T_arr = todo,
    T_map = todo,
    % TODO: { if first != p.last_aot { toml.util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, '${first} != ${p.last_aot}') if p.last_aot.len == 0 { p.last_aot = firstmut nm := &p.root_map if first.str() in table.keys() { toml.util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'adding to existing table entry at `${first}`.')table_first := table[first.str()] if table_first !is TypeNode(map[string]toml.ast.Value) { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' expected a table at "${first.str()}" but got "${table_first.type_name()}" instead. (excerpt): "...${p.excerpt()}..."') }nm = &(table_first as map[string]ast.Value) } else { toml.util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'implicit allocation of map for `${first}` in dotted key `${dotted_key}`.')nm = &map{  }p.implicit_declared << firstp.explicit_declared << first }nm[last.str()] = []table[first.str()] = ast.Value(nm)t_arr = &(nm[last.str()] as []ast.Value)t_arr << p.array_of_tables_contents()!return } else { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' nested array of tables key "${first}" does not match "${p.last_aot}". (excerpt): "...${p.excerpt()}..."') } };array_of_tables := table[p.last_aot.str()]; if first == p.last_aot {  if array_of_tables is TypeNode(map[string]toml.ast.Value) { p.undo_special_case_01(dotted_key)p.next()!return } }; if array_of_tables !is TypeNode([]toml.ast.Value) { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' nested array of tables "${p.last_aot}" expected an array but got "${table[p.last_aot.str()].type_name()}". Re-definition is not allowed. (excerpt): "...${p.excerpt()}..."') };t_arr = &(array_of_tables as []ast.Value);t_map = ast.Value(map{  }); if p.last_aot_index < t_arr.len { t_map = t_arr[p.last_aot_index] }; if t_map !is TypeNode(map[string]toml.ast.Value) { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' expected a table but got "${t_map.type_name()}". (excerpt): "...${p.excerpt()}..."') };mut t := &(t_map as map[string]ast.Value); if val := t[last.str()] {  if val is TypeNode([]toml.ast.Value) { mut arr := &valarr << p.double_array_of_tables_contents(dotted_key)!t[last.str()] = arr } else { return toml.parser.error(@MOD + '.' + @STRUCT + '.' + @FN + ' t[${last.str()}] is not an array. (excerpt): "...${p.excerpt()}..."') } } else { t[last.str()] = p.double_array_of_tables_contents(dotted_key)! }; if t_arr.len == 0 { t_arr << tp.last_aot_index = t_arr.len - 1 };}
    ok.

'Parser.double_array_of_tables_contents'(P, Target_key) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing contents from \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Tbl = #{},
    Implicit_allocation_key = todo,
    Peeked_over = 0,
    Peek_tok = maps:get(peek_tok, P),
    % TODO: for p.tok.kind != .eof {
    Arr = [],
    Arr << Tbl,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed array of tables ", (todo)/binary, ". leaving at \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    Arr.

'Parser.array'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing array...">>),
    Arr = [],
    'Parser.expect'(P, lsbr),
    Previous_token_was_value = false,
    % TODO: for p.tok.kind != .eof {
    'Parser.expect'(P, rsbr),
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    Arr.

'Parser.comment'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed hash comment \"#", (maps:get(lit, maps:get(tok, P)))/binary, "\"">>),
    #{text => maps:get(lit, maps:get(tok, P)), pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'Comment'}.

'Parser.key'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing key from \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" ...">>),
    Key = todo,
    case maps:get(kind, maps:get(tok, P)) == number of
        true -> begin
            case maps:get(kind, maps:get(peek_tok, P)) == minus of
                true -> begin
                    Lits = maps:get(lit, maps:get(tok, P)),
                    Pos = 'Token.pos'(maps:get(tok, P)),
                    % TODO: for p.peek_tok.kind != .assign && p.peek_tok.kind != .period && p.peek_tok.kind != .rsbr {
                    todo
                end;
                false -> ok
            end,
            Num = 'Parser.number'(P),
            case maps:get(kind, maps:get(peek_tok, P)) in [bare, underscore, minus] of
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
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" key expected .bare, .underscore, .number, .quoted or .boolean but got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\"">>);
        false -> ok
    end,
    case Key1 is todo of
        true -> begin
            case maps:get(run_checks, maps:get(config, P)) of
                true -> begin
                    Quoted = todo,
                    case maps:get(is_multiline, Quoted) of
                        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" multiline string as key is not allowed. (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
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
    Key2.

'Parser.key_value'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing key value pair...">>),
    Key = 'Parser.key'(P),
    Dotted_key = todo,
    maps:get(explicit_declared, P) << 'Parser.build_abs_dotted_key'(P, Dotted_key),
    'Parser.next'(P),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, assign),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Value = 'Parser.value'(P),
    case maps:get(value_is_immutable, P) of
        true -> begin
            case !'DottedKey.has'(maps:get(immutable, P), Dotted_key) of
                true -> maps:get(immutable, P) << 'Parser.build_abs_dotted_key'(P, Dotted_key);
                false -> ok
            end,
        end;
        false -> ok
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed key value pair. `", (Key)/binary, " = ", (Value)/binary, "`">>),
    Key.

'Parser.dotted_key_value'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing dotted key value pair...">>),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Dotted_key = 'Parser.dotted_key'(P),
    maps:get(explicit_declared, P) << 'Parser.build_abs_dotted_key'(P, Dotted_key),
    'Parser.ignore_while'(P, [whitespace, tab]),
    'Parser.check'(P, assign),
    'Parser.ignore_while'(P, [whitespace, tab]),
    Value = 'Parser.value'(P),
    case maps:get(value_is_immutable, P) of
        true -> begin
            case !'DottedKey.has'(maps:get(immutable, P), Dotted_key) of
                true -> maps:get(immutable, P) << 'Parser.build_abs_dotted_key'(P, Dotted_key);
                false -> ok
            end,
        end;
        false -> ok
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed dotted key value pair `", (Dotted_key)/binary, " = ", (Value)/binary, "`...">>),
    Dotted_key.

'Parser.value'(P) ->
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsing value from token \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\"...">>),
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
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" value expected .boolean, .quoted, .lsbr, .lcbr or .number got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" \"", (maps:get(lit, maps:get(tok, P)))/binary, "\" in this (excerpt): \"...", ('Parser.excerpt'(P))/binary, "...\"">>);
                false -> ok
            end
        end
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed \"", (maps:get(kind, maps:get(tok, P)))/binary, "\" as value ", (Value2)/binary>>),
    Value2.

'Parser.number_or_date'(P) ->
    case maps:get(kind, maps:get(peek_tok, P)) == minus || maps:get(kind, maps:get(peek_tok, P)) == colon of
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
    % TODO: for p.peek_tok.kind != .assign && p.peek_tok.kind != .period && p.peek_tok.kind != .rsbr && p.peek_tok.kind !in toml.parser.space_formatting {
    #{text => Lits, pos => Pos, {vbeam, type} => 'Bare'}.

'Parser.quoted'(P) ->
    Quote = lists:nth(1, maps:get(lit, maps:get(tok, P))),
    Is_multiline = length(maps:get(lit, maps:get(tok, P))) >= 6 && lists:nth(2, maps:get(lit, maps:get(tok, P))) == Quote && lists:nth(3, maps:get(lit, maps:get(tok, P))) == Quote,
    Lit = lists:nth(todo + 1, maps:get(lit, maps:get(tok, P))),
    case Is_multiline of
        true -> begin
            Lit1 = lists:nth(todo + 1, maps:get(lit, maps:get(tok, P))),
            case length(Lit1) > 0 && lists:nth(1, Lit1) == todo of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    #{text => Lit1, pos => 'Token.pos'(maps:get(tok, P)), quote => Quote, is_multiline => Is_multiline, {vbeam, type} => 'Quoted'}.

'Parser.boolean'(P) ->
    case maps:get(lit, maps:get(tok, P)) !in [<<"true">>, <<"false">>] of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" expected literal to be either `true` or `false` got \"", (maps:get(kind, maps:get(tok, P)))/binary, "\"">>);
        false -> ok
    end,
    #{text => maps:get(lit, maps:get(tok, P)), pos => 'Token.pos'(maps:get(tok, P)), {vbeam, type} => 'Bool'}.

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
            case (maps:get(kind, maps:get(peek_tok, P)) == bare && ('string.starts_with'(maps:get(lit, maps:get(peek_tok, P)), <<"T">>) || 'string.starts_with'(maps:get(lit, maps:get(peek_tok, P)), <<"t">>))) || maps:get(kind, maps:get(peek_tok, P)) == whitespace of
                true -> begin
                    'Parser.next'(P),
                    case 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"T">>) || 'string.starts_with'(maps:get(lit, maps:get(tok, P)), <<"t">>) of
                        true -> ok;
                        false -> begin
                            Peek = 'Parser.peek'(P, 0),
                            case maps:get(kind, Peek) != number of
                                true -> #{text => Lit1, pos => Pos, {vbeam, type} => 'Date'};
                                false -> ok
                            end,
                            Lit2 = maps:get(lit, maps:get(tok, P)),
                            'Parser.next'(P)
                        end
                    end,
                    Time1 = 'Parser.time'(P),
                    Lit3 = maps:get(text, Time1),
                    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed date-time: \"", (Lit3)/binary, "\"">>),
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
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed date: \"", (Lit4)/binary, "\"">>),
    #{text => Lit4, pos => Pos, {vbeam, type} => 'Date'}.

'Parser.time'(P) ->
    Lit = maps:get(lit, maps:get(tok, P)),
    Pos = 'Token.pos'(maps:get(tok, P)),
    case 'Parser.is_at'(P, bare) && ('string.starts_with'(Lit, <<"T">>) || 'string.starts_with'(Lit, <<"t">>)) of
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
    case !'u8.is_digit'(lists:nth(length(Lit6) - 1 + 1, Lit6)) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" expected a number as last occurrence in \"", (Lit6)/binary, "\" got \"", ('u8.ascii_str'(lists:nth(length(Lit6) - 1 + 1, Lit6)))/binary, "\"">>);
        false -> ok
    end,
    case maps:get(kind, maps:get(peek_tok, P)) == minus || maps:get(kind, maps:get(peek_tok, P)) == plus of
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
        false -> case maps:get(kind, maps:get(peek_tok, P)) == bare && (maps:get(lit, maps:get(peek_tok, P)) == <<"Z">> || maps:get(lit, maps:get(peek_tok, P)) == <<"z">>) of
            true -> begin
                'Parser.next'(P),
                Lit11 = maps:get(lit, maps:get(tok, P)),
                'Parser.expect'(P, bare)
            end;
            false -> ok
        end
    end,
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"parsed time: \"", (Lit11)/binary, "\"">>),
    #{text => Lit11, pos => Pos, {vbeam, type} => 'Time'}.

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
