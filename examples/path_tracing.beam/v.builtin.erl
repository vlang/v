-module('v.builtin').
-export(['__new_array'/3, '__new_array_with_default'/4, '__new_array_with_multi_default'/4, '__new_array_with_array_default'/5, 'array.len'/1, 'array.first'/1, 'array.last'/1, 'array.reverse'/1, 'array.free'/1, 'array.clone'/1, 'array.clear'/1, 'array.trim'/2, 'array.drop'/2, '[]string.join'/2, '[]u8.bytestr'/1, '[]u8.hex'/1, '[]string.contains'/2, 'array.filter'/2, 'array.map'/2, 'array.sort'/1, 'array.sort_with_compare'/2, 'array.push'/2, 'array.push_many'/3, 'array.grow_len'/2, 'array.grow_cap'/2, 'array.ensure_cap'/2, '[]u8.delete_last'/1, '[]u64.delete_last'/1, '[]int.delete_last'/1, '[]string.delete_last'/1, '[]u8.byterune'/1, '[]u8.utf8_to_utf32'/1, 'array.pointers'/1, 'array.get_unsafe'/2, println/1, print/1, eprintln/1, eprint/1, exit/1, panic/1, flush_stdout/0, flush_stderr/0, unbuffer_stdout/0, print_backtrace/0, arguments/0, panic_n/2, panic_n2/3, panic_n3/4, vmemcpy/3, vmemmove/3, vmemset/3, vcalloc/1, isnil/1, at_exit/1, copy/2, vmemcmp/3, malloc/1, malloc_noscan/1, free/1, f32_abs/1, f64_abs/1, f32_min/2, f32_max/2, f64_min/2, f64_max/2, 'f32.str'/1, 'f64.str'/1, 'f64.strsci'/2, 'f64.strlong'/1, 'f32.eq_epsilon'/2, 'f64.eq_epsilon'/2, 'i8.str'/1, 'i16.str'/1, 'u16.str'/1, 'int.str'/1, 'i32.str'/1, 'u32.str'/1, 'i64.str'/1, 'u64.str'/1, 'bool.str'/1, 'u8.str'/1, 'int literal.str'/1, 'u64.hex'/1, 'i64.hex'/1, 'u32.hex'/1, 'int.hex'/1, 'u16.hex'/1, 'i16.hex'/1, 'u8.hex'/1, 'i8.hex'/1, 'int literal.hex'/1, 'int.hex2'/1, 'u64.hex_full'/1, 'u8.is_capital'/1, 'u8.is_letter'/1, 'u8.is_digit'/1, 'u8.is_alnum'/1, 'u8.is_space'/1, 'u8.ascii_str'/1, 'u8.repeat'/2, int_min/2, int_max/2, ptr_str/1, 'u8.vstring_with_len'/2, 'u8.vstring_literal_with_len'/2, 'u8.vbytes'/2, 'u8.is_hex_digit'/1, 'u8.str_escaped'/1, 'VAssertMetaInfo.free'/1, '__print_assert_failure'/1, 'Error.msg'/1, 'Error.code'/1, 'MessageError.str'/1, 'MessageError.msg'/1, 'MessageError.code'/1, 'IError.str'/1, error/1, error_with_code/2, 'None__.str'/1, 'none.str'/1, 'rune.str'/1, '[]rune.string'/1, 'rune.repeat'/2, 'rune.bytes'/1, 'rune.length_in_bytes'/1, 'rune.to_upper'/1, 'rune.to_lower'/1, 'rune.to_title'/1, 'string.str'/1, 'string.len'/1, 'string.bytes'/1, 'string.runes'/1, 'string.repeat'/2, 'string.contains'/2, 'string.index'/2, 'string.index_'/2, 'string.split'/2, 'string.trim'/2, 'string.trim_space'/1, 'string.to_upper'/1, 'string.to_lower'/1, 'string.starts_with'/2, 'string.ends_with'/2, 'string.replace'/3, 'string.substr'/3, 'string.substr_unsafe'/3, 'string.int'/1, 'string.i8'/1, 'string.i16'/1, 'string.i32'/1, 'string.i64'/1, 'string.u8'/1, 'string.u16'/1, 'string.u32'/1, 'string.u64'/1, 'string.f64'/1, 'string.f32'/1, 'string.=='/2, 'string.+'/2, 'string.contains_only'/2, 'string.is_capital'/1, 'string.all_before'/2, 'string.all_after'/2, 'string.all_after_last'/2, 'string.all_before_last'/2, 'string.last_index_u8'/2, 'string.last_index'/2, 'string.index_last_'/2, 'string.trim_space_right'/1, 'string.capitalize'/1, 'string.split_into_lines'/1, 'string.clone'/1, 'string.trim_right'/2, 'string.trim_left'/2, 'string.index_u8'/2, 'string.free'/1, 'string.replace_once'/3, 'string.count'/2, 'string.replace_each'/2, 'string.index_after'/3, 'string.index_after_'/3, 'string.index_any'/2, 'string.trim_string_left'/2, 'string.trim_string_right'/2, cstring_to_vstring/1, tos_clone/1, tos/2, tos2/1, tos3/1, tos4/1, tos5/1, 'string.contains_any'/2, 'string.bool'/1, 'string.match_glob'/2, 'string.split_once'/2, 'string.split_nth'/3, utf8_str_visible_length/1, utf8_char_len/1, utf8_getchar/2, utf32_to_str/1, utf32_to_str_no_malloc/1, utf32_decode_to_buffer/2, 'AttributeKind__static__from'/1, 'ChanState__static__from'/1]).

'__new_array'(Mylen, Cap, Elm_size) ->
    #{len => Mylen, cap => case Cap < Mylen of
        true -> Mylen;
        false -> Cap
    end, element_size => Elm_size, {vbeam, type} => 'array'}.

'__new_array_with_default'(Mylen, Cap, Elm_size, Val) ->
    #{len => Mylen, cap => case Cap < Mylen of
        true -> Mylen;
        false -> Cap
    end, element_size => Elm_size, {vbeam, type} => 'array'}.

'__new_array_with_multi_default'(Mylen, Cap, Elm_size, Val) ->
    #{len => Mylen, cap => case Cap < Mylen of
        true -> Mylen;
        false -> Cap
    end, element_size => Elm_size, {vbeam, type} => 'array'}.

'__new_array_with_array_default'(Mylen, Cap, Elm_size, Val, Depth) ->
    #{len => Mylen, cap => case Cap < Mylen of
        true -> Mylen;
        false -> Cap
    end, element_size => Elm_size, {vbeam, type} => 'array'}.

'array.len'(A) ->
    length(A).

'array.first'(A) ->
    todo.

'array.last'(A) ->
    todo.

'array.reverse'(A) ->
    A.

'array.free'(A) ->
    ok.

'array.clone'(A) ->
    #{data => maps:get(data, A), offset => maps:get(offset, A), len => length(A), cap => maps:get(cap, A), element_size => maps:get(element_size, A), {vbeam, type} => 'array'}.

'array.clear'(A) ->
    ok.

'array.trim'(A, Index) ->
    ok.

'array.drop'(A, Num) ->
    A.

'[]string.join'(A, Sep) ->
    <<"">>.

'[]u8.bytestr'(A) ->
    <<"">>.

'[]u8.hex'(A) ->
    <<"">>.

'[]string.contains'(A, Val) ->
    false.

'array.filter'(A, Pred) ->
    A.

'array.map'(A, Transform) ->
    A.

'array.sort'(A) ->
    ok.

'array.sort_with_compare'(A, Cmp) ->
    ok.

'array.push'(A, Val) ->
    ok.

'array.push_many'(A, Val, Size) ->
    ok.

'array.grow_len'(A, Amount) ->
    ok.

'array.grow_cap'(A, Amount) ->
    ok.

'array.ensure_cap'(A, Required) ->
    ok.

'[]u8.delete_last'(A) ->
    case length(A) == 0 of
        true -> 0;
        false -> begin
            Last = lists:nth(length(A) - 1 + 1, A),
            A = '[]u8.clone'(lists:nth(todo + 1, A)),
            Last
        end
        end.

'[]u64.delete_last'(A) ->
    case length(A) == 0 of
        true -> 0;
        false -> begin
            Last = lists:nth(length(A) - 1 + 1, A),
            A = '[]u64.clone'(lists:nth(todo + 1, A)),
            Last
        end
        end.

'[]int.delete_last'(A) ->
    case length(A) == 0 of
        true -> 0;
        false -> begin
            Last = lists:nth(length(A) - 1 + 1, A),
            A = '[]int.clone'(lists:nth(todo + 1, A)),
            Last
        end
        end.

'[]string.delete_last'(A) ->
    case length(A) == 0 of
        true -> <<"">>;
        false -> begin
            Last = lists:nth(length(A) - 1 + 1, A),
            A = '[]string.clone'(lists:nth(todo + 1, A)),
            Last
        end
        end.

'[]u8.byterune'(B) ->
    case length(B) == 0 of
        true -> error(<<"empty byte array">>);
        false -> 
            case length(B) > 4 of
                true -> error(<<"more than 4 bytes">>);
                false -> todo
                        end
                end.

'[]u8.utf8_to_utf32'(B) ->
    case length(B) == 0 of
        true -> error(<<"empty byte array">>);
        false -> 
            case length(B) > 4 of
                true -> error(<<"more than 4 bytes">>);
                false -> todo
                        end
                end.

'array.pointers'(A) ->
    Res = [],
    lists:foreach(fun(I) ->
        todo,
        ok
    end, lists:seq(0, length(A) - 1)),
    Res.

'array.get_unsafe'(A, I) ->
    todo.

println(S) ->
    ok.

print(S) ->
    ok.

eprintln(S) ->
    ok.

eprint(S) ->
    ok.

exit(Code) ->
    % TODO: unhandled stmt type
    ok
panic(S) ->
    % TODO: unhandled stmt type
    ok
flush_stdout() ->
    ok.

flush_stderr() ->
    ok.

unbuffer_stdout() ->
    ok.

print_backtrace() ->
    ok.

arguments() ->
    [].

panic_n(S, N) ->
    panic(S),
    % TODO: unhandled stmt type
    ok
panic_n2(S, Number1, Number2) ->
    panic(S),
    % TODO: unhandled stmt type
    ok
panic_n3(S, Number1, Number2, Number3) ->
    panic(S),
    % TODO: unhandled stmt type
    ok
vmemcpy(Dest, Const_src, N) ->
    Dest.

vmemmove(Dest, Const_src, N) ->
    Dest.

vmemset(S, C, N) ->
    S.

vcalloc(N) ->
    todo.

isnil(V) ->
    V == 0.

at_exit(Cb) ->
    ok.

copy(Dst, Src) ->
    Min = case length(Dst) < length(Src) of
        true -> length(Dst);
        false -> length(Src)
    end,
    Min.

vmemcmp(Const_s1, Const_s2, N) ->
    0.

malloc(Sz) ->
    todo.

malloc_noscan(Sz) ->
    todo.

free(Ptr) ->
    ok.

f32_abs(A) ->
    case A < 0 of
        true -> -A;
        false -> A
    end.

f64_abs(A) ->
    case A < 0 of
        true -> -A;
        false -> A
    end.

f32_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

f32_max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

f64_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

f64_max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

'f32.str'(F) ->
    <<"">>.

'f64.str'(F) ->
    <<"">>.

'f64.strsci'(F, Decimals) ->
    <<"">>.

'f64.strlong'(F) ->
    <<"">>.

'f32.eq_epsilon'(A, B) ->
    Hi = f32_max(f32_abs(A), f32_abs(B)),
    Delta = f32_abs(A - B),
    case Hi > todo of
        true -> Delta =< Hi * todo;
        false -> todo * Delta =< Hi
    end.

'f64.eq_epsilon'(A, B) ->
    Hi = f64_max(f64_abs(A), f64_abs(B)),
    Delta = f64_abs(A - B),
    case Hi > todo of
        true -> Delta =< Hi * todo;
        false -> todo * Delta =< Hi
    end.

'i8.str'(N) ->
    integer_to_binary(todo).

'i16.str'(N) ->
    integer_to_binary(todo).

'u16.str'(N) ->
    integer_to_binary(todo).

'int.str'(N) ->
    <<"">>.

'i32.str'(N) ->
    integer_to_binary(todo).

'u32.str'(N) ->
    integer_to_binary(todo).

'i64.str'(N) ->
    <<"">>.

'u64.str'(N) ->
    <<"">>.

'bool.str'(B) ->
    case B of
        true -> <<"true">>;
        false -> <<"false">>
        end.

'u8.str'(B) ->
    integer_to_binary(todo).

'int literal.str'(N) ->
    integer_to_binary(todo).

'u64.hex'(Nn) ->
    <<"">>.

'i64.hex'(Nn) ->
    'u64.hex'(todo).

'u32.hex'(Nn) ->
    'u64.hex'(todo).

'int.hex'(Nn) ->
    'u32.hex'(todo).

'u16.hex'(Nn) ->
    'u64.hex'(todo).

'i16.hex'(Nn) ->
    'u16.hex'(todo).

'u8.hex'(Nn) ->
    'u64.hex'(todo).

'i8.hex'(Nn) ->
    'u64.hex'(todo).

'int literal.hex'(Nn) ->
    'u64.hex'(todo).

'int.hex2'(N) ->
    <<(<<"0x">>)/binary, ('int.hex'(N))/binary>>.

'u64.hex_full'(Nn) ->
    <<"">>.

'u8.is_capital'(C) ->
    C >= todo andalso C =< todo.

'u8.is_letter'(C) ->
    (C >= todo andalso C =< todo) orelse (C >= todo andalso C =< todo).

'u8.is_digit'(C) ->
    C >= todo andalso C =< todo.

'u8.is_alnum'(C) ->
    'u8.is_letter'(C) orelse 'u8.is_digit'(C).

'u8.is_space'(C) ->
    lists:member(C, [todo, todo, todo, todo, todo, todo]).

'u8.ascii_str'(B) ->
    <<"">>.

'u8.repeat'(B, Count) ->
    <<"">>.

int_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

int_max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

ptr_str(Ptr) ->
    <<"<ptr>">>.

'u8.vstring_with_len'(B, Len) ->
    <<"">>.

'u8.vstring_literal_with_len'(B, Len) ->
    <<"">>.

'u8.vbytes'(B, Len) ->
    [].

'u8.is_hex_digit'(C) ->
    (C >= todo andalso C =< todo) orelse (C >= todo andalso C =< todo) orelse (C >= todo andalso C =< todo).

'u8.str_escaped'(B) ->
    case B of
        todo -> <<"\\\\n">>;
        todo -> <<"\\\\r">>;
        todo -> <<"\\\\t">>;
        todo -> <<"\\\\\\\\">>;
        todo -> <<"\\\\\"">>;
        todo -> <<"\\\\'">>;
        _ -> begin
            case B < 32 orelse B > 126 of
                true -> <<(<<"\\\\x">>)/binary, ('u8.hex'(B))/binary>>;
                false -> ok
            end,
            'u8.ascii_str'(B)
        end
    end.

'VAssertMetaInfo.free'(Ami) ->
    ok.

'__print_assert_failure'(I) ->
    eprintln(<<(maps:get(fpath, I))/binary, ":", (integer_to_binary(maps:get(line_nr, I) + 1))/binary, ": FAIL: fn ", (maps:get(fn_name, I))/binary, ": assert ", (maps:get(src, I))/binary>>),
    case length(maps:get(op, I)) > 0 andalso maps:get(op, I) /= <<"call">> of
        true -> begin
            case maps:get(llabel, I) == maps:get(lvalue, I) of
                true -> eprintln(<<"   left value: ", (maps:get(llabel, I))/binary>>);
                false -> eprintln(<<"   left value: ", (maps:get(llabel, I))/binary, " = ", (maps:get(lvalue, I))/binary>>)
            end,
            case maps:get(rlabel, I) == maps:get(rvalue, I) of
                true -> eprintln(<<"  right value: ", (maps:get(rlabel, I))/binary>>);
                false -> eprintln(<<"  right value: ", (maps:get(rlabel, I))/binary, " = ", (maps:get(rvalue, I))/binary>>)
            end
        end;
        false -> ok
    end,
    case maps:get(has_msg, I) of
        true -> eprintln(<<"      message: ", (maps:get(message, I))/binary>>);
        false -> ok
    end.

'Error.msg'(Err) ->
    <<"">>.

'Error.code'(Err) ->
    0.

'MessageError.str'(Err) ->
    case maps:get(code, Err) > 0 of
        true -> <<(maps:get(msg, Err))/binary, "; code: ", (integer_to_binary(maps:get(code, Err)))/binary>>;
        false -> maps:get(msg, Err)
        end.

'MessageError.msg'(Err) ->
    maps:get(msg, Err).

'MessageError.code'(Err) ->
    maps:get(code, Err).

'IError.str'(Err) ->
    'IError.msg'(Err).

error(Message) ->
    #{msg => Message, {vbeam, type} => 'MessageError'}.

error_with_code(Message, Code) ->
    #{msg => Message, code => Code, {vbeam, type} => 'MessageError'}.

'None__.str'(_) ->
    <<"none">>.

'none.str'(_) ->
    <<"none">>.

'rune.str'(C) ->
    <<"">>.

'[]rune.string'(Ra) ->
    <<"">>.

'rune.repeat'(C, Count) ->
    <<"">>.

'rune.bytes'(C) ->
    [].

'rune.length_in_bytes'(C) ->
    0.

'rune.to_upper'(C) ->
    C.

'rune.to_lower'(C) ->
    C.

'rune.to_title'(C) ->
    C.

'string.str'(S) ->
    S.

'string.len'(S) ->
    length(S).

'string.bytes'(S) ->
    [].

'string.runes'(S) ->
    [].

'string.repeat'(S, Count) ->
    <<"">>.

'string.contains'(S, Substr) ->
    false.

'string.index'(S, P) ->
    Idx = 'string.index_'(S, P),
    case Idx == -1 of
        true -> todo;
        false -> Idx
        end.

'string.index_'(S, P) ->
    -1.

'string.split'(S, Delim) ->
    [].

'string.trim'(S, Cutset) ->
    <<"">>.

'string.trim_space'(S) ->
    <<"">>.

'string.to_upper'(S) ->
    <<"">>.

'string.to_lower'(S) ->
    <<"">>.

'string.starts_with'(S, Prefix) ->
    false.

'string.ends_with'(S, Suffix) ->
    false.

'string.replace'(S, Old, New_) ->
    <<"">>.

'string.substr'(S, Start, End) ->
    <<"">>.

'string.substr_unsafe'(S, Start, End) ->
    <<"">>.

'string.int'(S) ->
    0.

'string.i8'(S) ->
    0.

'string.i16'(S) ->
    0.

'string.i32'(S) ->
    0.

'string.i64'(S) ->
    0.

'string.u8'(S) ->
    0.

'string.u16'(S) ->
    0.

'string.u32'(S) ->
    0.

'string.u64'(S) ->
    0.

'string.f64'(S) ->
    0.0.

'string.f32'(S) ->
    0.0.

'string.=='(S, Other) ->
    false.

'string.+'(S, Other) ->
    <<"">>.

'string.contains_only'(S, Chars) ->
    false.

'string.is_capital'(S) ->
    false.

'string.all_before'(S, Sub) ->
    <<"">>.

'string.all_after'(S, Sub) ->
    <<"">>.

'string.all_after_last'(S, Sub) ->
    <<"">>.

'string.all_before_last'(S, Sub) ->
    <<"">>.

'string.last_index_u8'(S, C) ->
    -1.

'string.last_index'(S, Needle) ->
    todo.

'string.index_last_'(S, Needle) ->
    -1.

'string.trim_space_right'(S) ->
    S.

'string.capitalize'(S) ->
    S.

'string.split_into_lines'(S) ->
    [].

'string.clone'(S) ->
    S.

'string.trim_right'(S, Chars) ->
    S.

'string.trim_left'(S, Chars) ->
    S.

'string.index_u8'(S, C) ->
    -1.

'string.free'(S) ->
    ok.

'string.replace_once'(S, Old, New_) ->
    S.

'string.count'(S, Substr) ->
    0.

'string.replace_each'(S, Vals) ->
    case length(S) == 0 orelse length(Vals) == 0 of
        true -> 'string.clone'(S);
        false -> S
        end.

'string.index_after'(S, P, Start) ->
    Idx = 'string.index_after_'(S, P, Start),
    case Idx == -1 of
        true -> todo;
        false -> Idx
        end.

'string.index_after_'(S, P, Start) ->
    case length(P) > length(S) of
        true -> -1;
        false -> -1
        end.

'string.index_any'(S, Chars) ->
    -1.

'string.trim_string_left'(S, Str) ->
    S.

'string.trim_string_right'(S, Str) ->
    S.

cstring_to_vstring(Const_s) ->
    <<"">>.

tos_clone(Const_s) ->
    <<"">>.

tos(S, Len) ->
    <<"">>.

tos2(S) ->
    <<"">>.

tos3(S) ->
    <<"">>.

tos4(S) ->
    <<"">>.

tos5(S) ->
    <<"">>.

'string.contains_any'(S, Chars) ->
    false.

'string.bool'(S) ->
    S == <<"true">> orelse S == <<"1">>.

'string.match_glob'(S, Pattern) ->
    false.

'string.split_once'(S, Delim) ->
    Idx = 'string.index_'(S, Delim),
    case Idx == -1 of
        true -> todo;
        false -> 'string.substr'(S, 0, Idx)
        end.

'string.split_nth'(S, Delim, N) ->
    [].

utf8_str_visible_length(S) ->
    length(S).

utf8_char_len(B) ->
    case B band 16#80 == 0 of
        true -> 1;
        false -> case B band 16#e0 == 16#c0 of
            true -> 2;
            false -> case B band 16#f0 == 16#e0 of
                true -> 3;
                false -> case B band 16#f8 == 16#f0 of
                    true -> 4;
                    false -> ok
                end
            end
        end
    end,
    1.

utf8_getchar(S, Index) ->
    todo.

utf32_to_str(Code) ->
    <<"">>.

utf32_to_str_no_malloc(Code) ->
    utf32_to_str(Code).

utf32_decode_to_buffer(S, Buf) ->
    0.

'AttributeKind__static__from'(Input) ->
    error(<<"invalid value">>).

'ChanState__static__from'(Input) ->
    error(<<"invalid value">>).
