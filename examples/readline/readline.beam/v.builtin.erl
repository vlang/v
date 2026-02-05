-module('v.builtin').
-export(['string.str'/1, 'string.len'/1, 'string.bytes'/1, 'string.runes'/1, 'string.repeat'/2, 'string.contains'/2, 'string.index'/2, 'string.index_'/2, 'string.split'/2, 'string.trim'/2, 'string.trim_space'/1, 'string.to_upper'/1, 'string.to_lower'/1, 'string.starts_with'/2, 'string.ends_with'/2, 'string.replace'/3, 'string.substr'/3, 'string.int'/1, 'string.i8'/1, 'string.i16'/1, 'string.i32'/1, 'string.i64'/1, 'string.u8'/1, 'string.u16'/1, 'string.u32'/1, 'string.u64'/1, 'string.f64'/1, 'string.f32'/1, 'string.=='/2, 'string.+'/2, 'string.contains_only'/2, 'string.is_capital'/1, 'string.all_before'/2, 'string.all_after'/2, 'string.all_after_last'/2, 'string.all_before_last'/2, 'string.last_index_u8'/2, 'string.last_index'/2, 'string.index_last_'/2, 'string.trim_space_right'/1, 'string.capitalize'/1, 'string.split_into_lines'/1, 'string.clone'/1, 'string.trim_right'/2, 'string.trim_left'/2, 'string.index_u8'/2, 'string.free'/1, 'string.replace_once'/3, 'string.count'/2, 'string.replace_each'/2, 'string.index_after'/3, 'string.index_after_'/3, 'string.index_any'/2, 'string.trim_string_left'/2, 'string.trim_string_right'/2, cstring_to_vstring/1, tos_clone/1, tos/2, tos2/1, tos3/1, tos4/1, tos5/1, 'string.contains_any'/2, 'string.bool'/1, 'string.match_glob'/2]).

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
        false -> ok
    end,
    Idx.

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
    case length(S) == 0 || length(Vals) == 0 of
        true -> 'string.clone'(S);
        false -> ok
    end,
    S.

'string.index_after'(S, P, Start) ->
    Idx = 'string.index_after_'(S, P, Start),
    case Idx == -1 of
        true -> todo;
        false -> ok
    end,
    Idx.

'string.index_after_'(S, P, Start) ->
    case length(P) > length(S) of
        true -> -1;
        false -> ok
    end,
    -1.

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
    S == <<"true">> || S == <<"1">>.

'string.match_glob'(S, Pattern) ->
    false.
