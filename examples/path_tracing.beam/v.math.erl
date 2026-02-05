-module('v.math').
-export([f32_bits/1, f32_from_bits/1, f64_bits/1, f64_from_bits/1, with_set_low_word/2, with_set_high_word/2, get_high_word/1]).

f32_bits(F) ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    todo.

f32_from_bits(B) ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    todo.

f64_bits(F) ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    todo.

f64_from_bits(B) ->
    % TODO: [unhandled stmt str type: v.ast.EmptyStmt ]
    todo.

with_set_low_word(F, Lo) ->
    Tmp = f64_bits(F),
    Tmp1 = 0xffffffff00000000,
    Tmp2 = todo,
    f64_from_bits(Tmp2).

with_set_high_word(F, Hi) ->
    Tmp = f64_bits(F),
    Tmp1 = 0x00000000ffffffff,
    Tmp2 = todo << 32,
    f64_from_bits(Tmp2).

get_high_word(F) ->
    todo.
