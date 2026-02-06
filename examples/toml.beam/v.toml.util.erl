-module('v.toml.util').
-export([is_key_char/1, is_ascii_control_character/1, is_illegal_ascii_control_character/1, printdbg/2]).

is_key_char(C) ->
    'u8.is_letter'(C).

is_ascii_control_character(Byte_char) ->
    (Byte_char >= 0 andalso Byte_char =< 16#1f) orelse Byte_char == 16#7f.

is_illegal_ascii_control_character(Byte_char) ->
    Byte_char /= 16#09 andalso is_ascii_control_character(Byte_char).

printdbg(Id, Message) ->
    io:format(standard_error, "~s~n", [<<(<<(Id)/binary, (<<" ">>)/binary>>)/binary, (Message)/binary>>]),
    ok.
