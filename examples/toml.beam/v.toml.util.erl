-module('v.toml.util').
-export([is_key_char/1, is_ascii_control_character/1, is_illegal_ascii_control_character/1, printdbg/2]).

is_key_char(C) ->
    'u8.is_letter'(C).

is_ascii_control_character(Byte_char) ->
    (Byte_char >= 0 && Byte_char <= 0x1f) || Byte_char == 0x7f.

is_illegal_ascii_control_character(Byte_char) ->
    Byte_char != 0x09 && is_ascii_control_character(Byte_char).

printdbg(Id, Message) ->
    eprintln(Id + <<" ">> + Message),
    ok.
