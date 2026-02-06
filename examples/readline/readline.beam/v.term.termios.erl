-module('v.term.termios').
-export([flag/1, invert/1, tcgetattr/2, tcsetattr/3, ioctl/3, set_state/2, 'Termios.disable_echo'/1]).

flag(Value) ->
    todo.

invert(Value) ->
    bnot todo.

tcgetattr(Fd, Termios_p) ->
    0.

tcsetattr(Fd, Optional_actions, Termios_p) ->
    0.

ioctl(Fd, Request, Arg) ->
    -1.

set_state(Fd, New_state) ->
    X = New_state,
    tcsetattr(0, 0, X).

'Termios.disable_echo'(T) ->
