-module('v.term.termios').
-export([flag/1, invert/1, tcgetattr/2, tcsetattr/3, ioctl/3, set_state/2, 'Termios.disable_echo'/1]).
% TODO: const cclen = 20;
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: const tcsanow = 0;
% TODO: const tcsadrain = 1;
% TODO: const tcsaflush = 2;
% TODO: const brkint = 0x0002;
% TODO: const icrnl = 0x0100;
% TODO: const inpck = 0x0010;
% TODO: const istrip = 0x0020;
% TODO: const ixon = 0x0400;
% TODO: const cs8 = 0x0300;
% TODO: const echo = 0x0008;
% TODO: const icanon = 0x0002;
% TODO: const iexten = 0x8000;
% TODO: const isig = 0x0001;
% TODO: const vmin = 6;
% TODO: const vtime = 5;
% TODO: const tiocgwinsz = 0x5413;

flag(Value) ->
    todo.

invert(Value) ->
    ~todo.

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
