-module('v.os.notify').
-export([new/0, 'BeamFdNotifier.add'/4, 'BeamFdNotifier.modify'/4, 'BeamFdNotifier.remove'/2, 'BeamFdNotifier.wait'/2, 'BeamFdNotifier.close'/1, 'FdEventType.is_empty'/1, 'FdEventType.has'/2, 'FdEventType.all'/2, 'FdEventType.set'/2, 'FdEventType.set_all'/1, 'FdEventType.clear'/2, 'FdEventType.clear_all'/1, 'FdEventType.toggle'/2, 'FdEventType__static__zero'/0, 'FdEventType__static__from'/1, 'FdConfigFlags.is_empty'/1, 'FdConfigFlags.has'/2, 'FdConfigFlags.all'/2, 'FdConfigFlags.set'/2, 'FdConfigFlags.set_all'/1, 'FdConfigFlags.clear'/2, 'FdConfigFlags.clear_all'/1, 'FdConfigFlags.toggle'/2, 'FdConfigFlags__static__zero'/0, 'FdConfigFlags__static__from'/1]).

new() ->
    #{fds => [], {vbeam, type} => 'BeamFdNotifier'}.

'BeamFdNotifier.add'(N, Fd, Events, Conf) ->
    maps:get(fds, N) bsl Fd,
    ok.

'BeamFdNotifier.modify'(N, Fd, Events, Conf) ->
    ok.

'BeamFdNotifier.remove'(N, Fd) ->
    ok.

'BeamFdNotifier.wait'(N, Timeout) ->
    [].

'BeamFdNotifier.close'(N) ->
    '[]int.clear'(maps:get(fds, N)),
    ok.

'FdEventType.is_empty'(E) ->
    todo == 0.

'FdEventType.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'FdEventType.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'FdEventType.set'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdEventType.set_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FdEventType.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdEventType.clear_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FdEventType.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdEventType__static__zero'() ->
    todo.

'FdEventType__static__from'(Input) ->
    error(<<"invalid value">>).

'FdConfigFlags.is_empty'(E) ->
    todo == 0.

'FdConfigFlags.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'FdConfigFlags.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'FdConfigFlags.set'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdConfigFlags.set_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FdConfigFlags.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdConfigFlags.clear_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FdConfigFlags.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FdConfigFlags__static__zero'() ->
    todo.

'FdConfigFlags__static__from'(Input) ->
    error(<<"invalid value">>).
