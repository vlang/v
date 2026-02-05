-module('v.os.notify').
-export(['FdEventType.is_empty'/1, 'FdEventType.has'/2, 'FdEventType.all'/2, 'FdEventType.set'/2, 'FdEventType.set_all'/1, 'FdEventType.clear'/2, 'FdEventType.clear_all'/1, 'FdEventType.toggle'/2, 'FdEventType__static__zero'/0, 'FdEventType__static__from'/1, 'FdConfigFlags.is_empty'/1, 'FdConfigFlags.has'/2, 'FdConfigFlags.all'/2, 'FdConfigFlags.set'/2, 'FdConfigFlags.set_all'/1, 'FdConfigFlags.clear'/2, 'FdConfigFlags.clear_all'/1, 'FdConfigFlags.toggle'/2, 'FdConfigFlags__static__zero'/0, 'FdConfigFlags__static__from'/1]).

'FdEventType.is_empty'(E) ->
    todo == 0.

'FdEventType.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'FdEventType.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'FdEventType.set'(E, Flag_) ->
    % TODO: {*e = notify.FdEventType(int(*e) | (int(flag_)));}

'FdEventType.set_all'(E) ->
    % TODO: {*e = notify.FdEventType(0b111111);}

'FdEventType.clear'(E, Flag_) ->
    % TODO: {*e = notify.FdEventType(int(*e) & ~(int(flag_)));}

'FdEventType.clear_all'(E) ->
    % TODO: {*e = notify.FdEventType(0);}

'FdEventType.toggle'(E, Flag_) ->
    % TODO: {*e = notify.FdEventType(int(*e) ^ (int(flag_)));}

'FdEventType__static__zero'() ->
    todo.

'FdEventType__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'FdConfigFlags.is_empty'(E) ->
    todo == 0.

'FdConfigFlags.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'FdConfigFlags.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'FdConfigFlags.set'(E, Flag_) ->
    % TODO: {*e = notify.FdConfigFlags(int(*e) | (int(flag_)));}

'FdConfigFlags.set_all'(E) ->
    % TODO: {*e = notify.FdConfigFlags(0b1111);}

'FdConfigFlags.clear'(E, Flag_) ->
    % TODO: {*e = notify.FdConfigFlags(int(*e) & ~(int(flag_)));}

'FdConfigFlags.clear_all'(E) ->
    % TODO: {*e = notify.FdConfigFlags(0);}

'FdConfigFlags.toggle'(E, Flag_) ->
    % TODO: {*e = notify.FdConfigFlags(int(*e) ^ (int(flag_)));}

'FdConfigFlags__static__zero'() ->
    todo.

'FdConfigFlags__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).
