-module('v.term.ui').
-export(['KeyCode__static__from'/1, 'Direction__static__from'/1, 'MouseButton__static__from'/1, 'EventType__static__from'/1, 'Modifiers.is_empty'/1, 'Modifiers.has'/2, 'Modifiers.all'/2, 'Modifiers.set'/2, 'Modifiers.set_all'/1, 'Modifiers.clear'/2, 'Modifiers.clear_all'/1, 'Modifiers.toggle'/2, 'Modifiers__static__zero'/0, 'Modifiers__static__from'/1]).

'KeyCode__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'Direction__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'MouseButton__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'EventType__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'Modifiers.is_empty'(E) ->
    todo == 0.

'Modifiers.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'Modifiers.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'Modifiers.set'(E, Flag_) ->
    % TODO: {*e = ui.Modifiers(int(*e) | (int(flag_)));}

'Modifiers.set_all'(E) ->
    % TODO: {*e = ui.Modifiers(0b111);}

'Modifiers.clear'(E, Flag_) ->
    % TODO: {*e = ui.Modifiers(int(*e) & ~(int(flag_)));}

'Modifiers.clear_all'(E) ->
    % TODO: {*e = ui.Modifiers(0);}

'Modifiers.toggle'(E, Flag_) ->
    % TODO: {*e = ui.Modifiers(int(*e) ^ (int(flag_)));}

'Modifiers__static__zero'() ->
    todo.

'Modifiers__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).
