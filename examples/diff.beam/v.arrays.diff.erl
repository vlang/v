-module('v.arrays.diff').
-export(['DiffContextFlag.is_empty'/1, 'DiffContextFlag.has'/2, 'DiffContextFlag.all'/2, 'DiffContextFlag.set'/2, 'DiffContextFlag.set_all'/1, 'DiffContextFlag.clear'/2, 'DiffContextFlag.clear_all'/1, 'DiffContextFlag.toggle'/2, 'DiffContextFlag__static__zero'/0, 'DiffContextFlag__static__from'/1]).

'DiffContextFlag.is_empty'(E) ->
    todo == 0.

'DiffContextFlag.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'DiffContextFlag.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'DiffContextFlag.set'(E, Flag_) ->
    % TODO: {*e = diff.DiffContextFlag(int(*e) | (int(flag_)));}

'DiffContextFlag.set_all'(E) ->
    % TODO: {*e = diff.DiffContextFlag(0b11);}

'DiffContextFlag.clear'(E, Flag_) ->
    % TODO: {*e = diff.DiffContextFlag(int(*e) & ~(int(flag_)));}

'DiffContextFlag.clear_all'(E) ->
    % TODO: {*e = diff.DiffContextFlag(0);}

'DiffContextFlag.toggle'(E, Flag_) ->
    % TODO: {*e = diff.DiffContextFlag(int(*e) ^ (int(flag_)));}

'DiffContextFlag__static__zero'() ->
    todo.

'DiffContextFlag__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).
