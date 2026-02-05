-module('v.flag').
-export(['ParseMode__static__from'/1, 'Style__static__from'/1, 'FieldHints.is_empty'/1, 'FieldHints.has'/2, 'FieldHints.all'/2, 'FieldHints.set'/2, 'FieldHints.set_all'/1, 'FieldHints.clear'/2, 'FieldHints.clear_all'/1, 'FieldHints.toggle'/2, 'FieldHints__static__zero'/0, 'FieldHints__static__from'/1, 'Show.is_empty'/1, 'Show.has'/2, 'Show.all'/2, 'Show.set'/2, 'Show.set_all'/1, 'Show.clear'/2, 'Show.clear_all'/1, 'Show.toggle'/2, 'Show__static__zero'/0, 'Show__static__from'/1]).

'ParseMode__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'Style__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'FieldHints.is_empty'(E) ->
    todo == 0.

'FieldHints.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'FieldHints.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'FieldHints.set'(E, Flag_) ->
    % TODO: {*e = flag.FieldHints(int(*e) | (int(flag_)));}

'FieldHints.set_all'(E) ->
    % TODO: {*e = flag.FieldHints(0b1111111);}

'FieldHints.clear'(E, Flag_) ->
    % TODO: {*e = flag.FieldHints(int(*e) & ~(int(flag_)));}

'FieldHints.clear_all'(E) ->
    % TODO: {*e = flag.FieldHints(0);}

'FieldHints.toggle'(E, Flag_) ->
    % TODO: {*e = flag.FieldHints(int(*e) ^ (int(flag_)));}

'FieldHints__static__zero'() ->
    todo.

'FieldHints__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).

'Show.is_empty'(E) ->
    todo == 0.

'Show.has'(E, Flag_) ->
    (todo & (todo)) != 0.

'Show.all'(E, Flag_) ->
    (todo & (todo)) == todo.

'Show.set'(E, Flag_) ->
    % TODO: {*e = flag.Show(int(*e) | (int(flag_)));}

'Show.set_all'(E) ->
    % TODO: {*e = flag.Show(0b11111111);}

'Show.clear'(E, Flag_) ->
    % TODO: {*e = flag.Show(int(*e) & ~(int(flag_)));}

'Show.clear_all'(E) ->
    % TODO: {*e = flag.Show(0);}

'Show.toggle'(E, Flag_) ->
    % TODO: {*e = flag.Show(int(*e) ^ (int(flag_)));}

'Show__static__zero'() ->
    todo.

'Show__static__from'(Input) ->
    ,
    ,
    error(<<"invalid value">>).
