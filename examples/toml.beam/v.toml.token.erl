-module('v.toml.token').
-export(['Token.pos'/1, 'Kind__static__from'/1]).

'Token.pos'(Tok) ->
    #{len => length(Tok), line_nr => maps:get(line_nr, Tok) - 1, pos => maps:get(pos, Tok), col => maps:get(col, Tok) - 1, {vbeam, type} => 'Pos'}.

'Kind__static__from'(Input) ->
    error(<<"invalid value">>).
