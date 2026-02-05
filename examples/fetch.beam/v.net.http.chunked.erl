-module('v.net.http.chunked').
-export(['ChunkScanner.read_chunk_size'/1, unhex/1, 'ChunkScanner.skip_crlf'/1, 'ChunkScanner.read_chunk'/2, decode/1]).

'ChunkScanner.read_chunk_size'(S) ->
    N = todo,
    % TODO: for {
    N.

unhex(C) ->
    case todo <= C && C <= todo of
        true -> C - todo;
        false -> case todo <= C && C <= todo of
            true -> C - todo + 10;
            false -> case todo <= C && C <= todo of
                true -> C - todo + 10;
                false -> ok
            end
        end
    end,
    0.

'ChunkScanner.skip_crlf'(S) ->

'ChunkScanner.read_chunk'(S, Chunksize) ->
    Startpos = maps:get(pos, S),
    case maps:get(pos, S) > length(maps:get(text, S)) of
        true -> error(<<"invalid chunksize">>);
        false -> ok
    end,
    lists:nth(todo + 1, maps:get(text, S)).

decode(Text) ->
    Sb = new_builder(100),
    Cscanner = #{pos => 0, text => Text, {vbeam, type} => 'ChunkScanner'},
    % TODO: for {
    'ChunkScanner.skip_crlf'(Cscanner),
    'Builder.str'(Sb).
