-module('v.io').
-export([new_buffered_reader/1, 'BufferedReader.read'/2, 'BufferedReader.free'/1, 'BufferedReader.fill_buffer'/1, 'BufferedReader.needs_fill'/1, 'BufferedReader.end_of_stream'/1, 'BufferedReader.read_line'/2, new_buffered_writer/1, 'BufferedWriter.reset'/1, 'BufferedWriter.buffered'/1, 'BufferedWriter.flush'/1, 'BufferedWriter.available'/1, 'BufferedWriter.write'/2, cp/3, new_multi_writer/1, 'MultiWriter.write'/2, 'NotExpected.msg'/1, 'NotExpected.code'/1, read_all/1, read_any/1, 'ReaderWriterImpl.read'/2, 'ReaderWriterImpl.write'/2, make_readerwriter/2]).

new_buffered_reader(O) ->
    case maps:get(cap, O) =< 0 of
        true -> erlang:error({panic, <<"new_buffered_reader should be called with a positive `cap`">>});
        false -> ok
    end,
    R = #{reader => maps:get(reader, O), buf => [], offset => 0, mfails => maps:get(retries, O), {vbeam, type} => 'BufferedReader'},
    R.

'BufferedReader.read'(R, Buf) ->
    case maps:get(end_of_stream, R) of
        true -> todo;
        false -> begin
            case 'BufferedReader.needs_fill'(R) of
                true -> case not 'BufferedReader.fill_buffer'(R) of
                    true -> todo;
                    false -> ok
                end;
                false -> ok
            end,
            Read = copy(Buf, lists:nth(todo + 1, maps:get(buf, R))),
            case Read == 0 of
                true -> todo;
                false -> begin
                    Read
                end
                        end
        end
        end.

'BufferedReader.free'(R) ->
    todo,
    ok.

'BufferedReader.fill_buffer'(R) ->
    case maps:get(end_of_stream, R) of
        true -> true;
        false -> begin
            case length(R) == 0 of
                true -> todo;
                false -> ok
            end,
            case maps:get(fails, R) >= maps:get(mfails, R) of
                true -> false;
                false -> true
                        end
        end
        end.

'BufferedReader.needs_fill'(R) ->
    maps:get(offset, R) >= length(R).

'BufferedReader.end_of_stream'(R) ->
    maps:get(end_of_stream, R).

'BufferedReader.read_line'(R, Config) ->
    case maps:get(end_of_stream, R) of
        true -> todo;
        false -> begin
            Line = [],
            % TODO: unhandled stmt type
            todo
        end
        end.

new_buffered_writer(O) ->
    case maps:get(cap, O) < 1 of
        true -> error(<<"`o.cap` must be a positive integer">>);
        false -> #{buf => [], wr => maps:get(writer, O), {vbeam, type} => 'BufferedWriter'}
        end.

'BufferedWriter.reset'(B) ->
    Cap = length(maps:get(buf, B)),

'BufferedWriter.buffered'(B) ->
    maps:get(n, B).

'BufferedWriter.flush'(B) ->
    case 'BufferedWriter.buffered'(B) == 0 of
        true -> ok;
        false -> begin
            N = 'Writer.write'(maps:get(wr, B), lists:nth(todo + 1, maps:get(buf, B))),
            case N < maps:get(n, B) of
                true -> error(<<"Writer accepted less bytes than expected without returning any explicit error.">>);
                false -> begin
                    ok
                end
                        end
        end
        end.

'BufferedWriter.available'(B) ->
    length(maps:get(buf, B)) - maps:get(n, B).

'BufferedWriter.write'(B, Src) ->
    P = Src,
    Nn = 0,
    % TODO: unhandled stmt type
    N = copy(lists:nth(todo + 1, maps:get(buf, B)), P),
    Nn1 = N,
    Nn1.

cp(Src, Dst, Params) ->
    Buf = [],
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    ok.

new_multi_writer(Writers) ->
    #{writers => Writers, {vbeam, type} => 'MultiWriter'}.

'MultiWriter.write'(M, Buf) ->
    lists:foreach(fun(W) ->
        N = 'Writer.write'(W, Buf),
        case N /= length(Buf) of
            true -> error(<<"io: incomplete write to writer of MultiWriter">>);
            false -> ok
        end,
        ok
    end, maps:get(writers, M)),
    length(Buf).

'NotExpected.msg'(Err) ->
    maps:get(cause, Err).

'NotExpected.code'(Err) ->
    maps:get(code, Err).

read_all(Config) ->
    R = maps:get(reader, Config),
    Read_till_eof = maps:get(read_to_end_of_stream, Config),
    B = [],
    Read = 0,
    % TODO: unhandled stmt type
    lists:nth(todo + 1, B).

read_any(R) ->
    B = [],
    Read = 0,
    % TODO: unhandled stmt type
    lists:nth(todo + 1, B).

'ReaderWriterImpl.read'(R, Buf) ->
    'Reader.read'(maps:get(r, R), Buf).

'ReaderWriterImpl.write'(R, Buf) ->
    'Writer.write'(maps:get(w, R), Buf).

make_readerwriter(R, W) ->
    #{r => R, w => W, {vbeam, type} => 'ReaderWriterImpl'}.
