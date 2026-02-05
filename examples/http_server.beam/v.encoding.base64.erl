-module('v.encoding.base64').
-export([url_decode/1, url_decode_str/1, url_encode/1, url_encode_str/1, assemble64/8, assemble32/4]).
% TODO: const index = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 63, 62, 62, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 63, 0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51]int{};
% TODO: const ending_table = [0, 2, 1]int{};
% TODO: const enc_table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

url_decode(Data) ->
    Result = 'string.replace_each'(Data, [<<"-">>, <<"+">>, <<"_">>, <<"/">>]),
    case length(Result) % 4 of
        2 -> ok;
        3 -> ok;
        _ -> ok
    end,
    decode(Result).

url_decode_str(Data) ->
    Result = 'string.replace_each'(Data, [<<"-">>, <<"+">>, <<"_">>, <<"/">>]),
    case length(Result) % 4 of
        2 -> ok;
        3 -> ok;
        _ -> ok
    end,
    decode_str(Result).

url_encode(Data) ->
    'string.replace_each'(encode(Data), [<<"+">>, <<"-">>, <<"/">>, <<"_">>, <<"=">>, <<"">>]).

url_encode_str(Data) ->
    'string.replace_each'(encode_str(Data), [<<"+">>, <<"-">>, <<"/">>, <<"_">>, <<"=">>, <<"">>]).

assemble64(N1, N2, N3, N4, N5, N6, N7, N8) ->
    todo << 58 | todo << 52 | todo << 46 | todo << 40 | todo << 34 | todo << 28 | todo << 22 | todo << 16.

assemble32(N1, N2, N3, N4) ->
    todo << 26 | todo << 20 | todo << 14 | todo << 8.
