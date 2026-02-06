-module('v.main').
-export([main/0, translate/3]).

main() ->
    Dest_lang = case length(init:get_plain_arguments()) > 1 of
        true -> lists:nth(2, init:get_plain_arguments());
        false -> <<"en">>
    end,
    Rnd = f32(),
    Url = <<"https://api.caiyunapp.com/v2.5/96Ly7wgKGq6FhllM/116.391912,40.010711/weather.jsonp?hourlysteps=120&random=", (float_to_binary(Rnd))/binary>>,
    Resp = fetch(#{url => Url, {vbeam, type} => 'FetchConfig'}),
    Weather = decode(maps:get(body, Resp), #{{vbeam, type} => 'DecoderOptions'}),
    lists:foreach(fun(Ch) ->
        vbeam_io:println(<<(maps:get(lang, Weather))/binary, ": ", (Ch)/binary>>),
        ok.
        T = translate(Ch, maps:get(lang, Weather), Dest_lang),
        vbeam_io:println(<<(Dest_lang)/binary, ": ", (T)/binary>>),
        ok.
        ok
    end, [<<"未来两小时天气">>, maps:get(forecast_keypoint, maps:get(result, Weather))]),
        ok.

translate(Q, Sl, Tl) ->
    Url = <<"https://translate.googleapis.com/translate_a/single?client=gtx&sl=", (Sl)/binary, "&tl=", (Tl)/binary, "&dt=t&q=", (Q)/binary>>,
    Resp = fetch(#{url => Url, {vbeam, type} => 'FetchConfig'}),
    Json_resp = decode(maps:get(body, Resp), #{{vbeam, type} => 'DecoderOptions'}),
    A = 'Any.as_array'(Json_resp),
    case length(A) > 0 of
        true -> begin
            A0 = 'Any.as_array'(lists:nth(1, A)),
            case length(A0) > 0 of
                true -> begin
                    A00 = 'Any.as_array'(lists:nth(1, A0)),
                    case length(A00) > 0 of
                        true -> 'Any.str'(lists:nth(1, A00));
                        false -> ok
                    end
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    error(<<"invalid translation response">>).
