-module('v.main').
-export([main/0]).

main() ->
    Html = get_text(<<"https://news.ycombinator.com">>),
    Pos = 0,
    % TODO: unhandled stmt type
    ok