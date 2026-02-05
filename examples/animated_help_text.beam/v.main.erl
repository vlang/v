-module('v.main').
-export([event/2, frame/1, main/0]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

event(E, _app) ->
    case maps:get(typ, E) of
        mouse_down -> ok;
        mouse_drag -> ok;
        mouse_up -> ok;
        key_down -> case maps:get(code, E) == c of
            true -> ok;
            false -> case maps:get(code, E) == escape of
                true -> exit(0);
                false -> ok
            end
        end;
        _ -> ok
    end.

frame(App) ->
    'Context.clear'(maps:get(tui, App)),
    todo,
    case maps:get(frame, App) % 4 == 0 of
        true -> begin
        end;
        false -> ok
    end,
    case maps:get(frame, App) % 100 == 0 of
        true -> case maps:get(direction, App) > 0 of
            true -> ok;
            false -> ok
        end;
        false -> ok
    end,
    Help_text = to_doc(#{version => <<"1.0">>, description => <<"Hello! This should show an *animated* example application description.\nWe are at frame ", (integer_to_binary(maps:get(frame, App)))/binary, ".\nPress ESCAPE or Ctrl+C to exit">>, footer => <<"\nPress ESCAPE or Ctrl+C to exit">>, fields => #{<<"level">> => <<"Level of lorem ipsum\\nand more\\nmany many many more.\\nNotice how user newlines/format is kept since\\ninput lines are all less or within\\nthe default layout.description_padding\\nand max width">>, <<"example">> => <<"Looong example text without newlines or anything else and lorem ipsum and more and many many many more. Should be auto fitted">>, <<"multi">> => <<"This flag can be repeated">>, <<"-e, --extra">> => <<"Secret flag that does not exist on the struct, but we want documented (in same format as the others)">>, <<"-q, --quiet-and-quite-long-flag <string>">> => <<"Mega long description and secret flag that does not exist on the struct, but we want documented. Also the flag has custom newlines\\nand the flag line itself is super long">>, <<"square">> => maps:get(square, App)}, {vbeam, type} => 'DocConfig'}),
    'Context.draw_text'(maps:get(tui, App), 0, 0, Help_text),
    'Context.reset'(maps:get(tui, App)),
    'Context.flush'(maps:get(tui, App)),
    ok.

main() ->
    App = &#{{vbeam, type} => 'App'},
    'Context.run'(maps:get(tui, App)),
    ok.
