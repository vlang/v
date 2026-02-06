-module('v.main').
-export(['Edit.next'/1, event/2, frame/1, main/0, 'Edit__static__from'/1]).

'Edit.next'(E) ->
    case E of
        description_padding -> description_width;
        description_width -> flag_indent;
        flag_indent -> compact;
        compact -> name;
        name -> version;
        version -> flags;
        flags -> flag_type;
        flag_type -> flag_hint;
        flag_hint -> description;
        description -> flags_header;
        flags_header -> footer;
        footer -> description_padding
    end.

event(E, App) ->
    Incr_decr = 0,
    case maps:get(typ, E) of
        mouse_down -> ok;
        mouse_drag -> ok;
        mouse_up -> ok;
        key_down -> case maps:get(code, E) of
            left -> ok;
            right -> ok;
            space -> ok;
            escape -> exit(0);
            _ -> ok
        end;
        _ -> ok
    end,
    case Incr_decr /= 0 of
        true -> case maps:get(edit, App) of
            flag_indent -> ok;
            description_padding -> ok;
            description_width -> ok;
            compact -> ok;
            name -> 'Show.toggle'(maps:get(show, maps:get(options, App)), name);
            version -> 'Show.toggle'(maps:get(show, maps:get(options, App)), version);
            flags -> 'Show.toggle'(maps:get(show, maps:get(options, App)), flags);
            flag_type -> 'Show.toggle'(maps:get(show, maps:get(options, App)), flag_type);
            flag_hint -> 'Show.toggle'(maps:get(show, maps:get(options, App)), flag_hint);
            description -> 'Show.toggle'(maps:get(show, maps:get(options, App)), description);
            flags_header -> 'Show.toggle'(maps:get(show, maps:get(options, App)), flags_header);
            footer -> 'Show.toggle'(maps:get(show, maps:get(options, App)), footer)
        end;
        false -> ok
    end.

frame(App) ->
    'Context.clear'(maps:get(tui, App)),
    Value = case maps:get(edit, App) of
        flag_indent -> integer_to_binary(maps:get(flag_indent, maps:get(layout, App)));
        description_padding -> integer_to_binary(maps:get(description_padding, maps:get(layout, App)));
        description_width -> integer_to_binary(maps:get(description_width, maps:get(layout, App)));
        compact -> case maps:get(compact, maps:get(options, App)) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        name -> case 'Show.has'(maps:get(show, maps:get(options, App)), name) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        version -> case 'Show.has'(maps:get(show, maps:get(options, App)), version) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        flags -> case 'Show.has'(maps:get(show, maps:get(options, App)), flags) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        flag_type -> case 'Show.has'(maps:get(show, maps:get(options, App)), flag_type) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        flag_hint -> case 'Show.has'(maps:get(show, maps:get(options, App)), flag_hint) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        description -> case 'Show.has'(maps:get(show, maps:get(options, App)), description) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        flags_header -> case 'Show.has'(maps:get(show, maps:get(options, App)), flags_header) of
            true -> <<"on">>;
            false -> <<"off">>
        end;
        footer -> case 'Show.has'(maps:get(show, maps:get(options, App)), footer) of
            true -> <<"on">>;
            false -> <<"off">>
        end
    end,
    'Context.draw_text'(maps:get(tui, App), 0, 0, <<"Click left-mouse button or use space to edit the next property.\nUse keyboard arrow keys right and left to adjust the value of the property\nEditing property: ", (maps:get(edit, App))/binary, ", value: ", (Value)/binary>>),
    Help_text = to_doc(#{description => <<"Simple DocLayout editor.\nPress ESCAPE or Ctrl+C to exit and print layout code">>, footer => <<"\nPress ESCAPE or Ctrl+C to exit and print layout code">>, fields => todo, layout => maps:get(layout, App), options => maps:get(options, App), {vbeam, type} => 'DocConfig'}),
    'Context.draw_text'(maps:get(tui, App), 0, 5, Help_text),
    'Context.reset'(maps:get(tui, App)),
    'Context.flush'(maps:get(tui, App)),
    ok.

main() ->
    App = #{{vbeam, type} => 'App'},
    at_exit(todo),
    'Context.run'(maps:get(tui, App)),
    ok.

'Edit__static__from'(Input) ->
    error(<<"invalid value">>).
