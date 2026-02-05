-module('v.main').
-export([wkhtmltopdf_init/1, wkhtmltopdf_deinit/0, wkhtmltopdf_version/0, wkhtmltopdf_create_global_settings/0, wkhtmltopdf_destroy_global_settings/1, wkhtmltopdf_set_global_setting/3, wkhtmltopdf_create_object_settings/0, wkhtmltopdf_destroy_object_settings/1, wkhtmltopdf_set_object_setting/3, wkhtmltopdf_create_converter/1, wkhtmltopdf_destroy_converter/1, wkhtmltopdf_add_object/3, wkhtmltopdf_convert/1, wkhtmltopdf_http_error_code/1, wkhtmltopdf_get_output/2, main/0]).
% TODO: [unhandled stmt str type: v.ast.HashStmt ]
% TODO: [unhandled stmt str type: v.ast.HashStmt ]

wkhtmltopdf_init(Use_graphics) ->
    ok.

wkhtmltopdf_deinit() ->
    ok.

wkhtmltopdf_version() ->
    ok.

wkhtmltopdf_create_global_settings() ->
    ok.

wkhtmltopdf_destroy_global_settings(Global_settings) ->
    ok.

wkhtmltopdf_set_global_setting(Global_settings, Name, Value) ->
    ok.

wkhtmltopdf_create_object_settings() ->
    ok.

wkhtmltopdf_destroy_object_settings(Object_settings) ->
    ok.

wkhtmltopdf_set_object_setting(Object_settings, Name, Value) ->
    ok.

wkhtmltopdf_create_converter(Global_settings) ->
    ok.

wkhtmltopdf_destroy_converter(Converter) ->
    ok.

wkhtmltopdf_add_object(Converter, Object_settings, Data) ->
    ok.

wkhtmltopdf_convert(Converter) ->
    ok.

wkhtmltopdf_http_error_code(Converter) ->
    ok.

wkhtmltopdf_get_output(Converter, Data) ->
    ok.

main() ->
    Init = wkhtmltopdf_init(0),
    vbeam_io:println(<<"wkhtmltopdf_init: ", (integer_to_binary(Init))/binary>>),
    Version = todo,
    vbeam_io:println(<<"wkhtmltopdf_version: ", (Version)/binary>>),
    Global_settings = wkhtmltopdf_create_global_settings(),
    vbeam_io:println(<<"wkhtmltopdf_create_global_settings: ", (todo)/binary>>),
    Object_settings = wkhtmltopdf_create_object_settings(),
    io:format("~s~n", [<<"wkhtmltopdf_create_object_settings">>]),
    Converter = wkhtmltopdf_create_converter(Global_settings),
    vbeam_io:println(<<"wkhtmltopdf_create_converter: ", (todo)/binary>>),
    Result = wkhtmltopdf_set_object_setting(Object_settings, <<"page">>, <<"http://www.google.com.br">>),
    vbeam_io:println(<<"wkhtmltopdf_set_object_setting: ", (atom_to_binary(Result))/binary, " [page = http://www.google.com.br]">>),
    wkhtmltopdf_add_object(Converter, Object_settings, 0),
    io:format("~s~n", [<<"wkhtmltopdf_add_object">>]),
    Result1 = wkhtmltopdf_convert(Converter),
    vbeam_io:println(<<"wkhtmltopdf_convert: ", (atom_to_binary(Result1))/binary>>),
    Error_code = wkhtmltopdf_http_error_code(Converter),
    vbeam_io:println(<<"wkhtmltopdf_http_error_code: ", (integer_to_binary(Error_code))/binary>>),
    case Result1 of
        true -> begin
            Pdata = todo,
            Ppdata = &Pdata,
            Size = wkhtmltopdf_get_output(Converter, todo),
            vbeam_io:println(<<"wkhtmltopdf_get_output: ", (integer_to_binary(Size))/binary, " bytes">>),
            File = open_file(<<"./google.pdf">>, <<"w+">>, 0o666),
            Wrote = todo,
            vbeam_io:println(<<"write_bytes: ", (integer_to_binary(Wrote))/binary, " [./google.pdf]">>),
            'File.flush'(File),
            'File.close'(File)
        end;
        false -> ok
    end,
    wkhtmltopdf_destroy_converter(Converter),
    io:format("~s~n", [<<"wkhtmltopdf_destroy_converter">>]),
    wkhtmltopdf_destroy_object_settings(Object_settings),
    vbeam_io:println(<<"wkhtmltopdf_destroy_object_settings: ", (todo)/binary>>),
    wkhtmltopdf_destroy_global_settings(Global_settings),
    io:format("~s~n", [<<"wkhtmltopdf_destroy_global_settings">>]),
    Deinit = wkhtmltopdf_deinit(),
    vbeam_io:println(<<"wkhtmltopdf_deinit: ", (integer_to_binary(Deinit))/binary>>),
    ok.
