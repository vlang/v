-module('v.main').
-export([main/0]).

main() ->
    Doc = parse_text(<<"# This is a TOML document.\n\ntitle = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00-08:00 # First class dates\n\n[database]\nserver = \"192.168.1.1\"\nports = [ 8000, 8001, 8002 ]\nconnection_max = 5000\nenabled = true\n\n[servers]\n\n  # Indentation (tabs and/or spaces) is allowed but not required\n  [servers.alpha]\n  ip = \"10.0.0.1\"\n  dc = \"eqdc10\"\n\n  [servers.beta]\n  ip = \"10.0.0.2\"\n  dc = \"eqdc10\"\n\n[clients]\ndata = [ [\"gamma\", \"delta\"], [1, 2] ]\n\n# Line breaks are OK when inside arrays\nhosts = [\n  \"alpha\",\n  \"omega\"\n]">>),
    Title = 'Any.string'('Doc.value'(Doc, <<"title">>)),
    vbeam_io:println(<<"title: \"", (Title)/binary, "\"">>),
    Ip = 'Any.string'('Doc.value'(Doc, <<"servers.alpha.ip">>)),
    vbeam_io:println(<<"Server IP: \"", (Ip)/binary, "\"">>),
    Toml_json = json(Doc),
    vbeam_io:println(Toml_json),
    ok.
