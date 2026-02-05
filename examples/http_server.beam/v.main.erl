-module('v.main').
-export(['ExampleHandler.handle'/2, main/0]).

'ExampleHandler.handle'(H, Req) ->
    Res = #{header => new_header_from_map(#{content_type => <<"text/plain">>}), {vbeam, type} => 'Response'},
    Status_code = 200,
    Res.

main() ->
    Server = #{handler => #{{vbeam, type} => 'ExampleHandler'}, {vbeam, type} => 'Server'},
    'Server.listen_and_serve'(Server),
    ok.
