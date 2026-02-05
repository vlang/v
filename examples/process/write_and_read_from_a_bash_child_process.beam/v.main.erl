-module('v.main').
-export([exec/1, main/0]).
% TODO: const tmp_folder = os.join_path(os.temp_dir(), 'process_folder');
% TODO: const max_txt_files = 20;

exec(Cmd) ->
    Out = [],
    Er = [],
    Rc = 0,
    P = new_process(<<"/bin/bash">>),
    'Process.set_redirect_stdio'(P),
    'Process.run'(P),
    'Process.stdin_write'(P, <<"echo \"START         \" && sleep 0.1 && ", (Cmd)/binary, ";\\n">>),
    'Process.stdin_write'(P, <<"ECODE=\\$?;\\n">>),
    'Process.stdin_write'(P, <<"sleep 0.1;\\n">>),
    'Process.stdin_write'(P, <<"exit \\$ECODE;\\n">>),
    % TODO: for p.is_alive() {
    Out << 'Process.stdout_slurp'(P),
    Er << 'Process.stderr_slurp'(P),
    'Process.close'(P),
    'Process.wait'(P),
    case maps:get(code, P) > 0 of
        true -> begin
            eprintln(<<"----------------------------------------------------------">>),
            eprintln(<<"COMMAND: ", (Cmd)/binary>>),
            eprintln(<<"STDOUT:\\n", (Out)/binary>>),
            eprintln(<<"STDERR:\\n", (Er)/binary>>),
            eprintln(<<"----------------------------------------------------------">>),
            Rc1 = 1,
        end;
        false -> ok
    end,
    '[]string.join'(Out, <<"">>).

main() ->
    Out = <<"">>,
    Er = <<"">>,
    Ecode = 0,
    % TODO: defer {os.rmdir_all(tmp_folder);}
    mkdir_all(join_path(temp_dir(), <<"process_folder">>), #{{vbeam, type} => 'MkdirParams'}),
    lists:foreach(fun(I) ->
        write_file(join_path(join_path(temp_dir(), <<"process_folder">>), <<(integer_to_binary(I))/binary, ".txt">>), <<(integer_to_binary(I))/binary, "\\n", (integer_to_binary(I))/binary, "\\n">>),
        ok
    end, lists:seq(0, 20 - 1)),
    Out1 = element(1, exec(<<"find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; sleep 0.1; find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; echo '******'">>)),
    Ecode1 = element(2, exec(<<"find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; sleep 0.1; find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; echo '******'">>)),
    Er1 = element(3, exec(<<"find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; sleep 0.1; find ", (quoted_path(join_path(temp_dir(), <<"process_folder">>)))/binary, " ; echo '******'">>)),
    % TODO: assert out.ends_with('******\n')
    % TODO: assert er == ''
    Out2 = element(1, exec(<<"echo to stdout">>)),
    Ecode2 = element(2, exec(<<"echo to stdout">>)),
    Er2 = element(3, exec(<<"echo to stdout">>)),
    % TODO: assert out.contains('to stdout')
    % TODO: assert er == ''
    Out3 = element(1, exec(<<"echo to stderr 1>&2">>)),
    Ecode3 = element(2, exec(<<"echo to stderr 1>&2">>)),
    Er3 = element(3, exec(<<"echo to stderr 1>&2">>)),
    % TODO: assert out.starts_with('START')
    % TODO: assert er.contains('to stderr')
    Out4 = element(1, exec(<<"ls /sssss">>)),
    Ecode4 = element(2, exec(<<"ls /sssss">>)),
    Er4 = element(3, exec(<<"ls /sssss">>)),
    % TODO: assert out.starts_with('START')
    % TODO: assert er != ''
    % TODO: assert ecode > 0
    vbeam_io:println(<<"test ok stderr & stdout is indeed redirected, ecode: ", (integer_to_binary(Ecode4))/binary>>),
    ok.
