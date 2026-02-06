-module('v.v.vmod').
-export([quote/1, encode_array/2, encode/1, from_file/1, decode/1, 'Scanner.tokenize'/3, 'Scanner.skip_whitespace'/1, is_name_alpha/1, 'Scanner.create_string'/2, 'Scanner.create_ident'/1, 'Scanner.peek_char'/2, 'Scanner.scan_all'/1, get_array_content/2, 'Parser.parse'/1, get_cache/0, new_mod_file_cacher/0, 'ModFileCacher.debug'/1, 'ModFileCacher.get_by_file'/2, 'ModFileCacher.get_by_folder'/2, 'ModFileCacher.add'/3, 'ModFileCacher.traverse'/2, 'ModFileCacher.mark_folders_with_vmod'/3, 'ModFileCacher.mark_folders_as_vmod_free'/2, 'ModFileCacher.check_for_stop'/2, 'ModFileCacher.get_files'/2, 'TokenKind__static__from'/1]).

quote(Input) ->
    case case binary:match(Input, <<"'">>) of nomatch -> false; _ -> true end of
        true -> <<(<<(<<"\"">>)/binary, (Input)/binary>>)/binary, (<<"\"">>)/binary>>;
        false -> <<(<<(<<"'">>)/binary, (Input)/binary>>)/binary, (<<"'">>)/binary>>
        end.

encode_array(B, Input) ->
    case length(iolist_to_binary(lists:join(<<"">>, Input))) > 60 of
        true -> begin
            'Builder.writeln'(B, <<"[">>),
            lists:foreach(fun(Item) ->
                'Builder.write_string'(B, <<"\\t\\t">>),
                ok.
                'Builder.write_string'(B, quote(Item)),
                ok.
                'Builder.writeln'(B, <<",">>),
                ok.
                ok
            end, Input),
            'Builder.writeln'(B, <<"\\t]">>)
        end;
        false -> begin
            Quoted = [],
            lists:foreach(fun(Item) ->
                Quoted bsl quote(Item),
                ok.
                ok
            end, Input),
            'Builder.write_string'(B, <<"[">>),
            'Builder.write_string'(B, iolist_to_binary(lists:join(<<", ">>, Quoted))),
            'Builder.writeln'(B, <<"]">>)
        end
    end.

encode(Manifest) ->
    B = new_builder(512),
    'Builder.writeln'(B, <<"Module {">>),
    'Builder.write_string'(B, <<"\\tname: ">>),
    'Builder.writeln'(B, quote(maps:get(name, Manifest))),
    'Builder.write_string'(B, <<"\\tdescription: ">>),
    'Builder.writeln'(B, quote(maps:get(description, Manifest))),
    'Builder.write_string'(B, <<"\\tversion: ">>),
    'Builder.writeln'(B, quote(maps:get(version, Manifest))),
    'Builder.write_string'(B, <<"\\tlicense: ">>),
    'Builder.writeln'(B, quote(maps:get(license, Manifest))),
    case maps:get(repo_url, Manifest) /= <<"">> of
        true -> begin
            'Builder.write_string'(B, <<"\\trepo_url: ">>),
            'Builder.writeln'(B, quote(maps:get(repo_url, Manifest)))
        end;
        false -> ok
    end,
    case maps:get(author, Manifest) /= <<"">> of
        true -> begin
            'Builder.write_string'(B, <<"\\tauthor: ">>),
            'Builder.writeln'(B, quote(maps:get(author, Manifest)))
        end;
        false -> ok
    end,
    'Builder.write_string'(B, <<"\\tdependencies: ">>),
    encode_array(B, maps:get(dependencies, Manifest)),
    lists:foreach(fun(Values) ->
        'Builder.write_string'(B, <<"\\t">>),
        'Builder.write_string'(B, Key),
        'Builder.write_string'(B, <<": ">>),
        encode_array(B, Values),
        ok
    end, maps:get(unknown, Manifest)),
    'Builder.write_string'(B, <<"}">>),
    'Builder.str'(B).

from_file(Vmod_path) ->
    case not exists(Vmod_path) of
        true -> error(<<"v.mod: v.mod file not found.">>);
        false -> begin
            Contents = read_file(Vmod_path),
            decode(Contents)
        end
        end.

decode(Contents) ->
    Parser = #{scanner => #{pos => 0, text => Contents, {vbeam, type} => 'Scanner'}, {vbeam, type} => 'Parser'},
    'Parser.parse'(Parser).

'Scanner.tokenize'(S, T_type, Val) ->
    maps:get(tokens, S) bsl #{typ => T_type, val => Val, line => maps:get(line, S), {vbeam, type} => 'Token'},
    ok.

'Scanner.skip_whitespace'(S) ->
    % TODO: unhandled stmt type
        ok.

is_name_alpha(Chr) ->
    'u8.is_letter'(Chr) orelse Chr == todo.

'Scanner.create_string'(S, Q) ->
    Str = <<"">>,
    % TODO: unhandled stmt type
    Str.

'Scanner.create_ident'(S) ->
    Text = <<"">>,
    % TODO: unhandled stmt type
    Text.

'Scanner.peek_char'(S, C) ->
    maps:get(pos, S) - 1 < length(maps:get(text, S)) andalso lists:nth(maps:get(pos, S) - 1 + 1, maps:get(text, S)) == C.

'Scanner.scan_all'(S) ->
    % TODO: unhandled stmt type
    'Scanner.tokenize'(S, eof, <<"eof">>),
    ok.

get_array_content(Tokens, St_idx) ->
    Vals = [],
    Idx = St_idx,
    case maps:get(typ, lists:nth(Idx + 1, Tokens)) /= labr of
        true -> error(<<(<<"vmod:">>)/binary, " not a valid array, at line ", (integer_to_binary(maps:get(line, lists:nth(Idx + 1, Tokens))))/binary>>);
        false -> begin
            todo,
            % TODO: unhandled stmt type
            Vals
        end
        end.

'Parser.parse'(P) ->
    case length(maps:get(text, maps:get(scanner, P))) == 0 of
        true -> error(<<(<<"vmod:">>)/binary, " no content.">>);
        false -> begin
            'Scanner.scan_all'(maps:get(scanner, P)),
            Tokens = maps:get(tokens, maps:get(scanner, P)),
            Mn = #{{vbeam, type} => 'Manifest'},
            case maps:get(typ, lists:nth(1, Tokens)) /= module_keyword of
                true -> error(<<(<<"vmod:">>)/binary, " v.mod files should start with Module, at line ", (integer_to_binary(maps:get(line, lists:nth(1, Tokens))))/binary>>);
                false -> begin
                    I = 1,
                    % TODO: unhandled stmt type
                    Mn
                end
                        end
        end
        end.

get_cache() ->
    new_mod_file_cacher().

new_mod_file_cacher() ->
    #{{vbeam, type} => 'ModFileCacher'}.

'ModFileCacher.debug'(Mcache) ->
    io:format(standard_error, "~s~n", [<<"ModFileCacher hits: ", (integer_to_binary(maps:get(hits, Mcache)))/binary, ", misses: ", (integer_to_binary(maps:get(misses, Mcache)))/binary, " | get_files_hits: ", (integer_to_binary(maps:get(get_files_hits, Mcache)))/binary, " | get_files_misses: ", (integer_to_binary(maps:get(get_files_misses, Mcache)))/binary>>]),
    io:format(standard_error, "~s~n", [<<"\t ModFileCacher.cache.len: ", (integer_to_binary(maps:size(maps:get(cache, Mcache))))/binary>>]),
    lists:foreach(fun(V) ->
        io:format(standard_error, "~s~n", [<<"\t K: ", (K)/binary, " | v.mod: ", (maps:get(vmod_file, V))/binary, " | folder: `", (maps:get(vmod_folder, V))/binary, "`">>]),
        ok
    end, maps:get(cache, Mcache)),
    io:format(standard_error, "~s~n", [<<"\t ModFileCacher.folder_files:">>]),
    lists:foreach(fun(V) ->
        io:format(standard_error, "~s~n", [<<"\t K: ", (K)/binary, " | folder_files: ", (V)/binary>>]),
        ok.
        ok
    end, maps:get(folder_files, Mcache)),
        ok.

'ModFileCacher.get_by_file'(Mcache, Vfile) ->
    'ModFileCacher.get_by_folder'(Mcache, dir(Vfile)).

'ModFileCacher.get_by_folder'(Mcache, Vfolder) ->
    Mfolder = real_path(Vfolder),
    case lists:member(Mfolder, maps:get(cache, Mcache)) of
        true -> maps:get(Mfolder, maps:get(cache, Mcache));
        false -> begin
            Traversed_folders = element(1, 'ModFileCacher.traverse'(Mcache, Mfolder)),
            Res = element(2, 'ModFileCacher.traverse'(Mcache, Mfolder)),
            lists:foreach(fun(Tfolder) ->
                'ModFileCacher.add'(Mcache, Tfolder, Res),
                ok
            end, Traversed_folders),
            todo,
            Res
        end
        end.

'ModFileCacher.add'(Cacher, Path, Result) ->

'ModFileCacher.traverse'(Mcache, Mfolder) ->
    Cfolder = Mfolder,
    Folders_so_far = [Cfolder],
    Levels = 0,
    % TODO: unhandled stmt type
    'ModFileCacher.mark_folders_as_vmod_free'(Mcache, Folders_so_far),
    [Mfolder].

'ModFileCacher.mark_folders_with_vmod'(Mcache, Folders_so_far, Vmod) ->
    lists:foreach(fun(F) ->
        'ModFileCacher.add'(Mcache, F, Vmod),
        ok.
        ok
    end, Folders_so_far),
        ok.

'ModFileCacher.mark_folders_as_vmod_free'(Mcache, Folders_so_far) ->
    lists:foreach(fun(F) ->
        'ModFileCacher.add'(Mcache, F, #{vmod_file => <<"">>, vmod_folder => F, {vbeam, type} => 'ModFileAndFolder'}),
        ok.
        ok
    end, Folders_so_far),
        ok.

'ModFileCacher.check_for_stop'(Mcache, Files) ->
    lists:foreach(fun(I) ->
        case lists:member(I, Files) of
            true -> true;
            false -> ok
        end,
        ok
    end, [<<".git">>, <<".hg">>, <<".svn">>, <<".v.mod.stop">>]),
    false.

'ModFileCacher.get_files'(Mcache, Cfolder) ->
    case lists:member(Cfolder, maps:get(folder_files, Mcache)) of
        true -> maps:get(Cfolder, maps:get(folder_files, Mcache));
        false -> begin
            todo,
            Files = [],
            case exists(Cfolder) andalso is_dir(Cfolder) of
                true -> case todo of
                    true -> ok;
                    false -> ok
                end;
                false -> ok
            end,
            Files
        end
        end.

'TokenKind__static__from'(Input) ->
    error(<<"invalid value">>).
