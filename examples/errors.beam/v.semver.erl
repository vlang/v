-module('v.semver').
-export([version_satisfies/2, compare_eq/2, compare_gt/2, compare_lt/2, compare_ge/2, compare_le/2, parse/1, 'RawVersion.is_valid'/1, 'RawVersion.is_missing'/2, 'RawVersion.coerce'/1, 'RawVersion.complete'/1, 'RawVersion.validate'/1, 'RawVersion.to_version'/1, 'Range.satisfies'/2, 'ComparatorSet.satisfies'/2, 'Comparator.satisfies'/2, parse_range/1, parse_comparator_set/1, parse_comparator/1, parse_xrange/1, can_expand/1, expand_comparator_set/1, expand_tilda/1, expand_caret/1, expand_hyphen/1, expand_xrange/1, make_comparator_set_ge_lt/2, make_comparator_set_ge_le/2, 'EmptyInputError.msg'/1, 'InvalidVersionFormatError.msg'/1, from/1, build/3, 'Version.increment'/2, 'Version.satisfies'/2, 'Version.=='/2, 'Version.<'/2, 'Version.str'/1, coerce/1, is_valid/1, is_version_valid/1, coerce_version/1, increment_version/2, is_valid_string/1, is_valid_number/1, 'Operator__static__from'/1, 'Increment__static__from'/1]).

version_satisfies(Ver, Input) ->
    Range = parse_range(Input),
    'Range.satisfies'(Range, Ver).

compare_eq(V1, V2) ->
    maps:get(major, V1) == maps:get(major, V2) andalso maps:get(minor, V1) == maps:get(minor, V2) andalso maps:get(patch, V1) == maps:get(patch, V2) andalso maps:get(prerelease, V1) == maps:get(prerelease, V2).

compare_gt(V1, V2) ->
    if
        maps:get(major, V1) < maps:get(major, V2) -> false;
        maps:get(major, V1) > maps:get(major, V2) -> true;
        maps:get(minor, V1) < maps:get(minor, V2) -> false;
        maps:get(minor, V1) > maps:get(minor, V2) -> true;
        true -> maps:get(patch, V1) > maps:get(patch, V2)
    end.

compare_lt(V1, V2) ->
    if
        maps:get(major, V1) > maps:get(major, V2) -> false;
        maps:get(major, V1) < maps:get(major, V2) -> true;
        maps:get(minor, V1) > maps:get(minor, V2) -> false;
        maps:get(minor, V1) < maps:get(minor, V2) -> true;
        true -> maps:get(patch, V1) < maps:get(patch, V2)
    end.

compare_ge(V1, V2) ->
    case compare_eq(V1, V2) of
        true -> true;
        false -> compare_gt(V1, V2)
    end.

compare_le(V1, V2) ->
    case compare_eq(V1, V2) of
        true -> true;
        false -> compare_lt(V1, V2)
    end.

parse(Input) ->
    Raw_version = Input,
    Prerelease = <<"">>,
    Metadata = <<"">>,
    Plus_idx = 'string.last_index_u8'(Raw_version, todo),
    case Plus_idx > 0 of
        true -> begin
            Metadata1 = lists:nth(todo + 1, Raw_version),
            Raw_version1 = lists:nth(todo + 1, Raw_version),
        end;
        false -> ok
    end,
    Hyphen_idx = 'string.index_'(Raw_version1, <<"-">>),
    case Hyphen_idx > 0 of
        true -> begin
            Prerelease1 = lists:nth(todo + 1, Raw_version1),
            Raw_version2 = lists:nth(todo + 1, Raw_version1),
        end;
        false -> ok
    end,
    Raw_ints = binary:split(Raw_version2, <<".">>, [global]),
    #{prerelease => Prerelease1, metadata => Metadata1, raw_ints => Raw_ints, {vbeam, type} => 'RawVersion'}.

'RawVersion.is_valid'(Ver) ->
    case length(maps:get(raw_ints, Ver)) /= 3 of
        true -> false;
        false -> is_valid_number(lists:nth(0 + 1, maps:get(raw_ints, Ver))) andalso is_valid_number(lists:nth(1 + 1, maps:get(raw_ints, Ver))) andalso is_valid_number(lists:nth(2 + 1, maps:get(raw_ints, Ver))) andalso is_valid_string(maps:get(prerelease, Ver)) andalso is_valid_string(maps:get(metadata, Ver))
        end.

'RawVersion.is_missing'(Ver, Typ) ->
    Typ >= length(maps:get(raw_ints, Ver)) - 1.

'RawVersion.coerce'(Raw_ver) ->
    Ver = 'RawVersion.complete'(Raw_ver),
    case not is_valid_number(lists:nth(0 + 1, maps:get(raw_ints, Ver))) of
        true -> error(<<"Invalid major version: ", (maps:get(raw_ints, Ver))/binary, "[ver_major]">>);
        false -> 'RawVersion.to_version'(Ver)
        end.

'RawVersion.complete'(Raw_ver) ->
    Raw_ints = maps:get(raw_ints, Raw_ver),
    % TODO: unhandled stmt type
    #{prerelease => maps:get(prerelease, Raw_ver), metadata => maps:get(metadata, Raw_ver), raw_ints => Raw_ints, {vbeam, type} => 'RawVersion'}.

'RawVersion.validate'(Raw_ver) ->
    case not 'RawVersion.is_valid'(Raw_ver) of
        true -> todo;
        false -> 'RawVersion.to_version'(Raw_ver)
        end.

'RawVersion.to_version'(Raw_ver) ->
    #{major => binary_to_integer(lists:nth(0 + 1, maps:get(raw_ints, Raw_ver))), minor => binary_to_integer(lists:nth(1 + 1, maps:get(raw_ints, Raw_ver))), patch => binary_to_integer(lists:nth(2 + 1, maps:get(raw_ints, Raw_ver))), prerelease => maps:get(prerelease, Raw_ver), metadata => maps:get(metadata, Raw_ver), {vbeam, type} => 'Version'}.

'Range.satisfies'(R, Ver) ->
    'ComparatorSet.any'(maps:get(comparator_sets, R), 'ComparatorSet.satisfies'(It, Ver)).

'ComparatorSet.satisfies'(Set, Ver) ->
    lists:foreach(fun(Comp) ->
        case not 'Comparator.satisfies'(Comp, Ver) of
            true -> false;
            false -> ok
        end,
        ok
    end, maps:get(comparators, Set)),
    true.

'Comparator.satisfies'(C, Ver) ->
    case maps:get(op, C) of
        gt -> Ver > maps:get(ver, C);
        lt -> Ver < maps:get(ver, C);
        ge -> Ver >= maps:get(ver, C);
        le -> Ver =< maps:get(ver, C);
        eq -> Ver == maps:get(ver, C)
    end.

parse_range(Input) ->
    Raw_comparator_sets = binary:split(Input, <<" || ">>, [global]),
    Comparator_sets = [],
    lists:foreach(fun(Raw_comp_set) ->
        case can_expand(Raw_comp_set) of
            true -> begin
                S = expand_comparator_set(Raw_comp_set),
                Comparator_sets bsl S
            end;
            false -> begin
                S1 = parse_comparator_set(Raw_comp_set),
                Comparator_sets bsl S1
            end
        end,
        ok
    end, Raw_comparator_sets),
    #{comparator_sets => Comparator_sets, {vbeam, type} => 'Range'}.

parse_comparator_set(Input) ->
    Raw_comparators = binary:split(Input, <<" ">>, [global]),
    case length(Raw_comparators) > 2 of
        true -> todo;
        false -> begin
            Comparators = [],
            lists:foreach(fun(Raw_comp) ->
                C = parse_comparator(Raw_comp),
                Comparators bsl C,
                ok
            end, Raw_comparators),
            #{comparators => Comparators, {vbeam, type} => 'ComparatorSet'}
        end
        end.

parse_comparator(Input) ->
    Op = eq,
    Raw_version = if
        case string:prefix(Input, <<">=">>) of nomatch -> false; _ -> true end -> begin
            Op1 = ge,
            lists:nth(todo + 1, Input)
        end;
        case string:prefix(Input, <<"<=">>) of nomatch -> false; _ -> true end -> begin
            Op2 = le,
            lists:nth(todo + 1, Input)
        end;
        case string:prefix(Input, <<">">>) of nomatch -> false; _ -> true end -> begin
            Op3 = gt,
            lists:nth(todo + 1, Input)
        end;
        case string:prefix(Input, <<"<">>) of nomatch -> false; _ -> true end -> begin
            Op4 = lt,
            lists:nth(todo + 1, Input)
        end;
        case string:prefix(Input, <<"=">>) of nomatch -> false; _ -> true end -> lists:nth(todo + 1, Input);
        true -> Input
    end,
    Version = coerce_version(Raw_version),
    #{ver => Version, op => Op4, {vbeam, type} => 'Comparator'}.

parse_xrange(Input) ->
    Raw_ver = 'RawVersion.complete'(parse(Input)),
    lists:foreach(fun(Typ) ->
        case 'string.index_any'(lists:nth(Typ + 1, maps:get(raw_ints, Raw_ver)), <<"Xx*">>) == -1 of
            true -> ok;
            false -> ok
        end,
        case Typ of
            Semver.ver_major -> begin
            end;
            Semver.ver_minor -> begin
            end;
            Semver.ver_patch -> ok;
            _ -> ok
        end,
        ok
    end, [0, 1, 2]),
    'RawVersion.validate'(Raw_ver).

can_expand(Input) ->
    lists:nth(1, Input) == todo orelse lists:nth(1, Input) == todo orelse case binary:match(Input, <<" - ">>) of nomatch -> false; _ -> true end orelse 'string.index_any'(Input, <<"Xx*">>) > -1.

expand_comparator_set(Input) ->
    case lists:nth(1, Input) of
        todo -> expand_tilda(lists:nth(todo + 1, Input));
        todo -> expand_caret(lists:nth(todo + 1, Input));
        _ -> ok
    end,
    case case binary:match(Input, <<" - ">>) of nomatch -> false; _ -> true end of
        true -> expand_hyphen(Input);
        false -> expand_xrange(Input)
        end.

expand_tilda(Raw_version) ->
    Min_ver = coerce_version(Raw_version),
    Max_ver = case maps:get(minor, Min_ver) == 0 andalso maps:get(patch, Min_ver) == 0 of
        true -> 'Version.increment'(Min_ver, major);
        false -> 'Version.increment'(Min_ver, minor)
    end,
    make_comparator_set_ge_lt(Min_ver, Max_ver).

expand_caret(Raw_version) ->
    Min_ver = coerce_version(Raw_version),
    Max_ver = case maps:get(major, Min_ver) == 0 of
        true -> 'Version.increment'(Min_ver, minor);
        false -> 'Version.increment'(Min_ver, major)
    end,
    make_comparator_set_ge_lt(Min_ver, Max_ver).

expand_hyphen(Raw_range) ->
    Raw_versions = binary:split(Raw_range, <<" - ">>, [global]),
    case length(Raw_versions) /= 2 of
        true -> todo;
        false -> begin
            Min_ver = coerce_version(lists:nth(1, Raw_versions)),
            Raw_max_ver = parse(lists:nth(2, Raw_versions)),
            case 'RawVersion.is_missing'(Raw_max_ver, 0) of
                true -> todo;
                false -> 
                    case 'RawVersion.is_missing'(Raw_max_ver, 1) of
                        true -> make_comparator_set_ge_lt(Min_ver, Max_ver);
                        false -> begin
                            Max_ver = 'RawVersion.coerce'(Raw_max_ver),
                            make_comparator_set_ge_le(Min_ver, Max_ver)
                        end
                                        end
                                        end
        end
        end.

expand_xrange(Raw_range) ->
    Min_ver = parse_xrange(Raw_range),
    case maps:get(major, Min_ver) == 0 of
        true -> #{comparators => [#{ver => Min_ver, op => ge, {vbeam, type} => 'Comparator'}], {vbeam, type} => 'ComparatorSet'};
        false -> begin
            Max_ver = case maps:get(minor, Min_ver) == 0 of
                true -> 'Version.increment'(Min_ver, major);
                false -> 'Version.increment'(Min_ver, minor)
            end,
            make_comparator_set_ge_lt(Min_ver, Max_ver)
        end
        end.

make_comparator_set_ge_lt(Min, Max) ->
    #{comparators => [#{ver => Min, op => ge, {vbeam, type} => 'Comparator'}, #{ver => Max, op => lt, {vbeam, type} => 'Comparator'}], {vbeam, type} => 'ComparatorSet'}.

make_comparator_set_ge_le(Min, Max) ->
    #{comparators => [#{ver => Min, op => ge, {vbeam, type} => 'Comparator'}, #{ver => Max, op => le, {vbeam, type} => 'Comparator'}], {vbeam, type} => 'ComparatorSet'}.

'EmptyInputError.msg'(Err) ->
    <<"Empty input">>.

'InvalidVersionFormatError.msg'(Err) ->
    <<"Invalid version format for input \"", (maps:get(input, Err))/binary, "\"">>.

from(Input) ->
    case length(Input) == 0 of
        true -> todo;
        false -> begin
            Raw_version = parse(Input),
            'RawVersion.validate'(Raw_version)
        end
        end.

build(Major, Minor, Patch) ->
    #{major => Major, minor => Minor, patch => Patch, prerelease => <<"">>, metadata => <<"">>, {vbeam, type} => 'Version'}.

'Version.increment'(Ver, Typ) ->
    increment_version(Ver, Typ).

'Version.satisfies'(Ver, Input) ->
    version_satisfies(Ver, Input).

'Version.=='(V1, V2) ->
    compare_eq(V1, V2).

'Version.<'(V1, V2) ->
    compare_lt(V1, V2).

'Version.str'(Ver) ->
    Common_string = <<(integer_to_binary(maps:get(major, Ver)))/binary, ".", (integer_to_binary(maps:get(minor, Ver)))/binary, ".", (integer_to_binary(maps:get(patch, Ver)))/binary>>,
    Prerelease_string = case length(maps:get(prerelease, Ver)) > 0 of
        true -> <<"-", (maps:get(prerelease, Ver))/binary>>;
        false -> <<"">>
    end,
    Metadata_string = case length(maps:get(metadata, Ver)) > 0 of
        true -> <<"+", (maps:get(metadata, Ver))/binary>>;
        false -> <<"">>
    end,
    <<(Common_string)/binary, (Prerelease_string)/binary, (Metadata_string)/binary>>.

coerce(Input) ->
    coerce_version(Input).

is_valid(Input) ->
    is_version_valid(Input).

is_version_valid(Input) ->
    Raw_ver = parse(Input),
    'RawVersion.is_valid'(Raw_ver).

coerce_version(Input) ->
    Raw_ver = parse(Input),
    'RawVersion.coerce'(Raw_ver).

increment_version(Ver, Typ) ->
    Major = maps:get(major, Ver),
    Minor = maps:get(minor, Ver),
    Patch = maps:get(patch, Ver),
    case Typ of
        major -> begin
            todo,
            Minor1 = 0,
            Patch1 = 0,
        end;
        minor -> begin
            todo,
            Patch2 = 0,
        end;
        patch -> todo
    end,
    #{major => Major, minor => Minor1, patch => Patch2, prerelease => maps:get(prerelease, Ver), metadata => maps:get(metadata, Ver), {vbeam, type} => 'Version'}.

is_valid_string(Input) ->
    lists:foreach(fun(C) ->
        case not ('u8.is_letter'(C) orelse 'u8.is_digit'(C) orelse C == todo orelse C == todo) of
            true -> false;
            false -> ok
        end,
        ok
    end, Input),
    true.

is_valid_number(Input) ->
    lists:foreach(fun(C) ->
        case not 'u8.is_digit'(C) of
            true -> false;
            false -> ok
        end,
        ok
    end, Input),
    true.

'Operator__static__from'(Input) ->
    error(<<"invalid value">>).

'Increment__static__from'(Input) ->
    error(<<"invalid value">>).
