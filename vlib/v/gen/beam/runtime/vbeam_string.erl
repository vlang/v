%% vbeam_string - String operations for V language BEAM backend
%% Provides string manipulation functions matching V's string type methods

-module(vbeam_string).
-export([contains/2, starts_with/2, ends_with/2,
         split/2, trim/1, trim/2, trim_left/1, trim_right/1,
         to_lower/1, to_upper/1,
         replace/3, replace_all/3,
         index/2, index_of/2, last_index_of/2,
         substr/3, substr/2,
         len/1, 'int'/1, f64/1,
         repeat/2, reverse/1]).

%% Check if string contains substring
-spec contains(binary(), binary()) -> boolean().
contains(_Haystack, <<>>) ->
    true;  % Empty string is contained in any string
contains(Haystack, Needle) when is_binary(Haystack), is_binary(Needle) ->
    binary:match(Haystack, Needle) =/= nomatch.

%% Check if string starts with prefix
-spec starts_with(binary(), binary()) -> boolean().
starts_with(Str, Prefix) when is_binary(Str), is_binary(Prefix) ->
    PrefixLen = byte_size(Prefix),
    case Str of
        <<Prefix:PrefixLen/binary, _/binary>> -> true;
        _ -> false
    end.

%% Check if string ends with suffix
-spec ends_with(binary(), binary()) -> boolean().
ends_with(Str, Suffix) when is_binary(Str), is_binary(Suffix) ->
    StrLen = byte_size(Str),
    SuffixLen = byte_size(Suffix),
    case StrLen >= SuffixLen of
        true ->
            Skip = StrLen - SuffixLen,
            <<_:Skip/binary, Tail/binary>> = Str,
            Tail =:= Suffix;
        false ->
            false
    end.

%% Split string by delimiter
-spec split(binary(), binary()) -> [binary()].
split(Str, Delim) when is_binary(Str), is_binary(Delim) ->
    binary:split(Str, Delim, [global]).

%% Trim whitespace from both ends
-spec trim(binary()) -> binary().
trim(Str) when is_binary(Str) ->
    trim_right(trim_left(Str)).

%% Trim specific characters from both ends
-spec trim(binary(), binary()) -> binary().
trim(Str, Chars) when is_binary(Str), is_binary(Chars) ->
    CharList = binary_to_list(Chars),
    trim_chars_right(trim_chars_left(Str, CharList), CharList).

%% Trim specific chars from left
trim_chars_left(<<C, Rest/binary>>, CharList) ->
    case lists:member(C, CharList) of
        true -> trim_chars_left(Rest, CharList);
        false -> <<C, Rest/binary>>
    end;
trim_chars_left(<<>>, _CharList) ->
    <<>>.

%% Trim specific chars from right
trim_chars_right(Str, CharList) when is_binary(Str) ->
    trim_chars_right_impl(Str, byte_size(Str), CharList).

trim_chars_right_impl(Str, 0, _CharList) ->
    Str;
trim_chars_right_impl(Str, Len, CharList) ->
    case lists:member(binary:at(Str, Len - 1), CharList) of
        true -> trim_chars_right_impl(binary:part(Str, 0, Len - 1), Len - 1, CharList);
        false -> binary:part(Str, 0, Len)
    end.

%% Trim whitespace from left
-spec trim_left(binary()) -> binary().
trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    trim_left(Rest);
trim_left(Str) ->
    Str.

%% Trim whitespace from right
-spec trim_right(binary()) -> binary().
trim_right(Str) when is_binary(Str) ->
    trim_right_impl(Str, byte_size(Str)).

trim_right_impl(Str, 0) ->
    Str;
trim_right_impl(Str, Len) ->
    case binary:at(Str, Len - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            trim_right_impl(binary:part(Str, 0, Len - 1), Len - 1);
        _ ->
            binary:part(Str, 0, Len)
    end.

%% Convert to lowercase
-spec to_lower(binary()) -> binary().
to_lower(Str) when is_binary(Str) ->
    string:lowercase(Str).

%% Convert to uppercase
-spec to_upper(binary()) -> binary().
to_upper(Str) when is_binary(Str) ->
    string:uppercase(Str).

%% Replace first occurrence
-spec replace(binary(), binary(), binary()) -> binary().
replace(Str, Old, New) when is_binary(Str), is_binary(Old), is_binary(New) ->
    case binary:match(Str, Old) of
        {Pos, Len} ->
            <<Before:Pos/binary, _:Len/binary, After/binary>> = Str,
            <<Before/binary, New/binary, After/binary>>;
        nomatch ->
            Str
    end.

%% Replace all occurrences
-spec replace_all(binary(), binary(), binary()) -> binary().
replace_all(Str, Old, New) when is_binary(Str), is_binary(Old), is_binary(New) ->
    binary:replace(Str, Old, New, [global]).

%% Find index of first occurrence (-1 if not found)
-spec index(binary(), binary()) -> integer().
index(_Str, <<>>) ->
    0;  % Empty string is found at position 0
index(Str, Needle) when is_binary(Str), is_binary(Needle) ->
    case binary:match(Str, Needle) of
        {Pos, _} -> Pos;
        nomatch -> -1
    end.

%% Alias for compatibility
-spec index_of(binary(), binary()) -> integer().
index_of(Str, Needle) ->
    index(Str, Needle).

%% Find index of last occurrence (-1 if not found)
-spec last_index_of(binary(), binary()) -> integer().
last_index_of(Str, Needle) when is_binary(Str), is_binary(Needle) ->
    case binary:matches(Str, Needle) of
        [] -> -1;
        Matches -> element(1, lists:last(Matches))
    end.

%% Get substring from start position to end index (exclusive)
-spec substr(binary(), integer(), integer()) -> binary().
%% substr(<<"hello">>, 1, 4) -> <<"ell">>
substr(Str, Start, End) when is_binary(Str), is_integer(Start), is_integer(End) ->
    StrLen = byte_size(Str),
    ActualStart = max(0, Start),
    ActualEnd = min(End, StrLen),
    case ActualStart < ActualEnd of
        true -> binary:part(Str, ActualStart, ActualEnd - ActualStart);
        false -> <<>>
    end.

%% Get substring from start position to end
-spec substr(binary(), integer()) -> binary().
substr(Str, Start) when is_binary(Str), is_integer(Start) ->
    StrLen = byte_size(Str),
    ActualStart = max(0, Start),
    case ActualStart < StrLen of
        true -> binary:part(Str, ActualStart, StrLen - ActualStart);
        false -> <<>>
    end.

%% Get string length
-spec len(binary()) -> non_neg_integer().
len(Str) when is_binary(Str) ->
    byte_size(Str).

%% Parse string to integer (V's string.int())
-spec 'int'(binary()) -> integer().
'int'(Str) when is_binary(Str) ->
    vbeam_conv:string_to_int(Str).

%% Parse string to float (V's string.f64())
-spec f64(binary()) -> float().
f64(Str) when is_binary(Str) ->
    vbeam_conv:string_to_float(Str).

%% Repeat string N times
-spec repeat(binary(), integer()) -> binary().
repeat(Str, N) when is_binary(Str), is_integer(N), N >= 0 ->
    binary:copy(Str, N).

%% Reverse string
-spec reverse(binary()) -> binary().
reverse(Str) when is_binary(Str) ->
    list_to_binary(lists:reverse(binary_to_list(Str))).
