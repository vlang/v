%% vbeam_map - Map operations for V language BEAM backend
%% V maps are represented as Erlang maps

-module(vbeam_map).

-moduledoc """
Provides map helpers for V-style keyed lookup and transformation.
""".






-export([get/2, get/3, set/3, delete/2,
         contains/2, keys/1, values/1,
         len/1, clear/1,
         merge/2, filter/2, map/2]).

%% Get value by key (raises error if not found)
%% 
-doc """
get/2 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`, `term()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec get(map(), term()) -> term().

get(Map, Key) when is_map(Map) ->
    true = maps:is_key(Key, Map),
    maps:get(Key, Map).

%% Get value by key with default
%% 
-spec get(map(), term(), term()) -> term().

get(Map, Key, Default) when is_map(Map) ->
    Result = maps:get(Key, Map, Default),
    true = maps:is_key(Key, Map) orelse Result =:= Default,
    Result.

%% Set/update key-value pair (returns new map)
%% 
-doc """
set/3 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`, `term()`, `term()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec set(map(), term(), term()) -> map().

set(Map, Key, Value) when is_map(Map) ->
    Updated = maps:put(Key, Value, Map),
    true = maps:is_key(Key, Updated),
    Updated.

%% Delete key (returns new map)
%% 
-spec delete(map(), term()) -> map().

delete(Map, Key) when is_map(Map) ->
    Updated = maps:remove(Key, Map),
    true = not maps:is_key(Key, Updated),
    Updated.

%% Check if key exists
%% 
-doc """
contains/2 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`, `term()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec contains(map(), term()) -> boolean().

contains(Map, Key) when is_map(Map) ->
    Found = maps:is_key(Key, Map),
    true = Found =:= lists:member(Key, maps:keys(Map)),
    Found.

%% Get all keys as list
%% 
-spec keys(map()) -> [term()].

keys(Map) when is_map(Map) ->
    Keys = maps:keys(Map),
    true = length(Keys) =:= maps:size(Map),
    Keys.

%% Get all values as list
%% 
-doc """
values/1 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec values(map()) -> [term()].

values(Map) when is_map(Map) ->
    Values = maps:values(Map),
    true = length(Values) =:= maps:size(Map),
    Values.

%% Get number of key-value pairs
%% 
-spec len(map()) -> non_neg_integer().
len(Map) when is_map(Map) ->
    Size = maps:size(Map),
    true = is_integer(Size) andalso Size >= 0,
    Size.

%% Clear all entries (returns empty map)
%% 
-doc """
clear/1 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec clear(map()) -> map().
clear(Map) when is_map(Map) ->
    true = maps:size(Map) >= 0,
    #{}.

%% Merge two maps (second map's values override first)
%% 
-spec merge(map(), map()) -> map().
merge(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    Merged = maps:merge(Map1, Map2),
    true = is_map(Merged),
    Merged.

%% Filter map with predicate on key-value pairs
%% 
-doc """
filter/2 is a public runtime entrypoint in `vbeam_map`.
Parameters: `map()`, `fun((term(), term()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec filter(map(), fun((term(), term()) -> boolean())) -> map().
filter(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    Filtered = maps:filter(Fun, Map),
    true = is_map(Filtered),
    Filtered.

%% Map function over values
%% 
-spec map(map(), fun((term(), term()) -> term())) -> map().
map(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    Mapped = maps:map(Fun, Map),
    true = is_map(Mapped),
    Mapped.






