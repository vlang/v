%% vbeam_map - Map operations for V language BEAM backend
%% V maps are represented as Erlang maps

-module(vbeam_map).
-export([get/2, get/3, set/3, delete/2,
         contains/2, keys/1, values/1,
         len/1, clear/1,
         merge/2, filter/2, map/2]).

%% Get value by key (raises error if not found)
get(Map, Key) when is_map(Map) ->
    maps:get(Key, Map).

%% Get value by key with default
get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default).

%% Set/update key-value pair (returns new map)
set(Map, Key, Value) when is_map(Map) ->
    maps:put(Key, Value, Map).

%% Delete key (returns new map)
delete(Map, Key) when is_map(Map) ->
    maps:remove(Key, Map).

%% Check if key exists
contains(Map, Key) when is_map(Map) ->
    maps:is_key(Key, Map).

%% Get all keys as list
keys(Map) when is_map(Map) ->
    maps:keys(Map).

%% Get all values as list
values(Map) when is_map(Map) ->
    maps:values(Map).

%% Get number of key-value pairs
len(Map) when is_map(Map) ->
    maps:size(Map).

%% Clear all entries (returns empty map)
clear(_Map) ->
    #{}.

%% Merge two maps (second map's values override first)
merge(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    maps:merge(Map1, Map2).

%% Filter map with predicate on key-value pairs
filter(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    maps:filter(Fun, Map).

%% Map function over values
map(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    maps:map(Fun, Map).
