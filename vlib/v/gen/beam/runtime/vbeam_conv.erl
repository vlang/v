%% vbeam_conv - Type conversion operations for V language BEAM backend
%% Provides conversions between V types and their Erlang representations

-module(vbeam_conv).

-moduledoc """
Provides V-to-Erlang conversion and formatting helpers.
""".






-export([to_binary/1, string_to_int/1, string_to_float/1,
         int_to_string/1, float_to_string/1, bool_to_string/1,
         int_to_hex/1, hex_to_int/1, format_int/2, interpolate/1]).

%% Convert any V value to binary (string) representation
-doc """
to_binary/1 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `term()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec to_binary(term()) -> binary().

to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_binary(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 6}, compact]);
to_binary(true) ->
    <<"true">>;
to_binary(false) ->
    <<"false">>;
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(List) when is_list(List) ->
    %% For lists, try to convert as string or format as array
    try
        list_to_binary(List)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [List]))
    end;
to_binary(Map) when is_map(Map) ->
    %% For maps (V structs), format as inspection string
    list_to_binary(io_lib:format("~p", [Map]));
to_binary(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

%% String to integer conversion
%% V's string.int() equivalent
-doc """
string_to_int/1 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `binary() | string()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec string_to_int(binary() | string()) -> integer().

string_to_int(Bin) when is_binary(Bin) ->
    Int = try
        binary_to_integer(Bin)
    catch
        _:_ -> 0
    end,
    true = is_integer(Int),
    Int;
string_to_int(List) when is_list(List) ->
    Int = try
        list_to_integer(List)
    catch
        _:_ -> 0
    end,
    true = is_integer(Int),
    Int.

%% String to float conversion
%% V's string.f64() equivalent
-doc """
string_to_float/1 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `binary() | string()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec string_to_float(binary() | string()) -> float().

string_to_float(Bin) when is_binary(Bin) ->
    Float = try
        binary_to_float(Bin)
    catch
        error:badarg ->
            %% Try parsing as integer and converting
            try
                float(binary_to_integer(Bin))
            catch
                _:_ -> 0.0
            end
    end,
    true = is_float(Float),
    Float;
string_to_float(List) when is_list(List) ->
    Float = try
        list_to_float(List)
    catch
        error:badarg ->
            try
                float(list_to_integer(List))
            catch
                _:_ -> 0.0
            end
    end,
    true = is_float(Float),
    Float.

%% Integer to string (binary)
-doc """
int_to_string/1 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `integer()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec int_to_string(integer()) -> binary().

int_to_string(Int) when is_integer(Int) ->
    Bin = integer_to_binary(Int),
    true = is_binary(Bin),
    Bin.

%% Float to string (binary) with 6 decimal places, compact output
-spec float_to_string(float()) -> binary().

float_to_string(Float) when is_float(Float), Float =:= Float ->
    Bin = float_to_binary(Float, [{decimals, 6}, compact]),
    true = is_binary(Bin),
    Bin.

%% Bool to string (binary)
-doc """
bool_to_string/1 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `boolean()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec bool_to_string(boolean()) -> binary().
bool_to_string(true) -> <<"true">>;
bool_to_string(false) -> <<"false">>.

%% Integer to hexadecimal string
-spec int_to_hex(integer()) -> binary().
int_to_hex(Int) when is_integer(Int) ->
    Hex = integer_to_binary(Int, 16),
    true = is_binary(Hex),
    Hex.

%% Parse hexadecimal string to integer
-spec hex_to_int(binary()) -> integer().
hex_to_int(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    Int = binary_to_integer(Bin, 16),
    true = is_integer(Int),
    Int.

%% Format integer in given base (2-36)
-doc """
format_int/2 is a public runtime entrypoint in `vbeam_conv`.
Parameters: `integer()`, `integer()`.
Returns the result value of this runtime operation.
Side effects: No external side effects are introduced by this function.
""".
-spec format_int(integer(), integer()) -> binary().
format_int(Int, Base) when is_integer(Int), is_integer(Base), Base >= 2, Base =< 36 ->
    integer_to_binary(Int, Base).

%% V string interpolation helper
%% Takes a list of values, converts each to string, concatenates them
%% Used for V's string interpolation: 'value is ${x}' -> interpolate([<<"value is ">>, X])
-spec interpolate([term()]) -> binary().
interpolate(Parts) when is_list(Parts) ->
    Bins = [to_binary(Part) || Part <- Parts],
    true = lists:all(fun is_binary/1, Bins),
    iolist_to_binary(Bins).






