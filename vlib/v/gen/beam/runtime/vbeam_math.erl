%% vbeam_math - Math operations for V language BEAM backend
%% Provides mathematical functions matching V's math module

-module(vbeam_math).
-export([abs/1, min/2, max/2, clamp/3,
         floor/1, ceil/1, round/1, trunc/1,
         sqrt/1, pow/2, exp/1, log/1, log10/1, log2/1,
         sin/1, cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2,
         sinh/1, cosh/1, tanh/1,
         pi/0, e/0, inf/0, nan/0]).

%% Absolute value
%% 
-spec abs(integer() | float()) -> number().
abs(X) when is_integer(X) -> erlang:abs(X);
abs(X) when is_float(X) -> erlang:abs(X).

%% Minimum of two values
%% 
-spec min(number(), number()) -> number().
min(A, B) when A < B -> A;
min(_, B) -> B.

%% Maximum of two values
%% 
-spec max(number(), number()) -> number().
max(A, B) when A > B -> A;
max(_, B) -> B.

%% Clamp value between min and max
%% 
-spec clamp(number(), number(), number()) -> number().
clamp(X, Min, Max) ->
    erlang:max(Min, erlang:min(Max, X)).

%% Floor (round toward negative infinity)
%% 
-spec floor(number()) -> number().
floor(X) when is_float(X) -> math:floor(X);
floor(X) when is_integer(X) -> X.

%% Ceiling (round toward positive infinity)
%% 
-spec ceil(number()) -> number().
ceil(X) when is_float(X) -> math:ceil(X);
ceil(X) when is_integer(X) -> X.

%% Round to nearest integer
%% 
-spec round(number()) -> number().
round(X) when is_float(X) -> erlang:round(X);
round(X) when is_integer(X) -> X.

%% Truncate (round toward zero)
%% 
-spec trunc(number()) -> number().
trunc(X) when is_float(X) -> erlang:trunc(X);
trunc(X) when is_integer(X) -> X.

%% Square root
%% 
-spec sqrt(number()) -> float().
sqrt(X) -> math:sqrt(X).

%% Power
%% 
-spec pow(number(), number()) -> float().
pow(Base, Exp) -> math:pow(Base, Exp).

%% Exponential (e^x)
%% 
-spec exp(number()) -> float().
exp(X) -> math:exp(X).

%% Natural logarithm
%% 
-spec log(number()) -> float().
log(X) -> math:log(X).

%% Base-10 logarithm
%% 
-spec log10(number()) -> float().
log10(X) -> math:log10(X).

%% Base-2 logarithm
%% 
-spec log2(number()) -> float().
log2(X) -> math:log2(X).

%% Trigonometric functions
%% 
-spec sin(number()) -> float().
sin(X) -> math:sin(X).
%% 
-spec cos(number()) -> float().
cos(X) -> math:cos(X).
%% 
-spec tan(number()) -> float().
tan(X) -> math:tan(X).
%% 
-spec asin(number()) -> float().
asin(X) -> math:asin(X).
%% 
-spec acos(number()) -> float().
acos(X) -> math:acos(X).
%% 
-spec atan(number()) -> float().
atan(X) -> math:atan(X).
%% 
-spec atan2(number(), number()) -> float().
atan2(Y, X) -> math:atan2(Y, X).

%% Hyperbolic functions
%% 
-spec sinh(number()) -> float().
sinh(X) -> math:sinh(X).
%% 
-spec cosh(number()) -> float().
cosh(X) -> math:cosh(X).
%% 
-spec tanh(number()) -> float().
tanh(X) -> math:tanh(X).

%% Constants
%% 
-spec pi() -> float().
pi() -> math:pi().
%% 
-spec e() -> float().
e() -> 2.718281828459045.
%% 
-spec inf() -> float().
inf() ->
    %% Keep returning a runtime +infinity value without triggering
    %% compile-time badarith analysis.
    1.0e308 * float(erlang:system_time(nanosecond)).
%% 
-spec nan() -> float().
nan() ->
    %% NaN (results in badarith, handle specially)
    inf() - inf().
