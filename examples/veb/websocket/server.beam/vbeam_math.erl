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
abs(X) when is_integer(X) -> erlang:abs(X);
abs(X) when is_float(X) -> erlang:abs(X).

%% Minimum of two values
min(A, B) when A < B -> A;
min(_, B) -> B.

%% Maximum of two values
max(A, B) when A > B -> A;
max(_, B) -> B.

%% Clamp value between min and max
clamp(X, Min, Max) ->
    max(Min, min(Max, X)).

%% Floor (round toward negative infinity)
floor(X) when is_float(X) -> math:floor(X);
floor(X) when is_integer(X) -> X.

%% Ceiling (round toward positive infinity)
ceil(X) when is_float(X) -> math:ceil(X);
ceil(X) when is_integer(X) -> X.

%% Round to nearest integer
round(X) when is_float(X) -> erlang:round(X);
round(X) when is_integer(X) -> X.

%% Truncate (round toward zero)
trunc(X) when is_float(X) -> erlang:trunc(X);
trunc(X) when is_integer(X) -> X.

%% Square root
sqrt(X) -> math:sqrt(X).

%% Power
pow(Base, Exp) -> math:pow(Base, Exp).

%% Exponential (e^x)
exp(X) -> math:exp(X).

%% Natural logarithm
log(X) -> math:log(X).

%% Base-10 logarithm
log10(X) -> math:log10(X).

%% Base-2 logarithm
log2(X) -> math:log2(X).

%% Trigonometric functions
sin(X) -> math:sin(X).
cos(X) -> math:cos(X).
tan(X) -> math:tan(X).
asin(X) -> math:asin(X).
acos(X) -> math:acos(X).
atan(X) -> math:atan(X).
atan2(Y, X) -> math:atan2(Y, X).

%% Hyperbolic functions
sinh(X) -> math:sinh(X).
cosh(X) -> math:cosh(X).
tanh(X) -> math:tanh(X).

%% Constants
pi() -> math:pi().
e() -> 2.718281828459045.
inf() -> 1.0e308 * 10.0.  %% Positive infinity
nan() -> 0.0 / 0.0.        %% NaN (results in badarith, handle specially)
