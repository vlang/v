## Description

`math` provides commonly used mathematical functions for
trigonometry, logarithms, etc.

`factorial(n)` returns `n!` for nonnegative integers and uses `gamma(n + 1)` for other
values. When the result overflows `f64`, it returns positive infinity, including for
`n >= 171` and positive infinity. Use `is_inf(result, 1)` to detect this overflow;
`factorial(170)` remains finite.
