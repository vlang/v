# math.stats

`correlation` computes the Pearson coefficient for paired numeric samples and returns `f64`.
Lengths must match; empty or constant input returns NaN. `linear_regression` returns a
`LinearRegression[T]` with `f64` coefficients and predictions, including for integer samples.
It needs at least two paired samples with distinct x values.

`quantiles(sorted_data, n)` returns `n - 1` cut points as `[]f64`, using inclusive linear
interpolation between ascending sorted samples. At least two samples and `n >= 1` are required.
`multimode` groups values by equality and returns all modes in first-seen order.

`new_normal_dist(mean, std)` requires finite parameters and `std >= 0`; zero denotes a point
mass. `pdf`, `cdf` and `inv_cdf` evaluate that distribution. Inverse probabilities must lie
strictly between zero and one. Extreme-tail inverse values use a finite rational approximation;
ordinary inputs also receive a Halley refinement. A NaN evaluation input returns NaN.

Gaussian evaluation scales differences, densities, and affine inverse values to avoid
intermediate overflow or underflow for finite parameters. Final values can still overflow or
underflow when the mathematical result lies outside the floating-point range.
