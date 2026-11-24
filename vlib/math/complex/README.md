# Complex arithmetic

`Complex` stores binary64 real and imaginary components. The elementary functions use
principal values with signed-zero branch cuts. In particular, `z.sqrt()` has a nonnegative
real part and its imaginary part has the sign of `z.im`.

The exponential and hyperbolic functions scale large intermediate values so that a finite
component can survive when the other component overflows. The logarithm avoids forming
an overflowing or underflowing squared magnitude and retains product residuals near the
unit circle. Inverse functions use the Hull, Fairgrieve and Tang algorithm to avoid
cancellation near their cuts. Complex powers use the principal logarithm and a scaled
exponential; their phase is rounded to binary64 before evaluating sine and cosine.
For positive real bases and finite real exponents, `pow` and `cpow` use real `math.pow`.
The power exponent calculation scales overflowing sums of finite products, preserving
zero or infinite results instead of introducing NaNs through the roundoff calculation.

The portable implementations work with hosted and freestanding builds. They preserve
nonfinite values and branch-cut signs but do not explicitly set floating-point status flags.
The kernels are adapted from [FreeBSD msun](https://github.com/freebsd/freebsd-src/tree/main/lib/msun/src);
the original license notices are retained in the source files.

The tests include signed-zero and exceptional-value checks, a hosted C libm differential
test, and 2,547 independent GNU MPC oracle rows. Regeneration instructions are in
`testdata/generate_elementary.c`; generating the fixture requires MPC, MPFR and GMP.
Ordinary tests have no dependency on these libraries. The generator checks every expected
rounded binary64 component at both 512 and 1,024 bits of precision.

`testdata/elementary_regressions.tsv` records finite inputs where macOS binary64 libm
comparisons disagreed; MPC supplies their expected results. Nonfinite expectations are
recorded separately in `testdata/elementary_special.tsv`, following
[ISO C Annex G](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf) and its
[clarifications](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1867.htm).
The differential tests prefer these independent fixtures when available and use host
libm for other inputs on compilers with C99 complex support. TinyCC and MSVC use the fixtures.
