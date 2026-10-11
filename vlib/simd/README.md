# SIMD vectors

The `simd` module provides `F32x4`, a fixed vector of four `f32` values. Its arithmetic
operators work on corresponding lanes. On the C backend, the module uses SSE on x86 when
available and NEON on AArch64. TCC and C targets without these extensions use scalar
operations with the same API. On V's native backend (`-b native`) the operators are lowered
to single NEON instructions on the four lanes at once. Every other backend runs the scalar
bodies in `simd.v`.

```v
import simd

fn main() {
	a := simd.f32x4(1, 2, 3, 4)
	b := simd.broadcast_f32x4(2)
	println((a * b).to_array()) // [2.0, 4.0, 6.0, 8.0]
}
```

`load_f32x4` reads four values from a slice and `store` writes four values. Both return an
error if the slice is too short. For the final one to three values in a loop, use
`load_f32x4_part`, which zero-fills unused lanes, and `store_part`, which writes only as many
lanes as the destination contains. Partial operations reject slices longer than four.

`mul_add` computes a multiplication followed by an addition. It does not guarantee fused
rounding. The module currently covers four-lane `f32` arithmetic; `math.vec.Vec4` remains
the general-purpose geometric vector type. Compile with `-cflags -DV_SIMD_FORCE_SCALAR`
to use the scalar C implementation when comparing behavior or performance.

The fixed width follows the four-lane layout used by projects such as Viper. Go's
[portable SIMD design](https://go.dev/blog/simd-experiment) provides a reference for a
future size-independent API; this module does not yet offer runtime vector widths or
feature dispatch.

## How each backend gets its implementation

The operators name a helper function rather than inlining an implementation, because the
helper has one definition per backend family:

| backend | file | what runs |
| --- | --- | --- |
| `c` | `simd.c.v` + `simd.h` | the C functions, compiled by the C compiler with SSE or NEON |
| `native` (`-b native`) | `simd.v` | the native backend replaces the call with one NEON instruction |
| everything else | `simd.v` | the scalar body in `simd.v` |

`simd.c.v` is only compiled by the C backend, so the two files never compete for the same
name. The native substitution happens in `vlib/v/gen/arm64/gen.v`, where a call to one of
these helpers is matched by name and lowered to `fadd`/`fsub`/`fmul`/`fdiv`/`fsqrt` on four
`f32` lanes; every argument and result that is not exactly four `f32` values falls through
to the ordinary call, so the helper's own body remains the fallback rather than an
assumption. That also means the C and native paths are not both in play for one program:
each build takes exactly one of them.
