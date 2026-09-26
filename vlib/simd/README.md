# SIMD vectors

The `simd` module provides `F32x4`, a fixed vector of four `f32` values. Its arithmetic
operators work on corresponding lanes. On the C backend, the module uses SSE on x86 when
available and NEON on AArch64. TCC and C targets without these extensions use scalar
operations with the same API. The non-C backend implementation is also scalar.

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
