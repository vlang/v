# SIMD vectors

The `simd` module provides fixed-size vectors of numbers and lane masks. Operations work on
corresponding lanes. With gcc and clang they compile to the target's SIMD instructions (SSE/AVX,
NEON, ...); other C compilers and the non-C backends use scalar loops with the same results.

```v
import simd

fn main() {
	a := simd.f32x4(1, 2, 3, 4)
	b := simd.splat_f32x4(2)
	println((a * b).to_array()) // [2.0, 4.0, 6.0, 8.0]
	println(a.dot(b)) // 20.0
}
```

## Types

A vector type is named after its lane type and lane count: `F32x8` holds eight `f32` lanes.

| Lane type | 64 bits | 128 bits | 256 bits | 512 bits |
| --- | --- | --- | --- | --- |
| `f32` | `F32x2` | `F32x4` | `F32x8` | `F32x16` |
| `f64` | | `F64x2` | `F64x4` | `F64x8` |
| `i8`, `u8` | `I8x8`, `U8x8` | `I8x16`, `U8x16` | `I8x32`, `U8x32` | `I8x64`, `U8x64` |
| `i16`, `u16` | `I16x4`, `U16x4` | `I16x8`, `U16x8` | `I16x16`, `U16x16` | `I16x32`, `U16x32` |
| `i32`, `u32` | `I32x2`, `U32x2` | `I32x4`, `U32x4` | `I32x8`, `U32x8` | `I32x16`, `U32x16` |
| `i64`, `u64` | | `I64x2`, `U64x2` | `I64x4`, `U64x4` | `I64x8`, `U64x8` |

Comparisons return a mask named after the lane width and count: `Mask32x4` belongs to `F32x4`,
`I32x4` and `U32x4`, and `Mask8x16` to `I8x16` and `U8x16`. V has no `f16` type, so there are no
`f16` vectors.

## Operations

In the list below, `T` stands for a vector type such as `F32x8` and `t` for its lower-case name.

- Creating: `simd.t(lanes...)` for up to eight lanes, `simd.splat_t(x)`,
  `simd.from_array_t(arr)`.
- Loading and storing: `simd.load_t_at(src, offset)` and `v.store_at(mut dst, offset)` panic when
  the slice is too short. `simd.load_t(src)` and `v.store(mut dst)` return an error instead, and
  `simd.load_t_part(src)` and `v.store_part(mut dst)` handle the final lanes of a loop:
  `load_t_part` zero-fills the missing lanes and both reject slices longer than a vector.
- Lanes: `v[i]`, `v[i] = x` (they panic when `i` is out of range) and `v.to_array()`.
- Arithmetic: `+ - * /` and the methods `add sub mul div`, `minimum`, `maximum`; `neg` and `abs`
  for signed and float vectors; `sqrt` and `mul_add` for float vectors.
- Bitwise operations on integer vectors: `and or xor not`, `shl(count)` and `shr(count)`.
- Comparisons: `eq ne lt le gt ge` return a mask.
- Reductions: `sum`, `min`, `max`, `min_element`, `max_element` and `a.dot(b)`.
- Conversions between lane types of the same width keep the lane count, for example
  `F32x4.to_i32x4`, `I32x4.to_f32x4`, `I32x4.to_u32x4` and `I8x16.to_u8x16`.
- Masks: `simd.splat_mask32x4(b)`, `simd.from_array_mask32x4(arr)`, `m[i]`, `m[i] = b`,
  `m.to_array()`, `all`, `any`, `count`, `and`, `or`, `xor` and `not`.
- Selecting: `simd.select_t(mask, a, b)` takes the lanes of `a` where the mask is set and the
  lanes of `b` elsewhere.

`==` compares whole vectors bit for bit, as V does for every struct. Use `eq` for a lane mask.

Masks and `select` make branch-free loops possible:

```v
import simd

// clamp_negatives replaces the negative values of data by zero.
fn clamp_negatives(mut data []f32) {
	zero := simd.splat_f32x8(0)
	mut i := 0
	for ; i + 8 <= data.len; i += 8 {
		v := simd.load_f32x8_at(data, i)
		simd.select_f32x8(v.lt(zero), zero, v).store_at(mut data, i)
	}
	tail := simd.load_f32x8_part(data[i..]) or { panic(err) }
	simd.select_f32x8(tail.lt(zero), zero, tail).store_part(mut data[i..]) or { panic(err) }
}

fn main() {
	mut data := []f32{len: 11, init: f32(index) - 5}
	clamp_negatives(mut data)
	println(data) // [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
	a := simd.i32x4(3, -7, 8, 0)
	big := a.gt(simd.splat_i32x4(2))
	println('${big.count()} ${big.any()} ${big.all()}') // 2 true false
}
```

## Semantics

- Integer `+ - *`, `neg`, `abs`, `sum` and `dot` wrap on overflow. Integer `/` truncates toward
  zero and panics when a lane of the divisor is zero; `min / -1` wraps to `min`.
- `shl` and an unsigned `shr` give zero lanes for a count of the lane width or more. A signed
  `shr` is arithmetic and fills the lanes with their sign bit for such counts.
- Float operations follow IEEE 754. `minimum` and `maximum` return the other lane when one lane is
  NaN, and the lane of the receiver when the lanes compare equal (such as `-0.0` and `0.0`).
- `min_element` and `max_element` return the index of the first lane holding the smallest or
  largest value, and `min` and `max` return that lane. For float vectors, NaN lanes are ignored,
  and index 0 is returned when every lane is NaN.
- `sum` and `dot` add in lane order, so the vector and scalar paths round identically.
  `mul_add` and `dot` do not promise fused rounding: the C compiler may contract them when its
  flags allow it, as GCC does by default.
- Float to integer conversions truncate toward zero, map NaN to 0 and saturate out-of-range
  values. Integer to float conversions round to nearest. Conversions between signed and unsigned
  lanes keep the bits.

## Lowering

The vectors are structs holding a fixed array, so they can be stored in arrays and structs and
need no particular alignment. On the C backend, element-wise operations call the helpers in
`simd.h`: with gcc and clang these load the lanes into vectors declared with
`__attribute__((vector_size(N)))`, and the C compiler splits vectors wider than the target's
registers. Pass for example `-cflags -mavx2` to use 256-bit instructions on x86-64. TCC, MSVC and
builds with `-cflags -DV_SIMD_FORCE_SCALAR` use scalar C loops; reductions, integer division and
conversions are scalar V code on every path. The non-C backends use scalar V code throughout.

`vectors_generated.v`, `vectors_generated.c.v`, `simd.h` and `vectors_generated_test.v` are
written by `gen.vsh`. After editing it, regenerate them with `v run vlib/simd/gen.vsh`.
