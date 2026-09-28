#ifndef VLIB_SIMD_SIMD_H
#define VLIB_SIMD_SIMD_H

#include <math.h>

#if !defined(V_SIMD_FORCE_SCALAR) && !defined(__TINYC__) && defined(__SSE__)
#include <xmmintrin.h>
#define V_SIMD_HAS_SSE 1
#elif !defined(V_SIMD_FORCE_SCALAR) && !defined(__TINYC__) && defined(__aarch64__)
#include <arm_neon.h>
#define V_SIMD_HAS_NEON 1
#endif

static inline void v_simd_add_f32x4(const float *a, const float *b, float *out) {
#if defined(V_SIMD_HAS_SSE)
    _mm_storeu_ps(out, _mm_add_ps(_mm_loadu_ps(a), _mm_loadu_ps(b)));
#elif defined(V_SIMD_HAS_NEON)
    vst1q_f32(out, vaddq_f32(vld1q_f32(a), vld1q_f32(b)));
#else
    for (int i = 0; i < 4; ++i) out[i] = a[i] + b[i];
#endif
}

static inline void v_simd_sub_f32x4(const float *a, const float *b, float *out) {
#if defined(V_SIMD_HAS_SSE)
    _mm_storeu_ps(out, _mm_sub_ps(_mm_loadu_ps(a), _mm_loadu_ps(b)));
#elif defined(V_SIMD_HAS_NEON)
    vst1q_f32(out, vsubq_f32(vld1q_f32(a), vld1q_f32(b)));
#else
    for (int i = 0; i < 4; ++i) out[i] = a[i] - b[i];
#endif
}

static inline void v_simd_mul_f32x4(const float *a, const float *b, float *out) {
#if defined(V_SIMD_HAS_SSE)
    _mm_storeu_ps(out, _mm_mul_ps(_mm_loadu_ps(a), _mm_loadu_ps(b)));
#elif defined(V_SIMD_HAS_NEON)
    vst1q_f32(out, vmulq_f32(vld1q_f32(a), vld1q_f32(b)));
#else
    for (int i = 0; i < 4; ++i) out[i] = a[i] * b[i];
#endif
}

static inline void v_simd_div_f32x4(const float *a, const float *b, float *out) {
#if defined(V_SIMD_HAS_SSE)
    _mm_storeu_ps(out, _mm_div_ps(_mm_loadu_ps(a), _mm_loadu_ps(b)));
#elif defined(V_SIMD_HAS_NEON)
    vst1q_f32(out, vdivq_f32(vld1q_f32(a), vld1q_f32(b)));
#else
    for (int i = 0; i < 4; ++i) out[i] = a[i] / b[i];
#endif
}

static inline void v_simd_sqrt_f32x4(const float *a, float *out) {
#if defined(V_SIMD_HAS_SSE)
    _mm_storeu_ps(out, _mm_sqrt_ps(_mm_loadu_ps(a)));
#elif defined(V_SIMD_HAS_NEON)
    vst1q_f32(out, vsqrtq_f32(vld1q_f32(a)));
#else
    for (int i = 0; i < 4; ++i) out[i] = sqrtf(a[i]);
#endif
}

#endif
