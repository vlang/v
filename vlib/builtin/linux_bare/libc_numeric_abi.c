#include "libc_compat.h"
#include <fenv.h>
#include <float.h>
#include <math.h>
#include <stdint.h>

/* Only native floating argument/return ABIs; all bit operations live in V. */
_Static_assert(__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__, "native floating byte order");
_Static_assert(FLT_MANT_DIG == 24 && DBL_MANT_DIG == 53, "native float/double ABI");
_Static_assert(LDBL_MANT_DIG == 53 || LDBL_MANT_DIG == 64 || LDBL_MANT_DIG == 113,
               "native long double ABI");
extern int32_t v_native_frexp(uintptr_t, int32_t, uintptr_t);
extern void v_native_float_sign(uintptr_t, uintptr_t, int32_t);
extern int32_t v_native_round_get(void);
extern int32_t v_native_round_set(int32_t);
/* Native addition quiets signaling NaNs and observes the hardware default-NaN mode. */
float frexpf(float value, int *exponent) {
    if (v_native_frexp((uintptr_t)&value, FLT_MANT_DIG, (uintptr_t)exponent)) return value + value;
    return value;
}
double frexp(double value, int *exponent) {
    if (v_native_frexp((uintptr_t)&value, DBL_MANT_DIG, (uintptr_t)exponent)) return value + value;
    return value;
}
long double frexpl(long double value, int *exponent) {
    int native_special = v_native_frexp((uintptr_t)&value, LDBL_MANT_DIG, (uintptr_t)exponent);
    if (LDBL_MANT_DIG != 113 && native_special) return value + value;
    return value;
}
int fegetround(void) { return v_native_round_get(); }
int fesetround(int mode) { return v_native_round_set(mode); }
extern void v_fenv_get(uintptr_t);
extern uint32_t v_fenv_flags(int32_t);
extern int32_t v_fenv_flags_set(uint32_t,int32_t);
extern int32_t v_fenv_set(uintptr_t);
extern int32_t v_fenv_hold(uintptr_t);
extern int32_t v_fenv_enabled(int32_t,int32_t);
extern int32_t v_fenv_raise(int32_t);
extern int32_t v_fenv_update(uintptr_t);
#if defined(__x86_64__)
_Static_assert(sizeof(fenv_t)==32 && sizeof(fexcept_t)==2,"native x87/MXCSR fenv ABI");
#elif defined(__aarch64__)
_Static_assert(sizeof(fenv_t)==8 && sizeof(fexcept_t)==4,"native FPCR/FPSR fenv ABI");
#endif
int fegetenv(fenv_t *environment) { v_fenv_get((uintptr_t)environment); return 0; }
int fesetenv(const fenv_t *environment) { return v_fenv_set((uintptr_t)environment); }
int feholdexcept(fenv_t *environment) { return v_fenv_hold((uintptr_t)environment); }
int feupdateenv(const fenv_t *environment) { return v_fenv_update((uintptr_t)environment); }
int fetestexcept(int mask) { return (int)v_fenv_flags(mask); }
int feclearexcept(int mask) { return v_fenv_flags_set(0,mask); }
int fegetexceptflag(fexcept_t *out,int mask) { *out=(fexcept_t)v_fenv_flags(mask); return 0; }
int fesetexceptflag(const fexcept_t *flags,int mask) { return v_fenv_flags_set(*flags,mask); }
int feraiseexcept(int mask) { return v_fenv_raise(mask); }
int fegetexcept(void) { return v_fenv_enabled(0,0); }
int feenableexcept(int mask) { return v_fenv_enabled(mask,1); }
int fedisableexcept(int mask) { return v_fenv_enabled(mask,-1); }
float fabsf(float value) { v_native_float_sign((uintptr_t)&value,0,FLT_MANT_DIG); return value; }
long double fabsl(long double value) { v_native_float_sign((uintptr_t)&value,0,LDBL_MANT_DIG); return value; }
float copysignf(float value,float sign) { v_native_float_sign((uintptr_t)&value,(uintptr_t)&sign,FLT_MANT_DIG); return value; }
double copysign(double value,double sign) { v_native_float_sign((uintptr_t)&value,(uintptr_t)&sign,DBL_MANT_DIG); return value; }
long double copysignl(long double value,long double sign) { v_native_float_sign((uintptr_t)&value,(uintptr_t)&sign,LDBL_MANT_DIG); return value; }
