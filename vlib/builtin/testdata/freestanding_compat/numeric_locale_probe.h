#include <fenv.h>
#include <float.h>
#include <langinfo.h>
#include <math.h>
#ifndef FLT_TRUE_MIN
#define FLT_TRUE_MIN (FLT_MIN * FLT_EPSILON)
#define DBL_TRUE_MIN (DBL_MIN * DBL_EPSILON)
#define LDBL_TRUE_MIN (LDBL_MIN * LDBL_EPSILON)
#endif

static float (*volatile native_frexpf)(float,int *)=frexpf;
static double (*volatile native_frexp)(double,int *)=frexp;
static long double (*volatile native_frexpl)(long double,int *)=frexpl;
static int (*volatile native_setround)(int)=fesetround;
static int (*volatile native_getround)(void)=fegetround;
static float (*volatile native_copysignf)(float,float)=copysignf;
static double (*volatile native_copysign)(double,double)=copysign;
static long double (*volatile native_copysignl)(long double,long double)=copysignl;
static float (*volatile native_fabsf)(float)=fabsf;
static double (*volatile native_fabs)(double)=fabs;
static long double (*volatile native_fabsl)(long double)=fabsl;
static int (*volatile native_getenv)(fenv_t *)=fegetenv;
static int (*volatile native_setenv)(const fenv_t *)=fesetenv;
static int (*volatile native_hold)(fenv_t *)=feholdexcept;
static int (*volatile native_update)(const fenv_t *)=feupdateenv;
static int (*volatile native_test)(int)=fetestexcept;
static int (*volatile native_clear)(int)=feclearexcept;
static int (*volatile native_raise)(int)=feraiseexcept;
static int (*volatile native_getflag)(fexcept_t *,int)=fegetexceptflag;
static int (*volatile native_setflag)(const fexcept_t *,int)=fesetexceptflag;
static int (*volatile native_enabled)(void)=fegetexcept;
static int (*volatile native_enable)(int)=feenableexcept;
static int (*volatile native_disable)(int)=fedisableexcept;

static int native_environment_probe(void) {
    fenv_t original; CHECK(!native_getenv(&original));
    CHECK(!native_setenv(FE_DFL_ENV) && native_getround()==FE_TONEAREST && !native_test(FE_ALL_EXCEPT) && !native_enabled());
    struct { uint64_t before; fenv_t value; uint64_t after; } guarded={.before=0x12345678,.after=0x76543210};
    errno=E2BIG; CHECK(!native_getenv(&guarded.value) && errno==E2BIG);
    CHECK(guarded.before==0x12345678 && guarded.after==0x76543210);
    const int flags[]={FE_INVALID,FE_DIVBYZERO,FE_OVERFLOW,FE_UNDERFLOW,FE_INEXACT};
    for(size_t i=0;i<sizeof flags/sizeof *flags;i++) {
        CHECK(!native_clear(FE_ALL_EXCEPT) && !native_test(FE_ALL_EXCEPT));
        CHECK(!native_raise(flags[i]) && (native_test(FE_ALL_EXCEPT)&flags[i])==flags[i]);
        /* C permits overflow/underflow raising inexact as well. */
        CHECK(!(native_test(FE_ALL_EXCEPT)&~(flags[i]|FE_INEXACT)));
        struct { uint16_t before; fexcept_t value; uint16_t after; } bits={.before=0x1234,.after=0x7654};
        CHECK(!native_getflag(&bits.value,FE_ALL_EXCEPT) && (bits.value&flags[i])==flags[i]);
        CHECK(bits.before==0x1234 && bits.after==0x7654);
    }
    CHECK(!native_clear(FE_ALL_EXCEPT) && !native_raise(FE_INVALID|FE_DIVBYZERO));
    fexcept_t both; CHECK(!native_getflag(&both,FE_ALL_EXCEPT));
    CHECK(!native_clear(FE_INVALID) && native_test(FE_ALL_EXCEPT)==FE_DIVBYZERO);
    CHECK(!native_clear(FE_ALL_EXCEPT) && !native_setflag(&both,FE_INVALID) && native_test(FE_ALL_EXCEPT)==FE_INVALID);
    CHECK(!native_setflag(&both,FE_DIVBYZERO) && native_test(FE_ALL_EXCEPT)==(FE_INVALID|FE_DIVBYZERO));
    fexcept_t zero=0; CHECK(!native_setflag(&zero,FE_INVALID) && native_test(FE_ALL_EXCEPT)==FE_DIVBYZERO);
    CHECK(!native_setenv(FE_DFL_ENV));
    CHECK(!native_setround(FE_DOWNWARD) && !native_raise(FE_DIVBYZERO));
    fenv_t saved,held; CHECK(!native_getenv(&saved) && !native_hold(&held));
    CHECK(native_getround()==FE_DOWNWARD && !native_enabled() && !native_test(FE_ALL_EXCEPT));
    CHECK(!native_setround(FE_UPWARD) && !native_raise(FE_INVALID));
    CHECK(!native_update(&saved) && native_getround()==FE_DOWNWARD && native_test(FE_ALL_EXCEPT)==(FE_INVALID|FE_DIVBYZERO));
    CHECK(!native_setenv(&held) && native_test(FE_ALL_EXCEPT)==FE_DIVBYZERO);
    CHECK(!native_setenv(FE_DFL_ENV));
    int enabled=native_enable(FE_INVALID);
    if(enabled>=0) {
        CHECK(!enabled && native_enabled()==FE_INVALID);
        /* Flag restoration records an enabled exception without raising it. */
        CHECK(!native_setflag(&both,FE_INVALID) && native_test(FE_INVALID)==FE_INVALID);
        CHECK(!native_setflag(&zero,FE_INVALID));
        CHECK(!native_hold(&held) && !native_enabled());
        CHECK(!native_setenv(&held) && native_enabled()==FE_INVALID);
        CHECK(native_disable(FE_INVALID)==FE_INVALID && !native_enabled());
    }
#if defined(__x86_64__)
    /* A saved environment's old TOP/tag bits must not replace live registers. */
    CHECK(!native_getenv(&saved));
    long double first=1.25L,second=-7.5L,out_first=0,out_second=0;
    __asm__ volatile("fldt %0; fldt %1"::"m"(first),"m"(second):"st","st(1)");
    CHECK(!native_setenv(&saved));
    __asm__ volatile("fstpt %0; fstpt %1":"=m"(out_second),"=m"(out_first)::"st","st(1)");
    CHECK(out_first==first && out_second==second);
    CHECK(!native_setenv(FE_NOMASK_ENV) && native_enabled()==FE_ALL_EXCEPT && native_getround()==FE_TONEAREST);
    CHECK(!native_setenv(FE_DFL_ENV));
#elif defined(__aarch64__) && defined(FE_NOMASK_ENV)
    int installed = native_setenv(FE_NOMASK_ENV);
    CHECK((installed == 0) == (native_enabled() == FE_ALL_EXCEPT));
    CHECK(!native_setenv(FE_DFL_ENV));
    installed = native_update(FE_NOMASK_ENV);
    CHECK((installed == 0) == (native_enabled() == FE_ALL_EXCEPT));
    CHECK(!native_setenv(FE_DFL_ENV));
#endif
    for(size_t i=0;i<sizeof flags/sizeof *flags;i++) {
        pid_t child=fork(); CHECK(child>=0);
        if(!child) {
            struct rlimit no_core={0,0}; setrlimit(RLIMIT_CORE,&no_core); signal(SIGFPE,SIG_DFL);
            native_setenv(FE_DFL_ENV);
            if(native_enable(flags[i])<0)_exit(77);
            native_raise(flags[i]); _exit(99);
        }
        int status=0; CHECK(waitpid(child,&status,0)==child);
#if defined(__aarch64__)
        if(WIFEXITED(status) && WEXITSTATUS(status)==77)continue;
#endif
        CHECK(WIFSIGNALED(status) && WTERMSIG(status)==SIGFPE);
    }
    CHECK(!native_setenv(&original));
    return 0;
}

#ifndef V_RUNTIME_HOSTED_ORACLE
static int native_extended_bits_probe(void) {
    extern int32_t v_native_frexp(uintptr_t,int32_t,uintptr_t);
    extern void v_native_float_sign(uintptr_t,uintptr_t,int32_t);
    /* IEEE binary128 vectors exercise its path even on an x87 host. */
    const struct { uint64_t low,high,out_low,out_high; int exponent; } vectors[]={
        {0,0,0,0,0},{0,0x8000000000000000ULL,0,0x8000000000000000ULL,0},
        {0,0x3fff000000000000ULL,0,0x3ffe000000000000ULL,1},
        {1,0,0,0x3ffe000000000000ULL,-16493},
        {0x8000000000000000ULL,0,0,0x3ffe000000000000ULL,-16430},
        {UINT64_MAX,0x0000ffffffffffffULL,0xfffffffffffffffeULL,0x3ffeffffffffffffULL,-16382},
        {0,0x0001000000000000ULL,0,0x3ffe000000000000ULL,-16381},
        {UINT64_MAX,0x7ffeffffffffffffULL,UINT64_MAX,0x3ffeffffffffffffULL,16384},
        {0,0x7fff000000000000ULL,0,0x7fff000000000000ULL,0},
        {5,0x7fff800000000001ULL,5,0x7fff800000000001ULL,0}
    };
    for(size_t i=0;i<sizeof vectors/sizeof *vectors;i++) {
        struct { uint64_t before,words[2],after; } guarded={0x12345678,{vectors[i].low,vectors[i].high},0x76543210};
        int exponent=99; v_native_frexp((uintptr_t)guarded.words,113,(uintptr_t)&exponent);
        CHECK(guarded.words[0]==vectors[i].out_low && guarded.words[1]==vectors[i].out_high && exponent==vectors[i].exponent);
        CHECK(guarded.before==0x12345678 && guarded.after==0x76543210);
    }
    uint64_t extended[2]={1,0xa5a5a5a5a5a50000ULL}; int exponent=99;
    v_native_frexp((uintptr_t)extended,64,(uintptr_t)&exponent);
    CHECK(exponent==-16444 && extended[0]==0x8000000000000000ULL && extended[1]==0xa5a5a5a5a5a53ffeULL);
    uint64_t negative[2]={0,0x8000000000008000ULL};
    v_native_float_sign((uintptr_t)extended,(uintptr_t)negative,64);
    CHECK(extended[1]==0xa5a5a5a5a5a5bffeULL);
    extended[0]=17; extended[1]=0x7fff800000000001ULL;
    v_native_float_sign((uintptr_t)extended,(uintptr_t)negative,113);
    CHECK(extended[0]==17 && extended[1]==0xffff800000000001ULL);
    return 0;
}
#endif

static int decomposition_probe(void) {
#ifndef V_RUNTIME_HOSTED_ORACLE
    CHECK(!native_extended_bits_probe());
#endif
    CHECK(signbit(native_copysignf(0.0f,-1.0f)) && !signbit(native_fabsf(-0.0f)));
    CHECK(signbit(native_copysign(0.0,-1.0)) && !signbit(native_fabs(-0.0)));
    CHECK(signbit(native_copysignl(0.0L,-1.0L)) && !signbit(native_fabsl(-0.0L)));
    CHECK(native_copysignf(-3.25f,0.0f)==3.25f && native_copysign(3.25,-0.0)==-3.25);
    CHECK(native_copysignl(-0x1.000000000000001p0L,1.0L)==0x1.000000000000001p0L);
    CHECK(isnan(native_copysignf(NAN,-1.0f)) && signbit(native_copysignf(NAN,-1.0f)));
    CHECK(isnan(native_fabsl(-NAN)) && !signbit(native_fabsl(-NAN)));
    const float values_f[]={0.0f,-0.0f,.5f,1.0f,-7.75f,FLT_MIN,FLT_TRUE_MIN,FLT_MAX,INFINITY,-INFINITY,NAN};
    const double values_d[]={0.0,-0.0,.5,1.0,-7.75,DBL_MIN,DBL_TRUE_MIN,DBL_MAX,INFINITY,-INFINITY,NAN};
    const long double values_l[]={0.0L,-0.0L,.5L,1.0L,-7.75L,LDBL_MIN,LDBL_TRUE_MIN,LDBL_MAX,INFINITY,-INFINITY,NAN,
        0x1.000000000000001p0L,-0x1.fffffffffffffffep0L};
    for(size_t i=0;i<sizeof values_f/sizeof *values_f;i++) {
        int exponent=99; errno=EDOM; float part=native_frexpf(values_f[i],&exponent); CHECK(errno==EDOM);
        if(isnan(values_f[i])) { CHECK(isnan(part)); continue; }
        CHECK(!!signbit(part)==!!signbit(values_f[i]));
        if(!isfinite(values_f[i]) || !values_f[i]) { CHECK(part==values_f[i]); if(!part)CHECK(exponent==0); continue; }
        CHECK(fabsf(part)>=.5f && fabsf(part)<1.0f && scalbn((double)part,exponent)==(double)values_f[i]);
        if(i==5)CHECK(exponent==FLT_MIN_EXP); if(i==6)CHECK(exponent==FLT_MIN_EXP-FLT_MANT_DIG+1);
        if(i==7)CHECK(exponent==FLT_MAX_EXP);
    }
    for(size_t i=0;i<sizeof values_d/sizeof *values_d;i++) {
        int exponent=99; errno=EDOM; double part=native_frexp(values_d[i],&exponent); CHECK(errno==EDOM);
        if(isnan(values_d[i])) { CHECK(isnan(part)); continue; }
        CHECK(!!signbit(part)==!!signbit(values_d[i]));
        if(!isfinite(values_d[i]) || !values_d[i]) { CHECK(part==values_d[i]); if(!part)CHECK(exponent==0); continue; }
        CHECK(fabs(part)>=.5 && fabs(part)<1.0 && scalbn(part,exponent)==values_d[i]);
        if(i==5)CHECK(exponent==DBL_MIN_EXP); if(i==6)CHECK(exponent==DBL_MIN_EXP-DBL_MANT_DIG+1);
        if(i==7)CHECK(exponent==DBL_MAX_EXP);
    }
    for(size_t i=0;i<sizeof values_l/sizeof *values_l;i++) {
        int exponent=99; errno=EDOM; long double part=native_frexpl(values_l[i],&exponent); CHECK(errno==EDOM);
        if(isnan(values_l[i])) { CHECK(isnan(part)); continue; }
        CHECK(!!signbit(part)==!!signbit(values_l[i]));
        if(!isfinite(values_l[i]) || !values_l[i]) { CHECK(part==values_l[i]); if(!part)CHECK(exponent==0); continue; }
        CHECK(fabsl(part)>=.5L && fabsl(part)<1.0L && scalbnl(part,exponent)==values_l[i]);
        if(i==5)CHECK(exponent==LDBL_MIN_EXP); if(i==6)CHECK(exponent==LDBL_MIN_EXP-LDBL_MANT_DIG+1);
        if(i==7)CHECK(exponent==LDBL_MAX_EXP);
    }
    uint64_t random=0x29144fbee1673901ULL;
    for(int i=0;i<2048;i++) {
        random^=random<<13; random^=random>>7; random^=random<<17;
        double value; memcpy(&value,&random,sizeof value);
        if(!isfinite(value) || !value)continue;
        int exponent; double part=native_frexp(value,&exponent);
        CHECK(fabs(part)>=.5 && fabs(part)<1.0 && scalbn(part,exponent)==value);
    }
    return 0;
}

static int frexp_nan_probe(int default_nan) {
    int exponent;
    float sf = __builtin_nansf("1"), qf = __builtin_nanf("1");
    double sd = __builtin_nans("1"), qd = __builtin_nan("1");
    long double sl = __builtin_nansl("1"), ql = __builtin_nanl("1");
    float expected_f = default_nan ? __builtin_nanf("") : qf;
    double expected_d = default_nan ? __builtin_nan("") : qd;
    /* x87 long double has six bytes of padding on amd64. */
    size_t long_bytes = LDBL_MANT_DIG == 64 ? 10 : sizeof sl;
    for (int signaling = 0; signaling <= 1; ++signaling) {
        CHECK(!native_clear(FE_ALL_EXCEPT));
        float f = native_frexpf(signaling ? sf : qf, &exponent);
        CHECK(!!native_test(FE_INVALID) == signaling && !memcmp(&f, &expected_f, sizeof f));
        CHECK(!native_clear(FE_ALL_EXCEPT));
        double d = native_frexp(signaling ? sd : qd, &exponent);
        CHECK(!!native_test(FE_INVALID) == signaling && !memcmp(&d, &expected_d, sizeof d));
        CHECK(!native_clear(FE_ALL_EXCEPT));
        long double l = native_frexpl(signaling ? sl : ql, &exponent);
        CHECK(!!native_test(FE_INVALID) == signaling && !memcmp(&l, &ql, long_bytes));
    }
    return 0;
}
static void *rounding_worker(void *unused) {
    (void)unused;
    if(native_getround()!=FE_DOWNWARD || native_setround(FE_UPWARD) || native_getround()!=FE_UPWARD)return (void *)1;
    return NULL;
}
static int numeric_native_probe(void) {
    long double (*volatile remainder)(long double, long double) = fmodl;
    fenv_t saved;
    CHECK(!feholdexcept(&saved));
    CHECK(!frexp_nan_probe(0));
#if defined(__aarch64__)
    uint64_t fpcr;
    __asm__ volatile("mrs %0, fpcr" : "=r"(fpcr));
    uint64_t default_nan = fpcr | (UINT64_C(1) << 25);
    __asm__ volatile("msr fpcr, %0; isb" :: "r"(default_nan) : "memory");
    int nan_result = frexp_nan_probe(1);
    __asm__ volatile("msr fpcr, %0; isb" :: "r"(fpcr) : "memory");
    CHECK(!nan_result);
#endif
    CHECK(!feclearexcept(FE_ALL_EXCEPT));
    double (*volatile scale)(double, int) = scalbn;
    long double (*volatile scale_long)(long double, int) = scalbnl;
    CHECK(!fesetround(FE_TONEAREST));
    errno = E2BIG;
    CHECK(scale(DBL_MIN, -1) == DBL_MIN / 2 && !fetestexcept(FE_ALL_EXCEPT) && errno == E2BIG);
    CHECK(scale_long(0, 100) == 0 && errno == E2BIG);
    CHECK(isinf(scale(DBL_MAX, 1)) && fetestexcept(FE_OVERFLOW) && errno == ERANGE);
    CHECK(!feclearexcept(FE_ALL_EXCEPT));
    errno = E2BIG;
    CHECK(scale_long(LDBL_TRUE_MIN, -1) == 0 && fetestexcept(FE_UNDERFLOW) && errno == ERANGE);
    CHECK(!feclearexcept(FE_ALL_EXCEPT));
    CHECK(!fesetround(FE_UPWARD));
    errno = E2BIG;
    CHECK(scale_long(LDBL_TRUE_MIN, -1) == LDBL_TRUE_MIN && errno == E2BIG);
    CHECK(fetestexcept(FE_UNDERFLOW | FE_INEXACT) == (FE_UNDERFLOW | FE_INEXACT));
    CHECK(!fesetround(FE_DOWNWARD) && !feclearexcept(FE_ALL_EXCEPT));
    CHECK(scale_long(LDBL_MAX, 1) == LDBL_MAX && errno == E2BIG);
    CHECK(fetestexcept(FE_OVERFLOW | FE_INEXACT) == (FE_OVERFLOW | FE_INEXACT));
    CHECK(!fesetround(FE_TOWARDZERO) && !feclearexcept(FE_ALL_EXCEPT));
    CHECK(scale_long(-LDBL_MAX, 1) == -LDBL_MAX && errno == E2BIG);
    CHECK(fetestexcept(FE_OVERFLOW | FE_INEXACT) == (FE_OVERFLOW | FE_INEXACT));
    CHECK(!fesetround(FE_TONEAREST) && !feclearexcept(FE_ALL_EXCEPT));
    CHECK(scale_long(3 * LDBL_TRUE_MIN, -1) == 2 * LDBL_TRUE_MIN);
    CHECK(scale_long(5 * LDBL_TRUE_MIN, -1) == 2 * LDBL_TRUE_MIN);
    CHECK(fetestexcept(FE_UNDERFLOW | FE_INEXACT) == (FE_UNDERFLOW | FE_INEXACT));
    CHECK(!feclearexcept(FE_ALL_EXCEPT));
    CHECK(remainder(3 * LDBL_TRUE_MIN, 2 * LDBL_TRUE_MIN) == LDBL_TRUE_MIN);
    CHECK(!fetestexcept(FE_UNDERFLOW | FE_INEXACT));
    errno = 0;
    CHECK(isnan(remainder(1, 0)) && fetestexcept(FE_INVALID));
    if (math_errhandling & MATH_ERRNO) CHECK(errno == EDOM);
    CHECK(!fesetenv(&saved));
    int original=native_getround(); CHECK(original>=0);
    const int modes[]={FE_TONEAREST,FE_UPWARD,FE_DOWNWARD,FE_TOWARDZERO};
    double (*volatile native_trunc)(double) = trunc;
    for(size_t i=0;i<sizeof modes/sizeof *modes;i++) {
        errno=E2BIG; CHECK(!native_setround(modes[i]) && native_getround()==modes[i] && errno==E2BIG);
        double positive = native_trunc(0.5), negative = native_trunc(-0.5);
        CHECK(positive == 0 && !signbit(positive) && negative == 0 && signbit(negative));
        CHECK(native_trunc(3.75) == 3 && native_trunc(-3.75) == -3);
#if defined(__x86_64__)
        unsigned short control; unsigned vector;
        __asm__ volatile("fnstcw %0; stmxcsr %1":"=m"(control),"=m"(vector));
        CHECK((control&0xc00)==modes[i] && ((vector>>3)&0xc00)==modes[i]);
        for(size_t j=0;j<3;j++) {
            const int bad[]={-1,1,0x1000}; CHECK(native_setround(bad[j])!=0 && native_getround()==modes[i]);
            unsigned short after; unsigned after_vector;
            __asm__ volatile("fnstcw %0; stmxcsr %1":"=m"(after),"=m"(after_vector));
            CHECK(after==control && after_vector==vector);
        }
#else
        CHECK(native_setround(-1)!=0 && native_setround(1)!=0 && native_getround()==modes[i]);
#endif
        volatile float one_f=1.0f,half_f=0x1p-24f; volatile float rounded_f=one_f+half_f;
        volatile double one_d=1.0,half_d=0x1p-53; volatile double rounded_d=one_d+half_d;
        volatile long double one_l=1.0L,half_l=LDBL_EPSILON/2; volatile long double rounded_l=one_l+half_l;
        CHECK(rounded_f==(modes[i]==FE_UPWARD ? 1.0f+FLT_EPSILON:1.0f));
        CHECK(rounded_d==(modes[i]==FE_UPWARD ? 1.0+DBL_EPSILON:1.0));
        CHECK(rounded_l==(modes[i]==FE_UPWARD ? 1.0L+LDBL_EPSILON:1.0L));
        int result=decomposition_probe(); CHECK(!result);
    }
    CHECK(!native_setround(FE_DOWNWARD));
    for(int i=0;i<2;i++) {
        pthread_t thread; void *result=(void *)1;
        CHECK(!pthread_create(&thread,NULL,rounding_worker,NULL)); CHECK(!pthread_join(thread,&result));
        CHECK(!result && native_getround()==FE_DOWNWARD);
    }
    CHECK(!native_setround(original));
    int environment_result=native_environment_probe(); if(environment_result)return environment_result;
    puts("native floating decomposition, rounding modes and thread isolation passed");
    return 0;
}

static int locale_native_probe(void) {
    locale_t c=newlocale(LC_ALL_MASK,"C",NULL),utf=newlocale(LC_ALL_MASK,"C.UTF-8",NULL);
    CHECK(c && utf); locale_t saved=uselocale(c);
#ifdef __GLIBC__
    locale_t legacy=newlocale(1<<LC_ALL,"C.UTF-8",NULL); CHECK(legacy);
    CHECK(!strcmp(nl_langinfo_l(CODESET,legacy),"UTF-8")); freelocale(legacy);
    errno=0; CHECK(!newlocale((1<<LC_ALL)|LC_CTYPE_MASK,"C",NULL) && errno==EINVAL);
    for(int selected=0;selected<2;selected++) {
        locale_t locale=selected ? utf:c;
        const struct { nl_item wide,narrow; int count; } groups[]={
            {_NL_WABDAY_1,ABDAY_1,7},{_NL_WDAY_1,DAY_1,7},
            {_NL_WABMON_1,ABMON_1,12},{_NL_WMON_1,MON_1,12},
            {_NL_WAM_STR,AM_STR,2},{_NL_WD_T_FMT,D_T_FMT,1},
            {_NL_WD_FMT,D_FMT,1},{_NL_WT_FMT,T_FMT,1},{_NL_WT_FMT_AMPM,T_FMT_AMPM,1},
            {_NL_WERA_D_FMT,ERA_D_FMT,1},{_NL_WERA_D_T_FMT,ERA_D_T_FMT,1},{_NL_WERA_T_FMT,ERA_T_FMT,1}
        };
        for(size_t i=0;i<sizeof groups/sizeof *groups;i++)for(int j=0;j<groups[i].count;j++) {
            const wchar_t *wide=(const wchar_t *)nl_langinfo_l(groups[i].wide+j,locale);
            const unsigned char *narrow=(const unsigned char *)nl_langinfo_l(groups[i].narrow+j,locale);
            CHECK(wide && narrow); size_t k=0; for(;narrow[k];k++)CHECK(wide[k]==narrow[k]); CHECK(!wide[k]);
            CHECK((const wchar_t *)nl_langinfo_l(groups[i].wide+j,locale)==wide);
            printf("locale-wide %d:%zu:%d %ls\n",selected,i,j,wide);
        }
        CHECK((uintptr_t)nl_langinfo_l(_NL_NUMERIC_DECIMAL_POINT_WC,locale)==L'.');
        CHECK(!(uintptr_t)nl_langinfo_l(_NL_NUMERIC_THOUSANDS_SEP_WC,locale));
        /* GNU nl_langinfo exposes the raw unavailable-byte marker. */
        CHECK((unsigned char)*nl_langinfo_l(INT_FRAC_DIGITS,locale)==255);
        CHECK((unsigned char)*nl_langinfo_l(INT_N_SIGN_POSN,locale)==255);
        CHECK(!strcmp(nl_langinfo_l(_NL_TIME_CODESET,locale),selected ? "UTF-8":"ANSI_X3.4-1968"));
        CHECK(!strcmp(nl_langinfo_l(_NL_LOCALE_NAME(LC_CTYPE),locale),selected ? "C.UTF-8":"C"));
        printf("locale-meta %d %u %s %s\n",selected,(unsigned)(unsigned char)*nl_langinfo_l(INT_FRAC_DIGITS,locale),
               nl_langinfo_l(_NL_TIME_CODESET,locale),nl_langinfo_l(_NL_LOCALE_NAME(LC_CTYPE),locale));
    }
#endif
    const char *numbers[]={"  -0x1.8p+2tail","1.000000000000000000867361737988403547205962240695953369140625x","nonsense"};
    for(size_t i=0;i<sizeof numbers/sizeof *numbers;i++) {
        char *a,*b; float f=strtof(numbers[i],&a); CHECK(strtof_l(numbers[i],&b,utf)==f && a==b);
        double d=strtod(numbers[i],&a); CHECK(strtod_l(numbers[i],&b,utf)==d && a==b);
        long double l=strtold(numbers[i],&a); CHECK(strtold_l(numbers[i],&b,utf)==l && a==b);
        CHECK(uselocale(NULL)==c);
    }
#if LDBL_MANT_DIG > DBL_MANT_DIG
    CHECK(strtold_l(numbers[1],NULL,utf)==1.0L+0x1p-60L);
#endif
    struct tm calendar={.tm_year=124,.tm_mon=1,.tm_mday=29,.tm_wday=4,.tm_yday=59,.tm_hour=13,.tm_min=7,.tm_sec=9,.tm_zone="UTC"};
    const wchar_t *formats[]={L"Ж 🐻 %Y-%m-%d %A %H:%M:%S",L"%c %x %X %% %z %Z",L"%G-W%V-%u %j",L"%_d|%-d|%04Y|%Od|%EY",L"%300Y",L""};
    const wchar_t *expected[]={L"Ж 🐻 2024-02-29 Thursday 13:07:09",L"Thu Feb 29 13:07:09 2024 02/29/24 13:07:09 % +0000 UTC",L"2024-W09-4 060",L"29|29|2024|29|2024"};
    for(size_t i=0;i<sizeof formats/sizeof *formats;i++) {
        wchar_t out[512],reference[512];
        size_t n=wcsftime_l(reference,512,formats[i],&calendar,utf); CHECK(n || !*formats[i]);
        if(i<4)CHECK(!wcscmp(reference,expected[i]));
        if(i==4) { CHECK(n==300); for(size_t j=0;j<296;j++)CHECK(reference[j]==L'0'); CHECK(!wcscmp(reference+296,L"2024")); }
        CHECK(uselocale(NULL)==c);
        CHECK(wcsftime_l(out,n+1,formats[i],&calendar,utf)==n && !wcscmp(out,reference));
        CHECK(!wcsftime_l(out,n,formats[i],&calendar,utf));
        CHECK(uselocale(NULL)==c);
    }
    struct { wchar_t before; wchar_t text[2]; wchar_t after; } guarded={0x1234,{0,0},0x7654};
    CHECK(wcsftime_l(guarded.text,2,L"Ж",&calendar,c)==1 && guarded.text[0]==L'Ж' && !guarded.text[1]);
    CHECK(!wcsftime_l(guarded.text,1,L"Ж",&calendar,c));
    CHECK(guarded.before==0x1234 && guarded.after==0x7654);
    CHECK(wcsftime(guarded.text,2,L"%n",&calendar)==1 && guarded.text[0]==L'\n');
    wchar_t large_format[140],small[16]; large_format[0]=L'%';
    for(size_t i=1;i<132;i++)large_format[i]=L'0';
    large_format[132]=L'4'; large_format[133]=L'Y'; large_format[134]=0;
    CHECK(wcsftime_l(small,16,large_format,&calendar,utf)==4 && !wcscmp(small,L"2024"));
    /* An unknown UTC offset has a defined empty %z expansion. */
    calendar.tm_isdst=-1; CHECK(wcsftime_l(small,2,L"%zx",&calendar,utf)==1 && !wcscmp(small,L"x"));
    calendar.tm_isdst=0;
#ifndef V_RUNTIME_HOSTED_ORACLE
    extern locale_t __newlocale(int,const char *,locale_t),__duplocale(locale_t),__uselocale(locale_t);
    extern void __freelocale(locale_t);
    extern char *__nl_langinfo_l(nl_item,locale_t);
    extern double __strtod_l(const char *,char **,locale_t);
    extern float __strtof_l(const char *,char **,locale_t);
    extern long double __strtold_l(const char *,char **,locale_t);
    extern int __strcoll_l(const char *,const char *,locale_t),__wcscoll_l(const wchar_t *,const wchar_t *,locale_t);
    extern size_t __strxfrm_l(char *,const char *,size_t,locale_t),__wcsxfrm_l(wchar_t *,const wchar_t *,size_t,locale_t);
    extern size_t __strftime_l(char *,size_t,const char *,const struct tm *,locale_t),__wcsftime_l(wchar_t *,size_t,const wchar_t *,const struct tm *,locale_t);
    extern wctype_t __wctype_l(const char *,locale_t);
    extern int __iswctype_l(wint_t,wctype_t,locale_t);
    extern wint_t __towlower_l(wint_t,locale_t),__towupper_l(wint_t,locale_t);
    locale_t alias=__newlocale(LC_ALL_MASK,"C.UTF-8",NULL); CHECK(alias);
    locale_t copy=__duplocale(alias); CHECK(copy && !strcmp(__nl_langinfo_l(CODESET,copy),"UTF-8"));
    CHECK(__strtod_l("1.5",NULL,copy)==1.5 && __strtof_l("2.5",NULL,copy)==2.5f && __strtold_l("3.5",NULL,copy)==3.5L);
    CHECK(__strcoll_l("a","b",copy)<0 && __wcscoll_l(L"a",L"b",copy)<0);
    char narrow[16]; wchar_t wide[16];
    CHECK(__strxfrm_l(narrow,"abc",16,copy)==3 && !strcmp(narrow,"abc"));
    CHECK(__wcsxfrm_l(wide,L"Ж",16,copy)==1 && !wcscmp(wide,L"Ж"));
    CHECK(__strftime_l(narrow,16,"%Y",&calendar,copy)==4 && !strcmp(narrow,"2024"));
    CHECK(__wcsftime_l(wide,16,L"%Y",&calendar,copy)==4 && !wcscmp(wide,L"2024"));
    CHECK(__iswctype_l(L'Ж',__wctype_l("alpha",copy),copy));
    CHECK(__towlower_l(L'Ж',copy)==L'ж' && __towupper_l(L'ж',copy)==L'Ж');
    CHECK(__uselocale(copy)==c && __uselocale(c)==copy);
    __freelocale(copy); __freelocale(alias);
#endif
    CHECK(uselocale(NULL)==c); uselocale(saved); freelocale(utf); freelocale(c);
    puts("explicit-locale numeric parsing, wide date formatting and GNU ABI aliases passed");
    return 0;
}
