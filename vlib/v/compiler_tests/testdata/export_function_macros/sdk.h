#ifndef V_EXPORT_FUNCTION_MACROS_SDK_H
#define V_EXPORT_FUNCTION_MACROS_SDK_H

/* A header can expose both real C symbols and function-like checked wrappers. */
double (sdk_natural)(double value);
double (sdk_renamed)(double value);
static int sdk_checked_calls;
#define init() (++sdk_checked_calls, (init)())
#define cleanup() (++sdk_checked_calls, (cleanup)())
#define sdk_natural(value) (++sdk_checked_calls, (sdk_natural)((value)))
#define Multiplier__apply(object, value) (++sdk_checked_calls, (Multiplier__apply)((object), (value)))
#define sdk_renamed(value) (++sdk_checked_calls, (sdk_renamed)((value)))

static int sdk_macro_calls(void) { return sdk_checked_calls; }
static int sdk_consume_exports(void) {
    double (*volatile natural)(double) = sdk_natural;
    double (*volatile renamed)(double) = sdk_renamed;
    if (natural(2) != 12 || renamed(3) != 23) return -1;
    if (sdk_natural(4) != 14 || sdk_renamed(5) != 25) return -2;
    return sdk_checked_calls;
}
#endif
