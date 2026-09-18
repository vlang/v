module builtin

// V1 initializes its collector from the legacy C generator. Keep the platform
// builtin hook resolvable without running the V3 startup sequence.
@[inline]
fn gc_runtime_init() {}
