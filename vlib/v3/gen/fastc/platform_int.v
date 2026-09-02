@[has_globals]
module fastc

// fastc_platform_int_c_type is the C spelling FastC emits for V's
// platform-width `int`. Keep the state in its own module file so FastC's
// generation entry point can follow upstream refactors without carrying a
// conflict-prone module-header edit. FastC self-hosts for the current machine,
// so default this from the compiler's pointer width.
__global fastc_platform_int_c_type = $if x64 { 'i64' } $else { 'i32' }
