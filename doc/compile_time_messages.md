# Compile-time messages

`$compile_error('message')`, `$compile_warn('message')`, and `$compile_notice('message')`
report custom diagnostics during compilation. `$compile_notice` accepts a string literal and
reports information without stopping the build, including with `-prod`.

Top-level notices in imported modules are evaluated even when none of their functions is called.
Notices in generic functions are reported when the function is instantiated. Only selected
compile-time branches report their notices, and repeated calls do not repeat the same notice.
Unused generic functions do not emit notices. The formatter preserves `$compile_notice` calls.
