# Compile-time messages

`$compile_error('message')`, `$compile_warn('message')`, and `$compile_notice('message')`
report custom diagnostics during compilation. `$compile_notice` accepts a string literal and
reports information without stopping the build, including with `-prod`.

Top-level notices in imported modules are evaluated even when none of their functions is called.
Notices in generic functions are reported when the function is instantiated. Only selected
compile-time branches report their notices, and repeated calls do not repeat the same notice.
Unused generic functions do not emit notices. The formatter preserves `$compile_notice` calls.

`-N` treats notices, including unused parameter notices, as errors. A generic parameter used
only to infer a type for reflection, such as `$if T is $function`, is considered used. A quoted
type name in unrelated reflection metadata, such as `$if field.name == 'T'` inside a loop over
another type's fields, does not use a parameter of type `T`.
