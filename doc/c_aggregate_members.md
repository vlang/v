# C aggregate member names

C struct and union members retain their declared spelling, including names such as `index`
and `select` that are also C library functions. C keeps aggregate members in a separate
namespace, so these field names do not conflict with function names.

A binding such as `struct C.Record { index u64 }` accesses the C member `index` in both
`C.Record{ index: 42 }` and `record.index`. This also applies through V aliases and pointers,
and when the C aggregate is embedded in a V struct. Escapes for V keywords, such as `@type`,
refer to the native C member `type`.

Ordinary V struct fields retain V's generated identifier spelling, even when an alias or
pointer is used. This includes fields whose names overlap with C library functions.
