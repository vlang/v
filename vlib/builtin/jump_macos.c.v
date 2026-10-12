module builtin

// Both families share Darwin's architecture-specific jump buffer layout.
@[markused; noalloc; returns_twice]
fn C.setjmp(buffer &i32) i32

@[markused; noalloc; noreturn]
fn C.longjmp(buffer &i32, value i32)

@[markused; noalloc; returns_twice]
fn C._setjmp(buffer &i32) i32

@[markused; noalloc; noreturn]
fn C._longjmp(buffer &i32, value i32)
