module sfons

fn C.sfons_create(width int, height int, flags int) &C.FONScontext
fn C.sfons_destroy(ctx &C.FONScontext)
fn C.sfons_rgba(r byte, g byte, b byte, a byte) u32
fn C.sfons_flush(ctx &C.FONScontext)
