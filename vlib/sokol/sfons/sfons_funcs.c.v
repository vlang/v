module sfons

import fontstash

fn C.sfons_create(width int, height int, flags int) &fontstash.Context
fn C.sfons_destroy(ctx &fontstash.Context)
fn C.sfons_rgba(r u8, g u8, b u8, a u8) u32
fn C.sfons_flush(ctx &fontstash.Context)
