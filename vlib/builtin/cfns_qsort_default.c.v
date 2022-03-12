module builtin

fn C.qsort_r(base voidptr, items usize, item_size usize, context voidptr, cb C.qsort_r_bsd_callback_func_context)
fn C.qsort_s(base voidptr, items usize, item_size usize, cb C.qsort_s_iso_callback_func_context, context voidptr)
