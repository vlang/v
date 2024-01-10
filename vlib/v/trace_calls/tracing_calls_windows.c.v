module trace_calls

fn C.GetCurrentThreadId() u32
fn C.QueryPerformanceCounter(&u64) C.BOOL
