module trace_calls

// gettid is missing on musl/alpine, but present on most everything else
[export: 'gettid']
[weak]
fn vgettid() &u32 {
	return 0
}
