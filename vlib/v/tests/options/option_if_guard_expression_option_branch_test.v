struct GuardedFlushHost {
mut:
	flushes int
}

fn (mut host GuardedFlushHost) flush(mut messages []string) ?string {
	messages << 'flushed'
	host.flushes++
	return 'next.html'
}

fn guarded_navigation(parser_host ?&GuardedFlushHost) string {
	mut messages := []string{}
	// The option branch reads the guard binding; its payload type is the expression's.
	navigation := if mut host := parser_host { host.flush(mut messages) } else { none }
	if destination := navigation {
		return '${destination} ${messages.len}'
	}
	return 'none'
}

fn halved(value int) ?f64 {
	if value == 0 {
		return none
	}
	return f64(value) / 2
}

fn guarded_half(value ?int) ?f64 {
	half := if number := value { halved(number) } else { none }
	return half
}

fn test_if_guard_expression_keeps_the_option_branch_payload() {
	mut host := &GuardedFlushHost{}
	assert guarded_navigation(host) == 'next.html 1'
	assert host.flushes == 1
	assert guarded_navigation(none) == 'none'
	assert guarded_half(3) or { -1.0 } == 1.5
	assert guarded_half(0) == none
	assert guarded_half(none) == none
}
