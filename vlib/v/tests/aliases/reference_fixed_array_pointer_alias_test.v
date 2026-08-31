// A pointer alias to a fixed array (`type PtrFA = &FA`) is a pointer, not a
// fixed array value, so taking its address (`&p`) must be allowed. Resolving
// the alias chain must preserve the pointer indirection, otherwise the checker
// wrongly reports "cannot reference fixed array" (regression guard for the
// fix of #27646).
type FA = [8]int

type PtrFA = &FA

fn get_ref(p PtrFA) bool {
	q := &p
	return q != unsafe { nil }
}

fn test_reference_pointer_alias_to_fixed_array() {
	mut arr := FA{}
	assert get_ref(unsafe { &arr })
}
