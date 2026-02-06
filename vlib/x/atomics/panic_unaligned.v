module atomics

@[export: 'panicUnaligned']
fn panic_unaligned() {
	panic('unaligned atomic operation')
}
