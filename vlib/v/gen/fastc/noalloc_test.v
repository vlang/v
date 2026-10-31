module fastc

import v.pref

fn test_fastc_rejects_v_allocation_contracts() {
	prefs := &pref.Preferences{}
	for source in [
		'@[noalloc]\nfn pure(n int) int { return n }\nfn main() {}',
		'@[noalloc]\ntype Callback = fn () int\nfn main() {}',
	] {
		generate(source, 'noalloc.v', prefs) or {
			assert err.msg().contains('noalloc'), err.msg()
			continue
		}
		assert false, 'FastC must reject contracts it cannot check'
	}
}

fn test_fastc_keeps_trusted_foreign_declarations() {
	prefs := &pref.Preferences{}
	source := '@[noalloc]\nfn C.pure_external(n int) int\nfn main() {}'
	generated := generate(source, 'foreign_contract.c.v', prefs) or { panic(err) }
	assert generated.contains('main(')
}
