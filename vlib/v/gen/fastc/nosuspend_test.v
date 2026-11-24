module fastc

import v.pref

fn test_fastc_rejects_v_nosuspend_contracts() {
	prefs := &pref.Preferences{}
	for source in [
		'@[nosuspend]\nfn pure(n int) int { return n }\nfn main() {}',
		'@[nosuspend]\ntype Callback = fn () int\nfn main() {}',
	] {
		generate(source, 'nosuspend.v', prefs) or {
			assert err.msg().contains('nosuspend'), err.msg()
			continue
		}
		assert false, 'FastC must reject contracts it cannot check'
	}
}

fn test_fastc_keeps_trusted_nosuspend_foreign_declarations() {
	prefs := &pref.Preferences{}
	source := '@[nosuspend]\nfn C.pure_external(n int) int\nfn main() {}'
	generated := generate(source, 'foreign_contract.c.v', prefs) or { panic(err) }
	assert generated.contains('main(')
}
