module fastc

import v.pref
import v.scanner
import v.token

fn fastc_receiver_probe_test_parser(body string) Parser {
	prefs := pref.new_preferences()
	// A receiver after the closing brace belongs to the next declaration.
	source := '{\n${body}\n}\nmissing.after_body()\n'
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('receiver_probe.v', source.len), source)
	opening := scan.scan()
	assert opening == .lcbr
	return Parser{
		prefs:       prefs
		s:           scan
		tok:         opening
		module_name: 'main'
		locals:      {
			'known': FastcLocal{}
			'items': FastcLocal{}
		}
	}
}

fn test_fastc_undefined_receiver_probe_tracks_bindings_and_body_boundaries() {
	for body in ['missing.run()', 'marker := missing.headers["key"]',
		'known.call(arg_a, missing)\nmissing.run()', 'if known.ready { missing.run() }',
		r"text := '${missing.run()}'"] {
		g := fastc_receiver_probe_test_parser(body)
		assert g.method_uses_undefined_receiver(), body
	}
	for body in ['', 'known.run()', 'local := make()\nlocal.run()', 'local := make(); local.run()',
		'mut local := known\nlocal.run()', 'first, local := make_pair()\nlocal.run()',
		'for local in items { local.run() }', 'for index, local in items { local.run() }',
		'local /* binding */ := known\nlocal /* receiver */ .run()', 'C.run()\nit.run()\nerr.run()',
		'text := "missing.run()"', r"text := '${known.run()}'",
		r"text := '${known.run('${known.run()}')}'"] {
		g := fastc_receiver_probe_test_parser(body)
		assert !g.method_uses_undefined_receiver(), body
	}
}

fn test_fastc_undefined_receiver_probe_keeps_closure_bailout_and_scanner_state() {
	for body in ['missing.run()\ncallback := fn (arg int) { arg.run() }',
		'callback := fn [known] () { missing.run() }', r"known.run('\q')"] {
		g := fastc_receiver_probe_test_parser(body)
		offset := g.s.offset
		pos := g.s.pos
		lit := g.s.lit
		assert !g.method_uses_undefined_receiver(), body
		assert !g.method_uses_undefined_receiver(), body
		assert g.tok == .lcbr
		assert g.s.offset == offset
		assert g.s.pos == pos
		assert g.s.lit == lit
		assert g.s.diagnostics.len == 0
	}
	mut outside := fastc_receiver_probe_test_parser('missing.run()')
	outside.tok = .name
	assert !outside.method_uses_undefined_receiver()
}
