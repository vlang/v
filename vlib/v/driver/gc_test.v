module driver

fn test_v3_gc_mode_defines() {
	cases := {
		'':               ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm':          ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm_full':     ['gcboehm', 'gcboehm_full']
		'boehm_incr':     ['gcboehm', 'gcboehm_incr']
		'boehm_full_opt': ['gcboehm', 'gcboehm_full', 'gcboehm_opt']
		'boehm_incr_opt': ['gcboehm', 'gcboehm_incr', 'gcboehm_opt']
		'boehm_leak':     ['gcboehm', 'gcboehm_leak']
		'none':           []string{}
		'vgc':            ['vgc']
	}
	for mode, expected in cases {
		actual := v3_gc_mode_defines(mode, false) or { panic(err) }
		assert actual == expected
	}
}

fn test_v3_gc_mode_is_disabled_when_building_v() {
	for mode in ['', 'boehm', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt',
		'boehm_leak', 'none', 'vgc'] {
		actual := v3_gc_mode_defines(mode, true) or { panic(err) }
		assert actual == []
	}
}

fn test_v3_gc_mode_keeps_dynamic_boehm_define() {
	mut defines := ['dynamic_boehm']
	mut values := {
		'dynamic_boehm': 'true'
	}
	apply_v3_gc_mode('boehm', false, mut defines, mut values) or { panic(err) }
	assert defines == ['dynamic_boehm', 'gcboehm', 'gcboehm_full', 'gcboehm_opt']
	assert values['dynamic_boehm'] == 'true'
	assert values['gcboehm'] == 'true'
	assert values['gcboehm_full'] == 'true'
	assert values['gcboehm_opt'] == 'true'
}

fn test_v3_gc_mode_rejects_unknown_mode() {
	v3_gc_mode_defines('bogus', false) or {
		assert err.msg().contains('unknown garbage collection mode `-gc bogus`')
		return
	}
	assert false
}
