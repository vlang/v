module fastc

import v.flat

fn test_fastc_alias_resolution_keeps_single_hops_and_undefined_bases() {
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Word', 'u64')
	aliases.set('Pair', 'FixedArray_2_u8')
	aliases.set('Dangling', 'External*')
	for source, expected in {
		'':           ''
		'int':        'int'
		'External**': 'External**'
		'Word':       'u64'
		'Word**':     'u64**'
		'Pair*':      'FixedArray_2_u8*'
		'Dangling':   'External*'
		'Dangling**': 'External***'
	} {
		assert fastc_underlying_alias_type(source, aliases) == expected, source
	}
	assert aliases.get('Word') or { panic('missing alias') } == 'u64'
	assert aliases.get('Dangling') or { panic('missing alias') } == 'External*'
}

fn test_fastc_alias_resolution_keeps_pointer_depth_and_cycle_endpoints() {
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Word', 'u64')
	aliases.set('Middle', 'Word*')
	aliases.set('Outer', 'Middle*')
	aliases.set('Self', 'Self')
	aliases.set('SelfPointer', 'SelfPointer*')
	aliases.set('Left', 'Right')
	aliases.set('Right', 'Left')
	aliases.set('PointerLeft', 'PointerRight*')
	aliases.set('PointerRight', 'PointerLeft**')
	aliases.set('CycleEntry', 'Left*')
	for source, expected in {
		'Outer':         'u64**'
		'Outer*':        'u64***'
		'Middle**':      'u64***'
		'Self':          'Self'
		'Self*':         'Self*'
		'SelfPointer**': 'SelfPointer***'
		'Left':          'Left'
		'Right*':        'Right*'
		'PointerLeft*':  'PointerLeft****'
		'PointerRight':  'PointerRight***'
		'CycleEntry**':  'Left***'
	} {
		assert fastc_underlying_alias_type(source, aliases) == expected, source
	}
	assert aliases.get('Outer') or { panic('missing alias') } == 'Middle*'
	assert aliases.get('PointerRight') or { panic('missing alias') } == 'PointerLeft**'
}
