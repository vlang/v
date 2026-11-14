module wit

fn test_wit_cabi_layout_primitives() {
	doc := WitDoc{}
	sz, al := wit_cabi_size_align(prim_type('u32'), doc)
	assert sz == 4 && al == 4
	ssz, sal := wit_cabi_size_align(prim_type('string'), doc)
	assert ssz == 8 && sal == 4
	lsz, lal := wit_cabi_size_align(WitType{ kind: .list, items: [prim_type('u8')] }, doc)
	assert lsz == 8 && lal == 4
	osz, oal := wit_cabi_size_align(WitType{ kind: .option, items: [prim_type('u16')] }, doc)
	assert osz == 4 && oal == 2
	rsz, ral := wit_cabi_size_align(WitType{ kind: .result, items: [prim_type('u8'), prim_type('u32')] }, doc)
	assert rsz == 8 && ral == 4
	tsz, tal := wit_cabi_size_align(WitType{ kind: .tuple, items: [prim_type('u32'), prim_type('u8')] }, doc)
	assert tsz == 8 && tal == 4
}

fn test_wit_cabi_layout_named() {
	mut doc := WitDoc{}
	doc.interfaces << WitInterface{
		name:  'test'
		items: [
			WitItem{
				kind:   .record
				record: WitRecord{
					name:   'point'
					fields: [WitField{ name: 'x', typ: prim_type('u32') }, WitField{
						name: 'y'
						typ:  prim_type('u8')
					}]
				}
			},
			WitItem{
				kind:  .enum_
				enum_: WitEnum{ name: 'color', cases: ['red', 'green', 'blue'] }
			},
			WitItem{
				kind:  .flags
				flags: WitFlags{ name: 'perms', flags: ['r', 'w', 'x'] }
			},
			WitItem{
				kind:    .variant
				variant: WitVariant{
					name:  'v'
					cases: [WitVariantCase{ name: 'a', typ: prim_type('u8') }, WitVariantCase{
						name: 'b'
						typ:  prim_type('u32')
					}, WitVariantCase{ name: 'c' }]
				}
			},
		]
	}
	psz, pal := wit_cabi_size_align(named_type('point'), doc)
	assert psz == 8 && pal == 4
	esz, eal := wit_cabi_size_align(named_type('color'), doc)
	assert esz == 1 && eal == 1
	fsz, fal := wit_cabi_size_align(named_type('perms'), doc)
	assert fsz == 1 && fal == 1
	vsz, val := wit_cabi_size_align(named_type('v'), doc)
	assert vsz == 8 && val == 4
	// option<point> wraps the record: discriminant + 8-byte payload.
	osz, oal := wit_cabi_size_align(WitType{ kind: .option, items: [named_type('point')] }, doc)
	assert osz == 12 && oal == 4
	// unknown names and resources have no layout.
	bsz, _ := wit_cabi_size_align(named_type('missing'), doc)
	assert bsz == -1
	hsz, _ := wit_cabi_size_align(named_type('own<res>'), doc)
	assert hsz == -1
}

fn test_wit_func_lift_with_doc_enums_flags() {
	mut doc := WitDoc{}
	doc.interfaces << WitInterface{
		name:  'test'
		items: [
			WitItem{
				kind:  .enum_
				enum_: WitEnum{ name: 'color', cases: ['red', 'green', 'blue'] }
			},
			WitItem{
				kind:  .flags
				flags: WitFlags{ name: 'perms', flags: ['r', 'w', 'x'] }
			},
		]
	}
	// enum param lifts as component s32 over core i32.
	ef := WitFunc{
		name:    'take-color'
		params:  [WitParam{ name: 'c', typ: named_type('color') }]
		results: [prim_type('u32')]
	}
	if spec := wit_func_lift_with_doc(ef, doc) {
		assert spec.core_params == [u8(0x7f)]
		assert spec.comp_params == [u8(0x7a)]
		assert spec.comp_labels == ['c']
	} else {
		assert false, 'enum param must lift'
	}
	// flags result lifts as component u32 over core i32.
	ff := WitFunc{
		name:    'get-perms'
		results: [named_type('perms')]
	}
	if spec := wit_func_lift_with_doc(ff, doc) {
		assert spec.core_results == [u8(0x7f)]
		assert spec.comp_results == [u8(0x79)]
	} else {
		assert false, 'flags result must lift'
	}
	// composites are still refused: no wrapper allocation path yet.
	for t in [WitType{ kind: .list, items: [prim_type('u8')] }, WitType{
		kind:  .option
		items: [prim_type('u8')]
	}, WitType{ kind: .tuple, items: [prim_type('u8'), prim_type('u8')] }, named_type('missing')] {
		nf := WitFunc{
			name:   'take-it'
			params: [WitParam{ name: 'x', typ: t }]
		}
		if _ := wit_func_lift_with_doc(nf, doc) {
			assert false, 'composite ${t} must not lift yet'
		}
	}
}
