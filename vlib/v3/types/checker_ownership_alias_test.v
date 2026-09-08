// vtest vflags: -d ownership
module types

fn test_long_pointer_index_alias_chain_is_borrowed() {
	aliases := {
		'p0': 'm[k]'
		'p1': 'p0'
		'p2': 'p1'
		'p3': 'p2'
		'p4': 'p3'
		'p5': 'p4'
		'p6': 'p5'
		'p7': 'p6'
		'p8': 'p7'
	}
	assert ownership_alias_chain_borrows_indexed_storage(aliases, 'p8')
	assert !ownership_alias_chain_borrows_indexed_storage(aliases, 'unrelated')

	cycle := {
		'left':  'right'
		'right': 'left'
	}
	assert ownership_alias_chain_borrows_indexed_storage(cycle, 'left')
}
