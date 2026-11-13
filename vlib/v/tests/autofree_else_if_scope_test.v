// vtest vflags: -autofree
@[has_globals]
module main

__global cleanup_order int

struct ScopeTracked {
	id int
}

fn (value &ScopeTracked) free() {
	unsafe {
		cleanup_order = cleanup_order * 10 + value.id
	}
}

fn visit_membership_branch(word string, first []string, second []string) {
	if word in first {
		selected := ScopeTracked{1}
		assert selected.id == 1
	} else if word in second {
		selected := ScopeTracked{2}
		assert selected.id == 2
	} else {
		selected := ScopeTracked{3}
		assert selected.id == 3
	}
	outer := ScopeTracked{4}
	if word.len > 0 {
		final := word.clone()
		assert final == word
		inner := ScopeTracked{5}
		assert inner.id == 5
	}
	assert outer.id == 4
}

fn test_else_if_condition_temporaries_preserve_source_cleanup_scopes() {
	for word, expected in {
		'first':   154
		'second':  254
		'neither': 354
	} {
		unsafe { cleanup_order = 0 }
		visit_membership_branch(word, ['first'], ['second'])
		assert unsafe { cleanup_order } == expected
	}
}

fn visit_index_or_branch(index int, values []int) {
	selected := values[index] or {
		fallback := ScopeTracked{1}
		assert fallback.id == 1
		-1
	}
	assert selected == if index == 0 { 42 } else { -1 }
	outer := ScopeTracked{4}
	if selected != 0 {
		final := 'done'.clone()
		assert final == 'done'
		inner := ScopeTracked{5}
		assert inner.id == 5
	}
	assert outer.id == 4
}

fn test_index_or_success_preserves_fallback_and_later_cleanup_scopes() {
	for index, expected in {
		0:  54
		1:  154
		-1: 154
	} {
		unsafe {
			cleanup_order = 0
		}
		visit_index_or_branch(index, [42])
		assert unsafe { cleanup_order } == expected
	}
}
