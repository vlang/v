// vtest vflags: -d ownership

struct OwnedOptionalGuard {
mut:
	data ?[]u8
}

fn owned_guard_source_is_none(item &OwnedOptionalGuard) bool {
	return item.data == none
}

struct OwnedPromotedOptionalGuard {
	OwnedOptionalGuard
}

struct OwnedSharedGuardA {
mut:
	data ?[]u8
}

struct OwnedSharedGuardB {
mut:
	data ?[]u8
}

type OwnedSharedGuard = OwnedSharedGuardA | OwnedSharedGuardB

fn owned_promoted_guard_source_is_none(item &OwnedPromotedOptionalGuard) bool {
	return item.data == none
}

fn owned_shared_guard_source_is_none(item &OwnedSharedGuard) bool {
	return item.data == none
}

fn owned_statement_guard(mut item OwnedOptionalGuard) int {
	if item.data == none {
		return 0
	}
	mut result := 0
	if data := item.data {
		result = data.len
	}
	assert owned_guard_source_is_none(item)
	return result
}

fn owned_value_guard(mut item OwnedOptionalGuard) int {
	if item.data == none {
		return 0
	}
	result := if data := item.data { data.len } else { 0 }
	assert owned_guard_source_is_none(item)
	return result
}

fn owned_promoted_value_guard(mut item OwnedPromotedOptionalGuard) int {
	if item.data == none {
		return 0
	}
	result := if data := item.data { data.len } else { 0 }
	assert owned_promoted_guard_source_is_none(item)
	return result
}

fn owned_shared_value_guard(mut item OwnedSharedGuard) int {
	if item.data == none {
		return 0
	}
	result := if data := item.data { data.len } else { 0 }
	assert owned_shared_guard_source_is_none(item)
	return result
}

fn owned_shared_statement_guard(mut item OwnedSharedGuard) int {
	if item.data == none {
		return 0
	}
	mut result := 0
	if data := item.data {
		result = data.len
	}
	assert owned_shared_guard_source_is_none(item)
	return result
}

fn test_owned_optional_field_guards_after_none_check() {
	mut statement_item := OwnedOptionalGuard{ data: [u8(1), 2, 3] }
	mut value_item := OwnedOptionalGuard{ data: [u8(4), 5, 6] }
	mut promoted_item := OwnedPromotedOptionalGuard{
		OwnedOptionalGuard: OwnedOptionalGuard{
			data: [u8(7), 8, 9]
		}
	}
	mut shared_item := OwnedSharedGuard(OwnedSharedGuardA{ data: [u8(10), 11, 12] })
	mut shared_statement_item := OwnedSharedGuard(OwnedSharedGuardB{ data: [u8(13), 14, 15] })
	assert owned_statement_guard(mut statement_item) == 3
	assert owned_value_guard(mut value_item) == 3
	assert owned_promoted_value_guard(mut promoted_item) == 3
	assert owned_shared_value_guard(mut shared_item) == 3
	assert owned_shared_statement_guard(mut shared_statement_item) == 3
}
