type ItemType = u32

struct FirstItem {
	typ ItemType
}

struct SecondItem {
	typ ItemType
}

type Item = FirstItem | SecondItem

fn (typ ItemType) is_second() bool {
	return typ == 2
}

fn test_sum_shared_typ_field_keeps_declared_alias_type() {
	item := Item(SecondItem{
		typ: 2
	})
	assert item.typ.is_second()
}
