import os

struct File {
	typ int
}

struct List[T] {
mut:
	items []T
}

fn test_generic_field_loop_preserves_main_type_identity() {
	_ := os.path_separator
	mut list := List[File]{}
	list.items << File{ typ: 22 }
	list.items << File{ typ: 3 }
	mut total := 0
	for value in list.items {
		total += value.typ
	}
	assert total == 25
	for i, value in list.items {
		assert value.typ == list.items[i].typ
	}
	refs := List[&File]{ items: [&File{ typ: 42 }] }
	for value in refs.items {
		assert value.typ == 42
	}
}

type Mmap = int

fn (value Mmap) doubled() int {
	return int(value) * 2
}

enum FileInfo {
	first
	second
}

fn test_generic_field_loop_preserves_main_alias_and_enum() {
	mut aliases := List[Mmap]{}
	aliases.items << Mmap(7)
	for value in aliases.items {
		assert value.doubled() == 14
	}
	mut enums := List[FileInfo]{}
	enums.items << FileInfo.second
	for value in enums.items {
		assert value == FileInfo.second
	}
}
