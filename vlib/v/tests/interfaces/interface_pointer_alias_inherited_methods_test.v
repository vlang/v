// A pointer alias (`type RecordPtr = &Record`) implements an interface through
// the methods of its pointee. Its interface box holds the alias pointer, so
// every inherited method must receive the original object behind it.
struct Base {
	base int
}

fn (b &Base) base_value() int {
	return b.base
}

struct Record {
	Base
mut:
	n int
}

fn (r &Record) address() voidptr {
	return voidptr(r)
}

fn (r Record) num() int {
	return r.n
}

fn (mut r Record) set(n int) {
	r.n = n
}

type RecordPtr = &Record

fn (r RecordPtr) label() string {
	return 'alias ${r.n}'
}

type RecordPtrPtr = &&Record

interface Inherited {
	address() voidptr
	num() int
	base_value() int
mut:
	set(n int)
}

interface Mixed {
	address() voidptr
	label() string
mut:
	set(n int)
}

fn set_inherited(mut value Inherited, n int) {
	value.set(n)
}

fn new_record() Record {
	return Record{
		Base: Base{
			base: 3
		}
		n:    42
	}
}

fn test_cast_of_pointer_alias_uses_inherited_methods_on_original_object() {
	mut record := new_record()
	mut value := Inherited(RecordPtr(&record))
	assert value.address() == voidptr(&record)
	assert value.num() == 42
	assert value.base_value() == 3
	set_inherited(mut value, 7)
	assert record.n == 7
	assert value.num() == 7
}

fn test_array_of_pointer_alias_uses_inherited_methods_on_original_object() {
	mut record := new_record()
	ptr := RecordPtr(&record)
	mut values := []Inherited{}
	values << ptr
	assert values[0].address() == voidptr(&record)
	assert values[0].num() == 42
}

fn test_mixed_alias_and_inherited_methods_share_the_original_object() {
	mut record := new_record()
	mut value := Mixed(RecordPtr(&record))
	assert value.address() == voidptr(&record)
	value.set(9)
	assert record.n == 9
	assert value.label() == 'alias 9'
}

fn test_double_pointer_alias_uses_inherited_methods_on_original_object() {
	mut record := new_record()
	ptr := &record
	mut value := Inherited(RecordPtrPtr(&ptr))
	assert value.address() == voidptr(&record)
	assert value.num() == 42
	assert value.base_value() == 3
	set_inherited(mut value, 11)
	assert record.n == 11
}
