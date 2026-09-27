interface Named {
	name() string
}

struct Record {}

fn (r Record) name() string {
	return 'record'
}

type RecordPtr = &Record

struct AliasOnlyRecord {}

type AliasOnlyPtr = &AliasOnlyRecord

fn (r AliasOnlyPtr) name() string {
	return 'alias'
}

fn optional_named(value ?Named) string {
	return value or { return '' }.name()
}

fn test_optional_interface_accepts_pointer_alias() {
	assert optional_named(RecordPtr(&Record{})) == 'record'
	assert optional_named(AliasOnlyPtr(&AliasOnlyRecord{})) == 'alias'
}

interface HasValue {
	value int
	name() string
}

struct ValueRecord {
	value int
}

fn (r ValueRecord) name() string {
	return 'underlying record'
}

type ValueRecordPtr = &ValueRecord

fn (r ValueRecordPtr) name() string {
	return 'value record'
}

fn optional_value(value ?HasValue) int {
	return (value or { return -1 }).value
}

fn test_nil_pointer_alias_interface_field_is_safe() {
	missing := ValueRecordPtr(unsafe { nil })
	assert optional_value(missing) == 0
	assert optional_value(ValueRecordPtr(&ValueRecord{ value: 42 })) == 42
}

interface HasAddress {
	address() voidptr
}

fn (r &Record) address() voidptr {
	return voidptr(r)
}

fn optional_address(value ?HasAddress) voidptr {
	return (value or { return unsafe { nil } }).address()
}

fn test_inline_pointer_alias_preserves_original_address() {
	record := Record{}
	assert optional_address(RecordPtr(&record)) == voidptr(&record)
}
