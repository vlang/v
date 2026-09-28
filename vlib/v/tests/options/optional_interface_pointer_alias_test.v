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

fn (r AliasOnlyPtr) address() voidptr {
	return voidptr(r)
}

fn optional_address(value ?HasAddress) voidptr {
	return (value or { return unsafe { nil } }).address()
}

fn test_inline_pointer_alias_preserves_original_address() {
	record := Record{}
	assert optional_address(RecordPtr(&record)) == voidptr(&record)
}

fn retain_optional_address(mut values []HasAddress, value ?HasAddress) {
	values << value or { panic('expected an address') }
}

@[noinline]
fn retain_inline_pointer_alias(mut values []HasAddress, mut record AliasOnlyRecord) {
	retain_optional_address(mut values, AliasOnlyPtr(&record))
}

fn test_retained_inline_pointer_alias_preserves_original_address() {
	mut first := AliasOnlyRecord{}
	mut second := AliasOnlyRecord{}
	mut values := []HasAddress{}
	retain_inline_pointer_alias(mut values, mut first)
	retain_inline_pointer_alias(mut values, mut second)
	assert values[0].address() == voidptr(&first)
	assert values[1].address() == voidptr(&second)
}

fn return_inherited_alias_address(mut record Record) voidptr {
	return optional_address(RecordPtr(&record))
}

fn return_alias_address(mut record AliasOnlyRecord) voidptr {
	return optional_address(AliasOnlyPtr(&record))
}

fn retain_address_result(mut values []HasAddress, value ?HasAddress) bool {
	retain_optional_address(mut values, value)
	return true
}

@[noinline]
fn return_retained_alias(mut values []HasAddress, mut record AliasOnlyRecord) bool {
	return retain_address_result(mut values, AliasOnlyPtr(&record))
}

fn test_return_position_optional_interface_call_preserves_pointer_alias() {
	mut record := Record{}
	mut alias_record := AliasOnlyRecord{}
	assert return_inherited_alias_address(mut record) == voidptr(&record)
	assert return_alias_address(mut alias_record) == voidptr(&alias_record)
	mut values := []HasAddress{}
	assert return_retained_alias(mut values, mut alias_record)
	assert values[0].address() == voidptr(&alias_record)
}
