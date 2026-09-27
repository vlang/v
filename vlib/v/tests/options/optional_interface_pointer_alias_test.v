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
