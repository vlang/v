import time
import x.json2 as json

struct StructTypeSkippedFields[T] {
mut:
	val  T @[json: '-']
	val1 T
	val2 T @[json: '-']
	val3 T
}

struct StructTypeSkippedFields1[T] {
mut:
	val  T
	val1 T @[json: '-']
	val2 T
	val3 T @[json: '-']
}

struct StructTypeSkippedFields2[T] {
mut:
	val  T @[json: '-']
	val1 T @[json: '-']
	val2 T @[json: '-']
	val3 T @[json: '-']
}

struct StructTypeSkippedFields3[T, U, V] {
mut:
	val  T @[json: '-']
	val1 U
	val2 V
	val3 T @[json: '-']
}

struct StructTypeSkippedFields4 {
mut:
	val  string    @[json: '-']
	val1 i64
	val2 f64
	val3 time.Time
}

struct StructTypeSkippedFields5 {
mut:
	val  string    @[json: '-']
	val1 i64       @[json: '-']
	val2 f64
	val3 time.Time
}

struct StructTypeSkippedFields6 {
mut:
	val  string    @[json: '-']
	val1 i64
	val2 f64       @[json: '-']
	val3 time.Time
}

struct StructTypeSkippedFields7 {
mut:
	val  string
	val1 i64       @[json: '-']
	val2 f64       @[json: '-']
	val3 time.Time
}

struct StructTypeSkippedFields8 {
mut:
	val  string
	val1 i64       @[json: '-']
	val2 f64
	val3 time.Time @[json: '-']
}

fn test_encode_struct_skipped_fields() {
	assert json.encode(StructTypeSkippedFields[string]{
		val: 'string_val'
		val1: 'string_val1'
		val2: 'string_val2'
		val3: 'string_val3'
	}) == '{"val1":"string_val1","val3":"string_val3"}'

	assert json.encode(StructTypeSkippedFields1[string]{
		val: 'string_val'
		val1: 'string_val1'
		val2: 'string_val2'
		val3: 'string_val3'
	}) == '{"val":"string_val","val2":"string_val2"}'

	assert json.encode(StructTypeSkippedFields2[string]{
		val: 'string_val'
		val1: 'string_val1'
		val2: 'string_val2'
		val3: 'string_val3'
	}) == '{}'

	assert json.encode(StructTypeSkippedFields3[string, i64, f64]{
		val: 'string_val'
		val1: 1
		val2: 1.0
		val3: 'string_val'
	}) == '{"val1":1,"val2":1.0}'

	assert json.encode(StructTypeSkippedFields4{
		val: 'string_val'
		val1: 1
		val2: 1.0
	}) == '{"val1":1,"val2":1.0,"val3":"0000-00-00T00:00:00.000Z"}'

	assert json.encode(StructTypeSkippedFields5{
		val: 'string_val'
		val1: 1
		val2: 1.0
	}) == '{"val2":1.0,"val3":"0000-00-00T00:00:00.000Z"}'

	assert json.encode(StructTypeSkippedFields6{
		val: 'string_val'
		val1: 1
		val2: 1.0
	}) == '{"val1":1,"val3":"0000-00-00T00:00:00.000Z"}'

	assert json.encode(StructTypeSkippedFields7{
		val: 'string_val'
		val1: 1
		val2: 1.0
	}) == '{"val":"string_val","val3":"0000-00-00T00:00:00.000Z"}'

	assert json.encode(StructTypeSkippedFields8{
		val: 'string_val'
		val1: 1
		val2: 1.0
	}) == '{"val":"string_val","val2":1.0}'
}
