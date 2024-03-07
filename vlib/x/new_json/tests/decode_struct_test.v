import x.new_json as json
import time

const fixed_time = time.Time{
	year: 2022
	month: 3
	day: 11
	hour: 13
	minute: 54
	second: 25
	unix: 1647006865
}

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enumerates

type SumTypes = StructType[string] | []SumTypes | []string | bool | string | time.Time | u32

enum Enumerates {
	a
	b
	c
	d
	e = 99
	f
}

struct StructType[T] {
mut:
	val T
}

struct StructTypeOption[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn test_types() {
	assert json.decode[StructType[string]]('{"val": ""}')!.val == ''

	assert json.decode[StructType[string]]('{"val": "2"}')!.val == '2'

	assert json.decode[StructType[int]]('{"val": 2}')!.val == 2

	assert json.decode[StructType[SumTypes]]('{"val": "2"}')!.val == SumTypes('2')
	assert json.decode[StructType[SumTypes]]('{"val": 2}')!.val == SumTypes(u32(2))

	assert json.decode[StructType[StructType[string]]]('{"val": {"val": "test"}}')!.val.val == 'test'

	assert json.decode[StructType[map[string]string]]('{"val": {"val": "test"}}')!.val['val'] == 'test'

	// assert json.decode[StructType[map[string]Any]]('{"val": {"val": "test"}}')!.val.val == 'test'

	// assert json.decode[StructType[Enumerates]]('{"val": 2}')!.val == Enumerates(2)

	// assert json.decode[StructType[IntAlias]]('{"val": 2}')!.val == IntAlias(2)

	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.year == fixed_time.year
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.month == fixed_time.month
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.day == fixed_time.day
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.hour == fixed_time.hour
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.minute == fixed_time.minute
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.second == fixed_time.second
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix == fixed_time.unix
}

fn test_option_types() {
	if x := json.decode[StructTypeOption[string]]('{}')!.val {
		assert false, 'Should return none'
	} else {
		assert err.msg() == ''
		assert true
	}

	if x := json.decode[StructTypeOption[string]]('{"val": "2"}')!.val {
		assert x == '2'
	} else {
		assert false, 'Should not return none'
	}

	// if x := json.decode[StructTypeOption[int]]('{"val": 2}')!.val {
	// 	assert x == 2
	// } else {
	// 	assert false, 'Should not return none'
	// }

	// assert json.decode[StructTypeOption[string]]('{"val": "2"}')!.val == '2'

	// assert json.decode[StructTypeOption[int]]('{"val": 2}')!.val == 2

	// assert json.decode[StructTypeOption[SumTypes]]('{"val": "2"}')!.val == SumTypes('2')
	// assert json.decode[StructTypeOption[SumTypes]]('{"val": 2}')!.val == SumTypes(2)

	// assert json.decode[StructTypeOption[Enumerates]]('{"val": 2}')!.val == Enumerates(2)

	// assert json.decode[StructTypeOption[IntAlias]]('{"val": 2}')!.val == IntAlias(2)

	// assert json.decode[StructTypeOption[StructType[string]]]('{"val": {"val": "test"}}')!.val.val == 'test'

	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.year == fixed_time.year
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.month == fixed_time.month
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.day == fixed_time.day
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.hour == fixed_time.hour
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.minute == fixed_time.minute
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.second == fixed_time.second
	// assert json.decode[StructTypeOption[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix == fixed_time.unix
}

fn test_pointer_types() {
	// assert json.decode[StructTypePointer[string]]('{}')!.val == unsafe { nil }

	// assert json.decode[StructTypePointer[string]]('{"val": ""}')!.val == ''

	// assert json.decode[StructTypePointer[string]]('{"val": "2"}')!.val == '2'

	// assert json.decode[StructTypePointer[int]]('{"val": 2}')!.val == 2

	// assert json.decode[StructTypePointer[SumTypes]]('{"val": "2"}')!.val == SumTypes('2')
	// assert json.decode[StructTypePointer[SumTypes]]('{"val": 2}')!.val == SumTypes(2)

	// assert json.decode[StructTypePointer[Enumerates]]('{"val": 2}')!.val == Enumerates(2)

	// assert json.decode[StructTypePointer[IntAlias]]('{"val": 2}')!.val == IntAlias(2)

	// assert json.decode[StructTypePointer[StructType[string]]]('{"val": {"val": "test"}}')!.val.val == 'test'

	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.year == fixed_time.year
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.month == fixed_time.month
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.day == fixed_time.day
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.hour == fixed_time.hour
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.minute == fixed_time.minute
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.second == fixed_time.second
	// assert json.decode[StructTypePointer[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix == fixed_time.unix
}
