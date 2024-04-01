import x.json2.decoder2 as json
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

	assert json.decode[StructType[map[string]string]]('{"val": {"val": "test"}}')!.val['val'] == 'test'

	assert json.decode[StructType[Enumerates]]('{"val": 0}')!.val == Enumerates.a
	assert json.decode[StructType[Enumerates]]('{"val": 1}')!.val == Enumerates.b

	assert json.decode[StructType[IntAlias]]('{"val": 2}')!.val == IntAlias(2)
	assert json.decode[StructType[StringAlias]]('{"val": "2"}')!.val == StringAlias('2')

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
}
