import x.json2 as json
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

type SumTypes = bool | int | string

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

struct StructTypeOptional[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn test_types() {
	assert json.encode(StructType[string]{}) == '{"val":""}'
	assert json.encode(StructType[string]{ val: '' }) == '{"val":""}'
	assert json.encode(StructType[string]{ val: 'a' }) == '{"val":"a"}'

	assert json.encode(StructType[bool]{}) == '{"val":false}'
	assert json.encode(StructType[bool]{ val: false }) == '{"val":false}'
	assert json.encode(StructType[bool]{ val: true }) == '{"val":true}'

	assert json.encode(StructType[int]{}) == '{"val":0}'
	assert json.encode(StructType[int]{ val: 0 }) == '{"val":0}'
	assert json.encode(StructType[int]{ val: 1 }) == '{"val":1}'

	assert json.encode(StructType[time.Time]{}) == '{"val":"0000-00-00T00:00:00.000Z"}'
	assert json.encode(StructType[time.Time]{ val: fixed_time }) == '{"val":"2022-03-11T13:54:25.000Z"}'
}

fn test_optional_types() {
	assert json.encode(StructTypeOptional[string]{ val: none }) == '{}'
	assert json.encode(StructTypeOptional[string]{}) == '{}'
	assert json.encode(StructTypeOptional[string]{ val: '' }) == '{"val":""}'
	assert json.encode(StructTypeOptional[string]{ val: 'a' }) == '{"val":"a"}'

	assert json.encode(StructTypeOptional[bool]{ val: none }) == '{}'
	assert json.encode(StructTypeOptional[bool]{}) == '{}'
	assert json.encode(StructTypeOptional[bool]{ val: false }) == '{"val":false}'
	assert json.encode(StructTypeOptional[bool]{ val: true }) == '{"val":true}'

	assert json.encode(StructTypeOptional[int]{ val: none }) == '{}'
	assert json.encode(StructTypeOptional[int]{}) == '{}'
	assert json.encode(StructTypeOptional[int]{ val: 0 }) == '{"val":0}'
	assert json.encode(StructTypeOptional[int]{ val: 1 }) == '{"val":1}'

	assert json.encode(StructTypeOptional[time.Time]{}) == '{}'
	assert json.encode(StructTypeOptional[time.Time]{ val: fixed_time }) == '{"val":"2022-03-11T13:54:25.000Z"}'
}

fn test_array() {
	assert json.encode(StructType[[]string]{}) == '{"val":[]}'
	assert json.encode(StructType[[]string]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]string]{ val: ['0'] }) == '{"val":["0"]}'
	assert json.encode(StructType[[]string]{ val: ['1'] }) == '{"val":["1"]}'

	assert json.encode(StructType[[]int]{}) == '{"val":[]}'
	assert json.encode(StructType[[]int]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]int]{ val: [0] }) == '{"val":[0]}'
	assert json.encode(StructType[[]int]{ val: [1] }) == '{"val":[1]}'
	assert json.encode(StructType[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

	assert json.encode(StructType[[]byte]{}) == '{"val":[]}'
	assert json.encode(StructType[[]byte]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]byte]{ val: [byte(0)] }) == '{"val":[0]}'
	assert json.encode(StructType[[]byte]{ val: [byte(1)] }) == '{"val":[1]}'
	assert json.encode(StructType[[]byte]{ val: [byte(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

	assert json.encode(StructType[[]i64]{}) == '{"val":[]}'
	assert json.encode(StructType[[]i64]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]i64]{ val: [i64(0)] }) == '{"val":[0]}'
	assert json.encode(StructType[[]i64]{ val: [i64(1)] }) == '{"val":[1]}'
	assert json.encode(StructType[[]i64]{ val: [i64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

	assert json.encode(StructType[[]u64]{}) == '{"val":[]}'
	assert json.encode(StructType[[]u64]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]u64]{ val: [u64(0)] }) == '{"val":[0]}'
	assert json.encode(StructType[[]u64]{ val: [u64(1)] }) == '{"val":[1]}'
	assert json.encode(StructType[[]u64]{ val: [u64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

	assert json.encode(StructType[[]f64]{}) == '{"val":[]}'
	assert json.encode(StructType[[]f64]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]f64]{ val: [f64(0)] }) == '{"val":[0.0]}'
	assert json.encode(StructType[[]f64]{ val: [f64(1)] }) == '{"val":[1.0]}'
	assert json.encode(StructType[[]f64]{ val: [f64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0.0,1.0,0.0,2.0,3.0,2.0,5.0,1.0]}'

	assert json.encode(StructType[[]bool]{}) == '{"val":[]}'
	assert json.encode(StructType[[]bool]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructType[[]bool]{ val: [true] }) == '{"val":[true]}'
	assert json.encode(StructType[[]bool]{ val: [false] }) == '{"val":[false]}'
	assert json.encode(StructType[[]bool]{ val: [false, true, false] }) == '{"val":[false,true,false]}'
}

fn test_optional_array() {
	assert json.encode(StructTypeOptional[[]int]{ val: none }) == '{}'
	assert json.encode(StructTypeOptional[[]int]{}) == '{}'
	assert json.encode(StructTypeOptional[[]int]{ val: [] }) == '{"val":[]}'
	assert json.encode(StructTypeOptional[[]int]{ val: [0] }) == '{"val":[0]}'
	assert json.encode(StructTypeOptional[[]int]{ val: [1] }) == '{"val":[1]}'
	assert json.encode(StructTypeOptional[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'
}

fn test_alias() {
	assert json.encode(StructType[StringAlias]{}) == '{"val":""}'
	assert json.encode(StructType[StringAlias]{ val: '' }) == '{"val":""}'
	assert json.encode(StructType[StringAlias]{ val: 'a' }) == '{"val":"a"}'

	assert json.encode(StructType[BoolAlias]{}) == '{"val":false}'
	assert json.encode(StructType[BoolAlias]{ val: false }) == '{"val":false}'
	assert json.encode(StructType[BoolAlias]{ val: true }) == '{"val":true}'

	assert json.encode(StructType[IntAlias]{}) == '{"val":0}'
	assert json.encode(StructType[IntAlias]{ val: 0 }) == '{"val":0}'
	assert json.encode(StructType[IntAlias]{ val: 1 }) == '{"val":1}'
}
