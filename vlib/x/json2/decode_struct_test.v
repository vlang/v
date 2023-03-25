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
	assert json.decode[StructType[string]]('{"val": "0"}')!.val == '0'
	assert json.decode[StructType[string]]('{"val": "1"}')!.val == '1'
	assert json.decode[StructType[string]]('{"val": "2"}')!.val == '2'
	assert json.decode[StructType[string]]('{"val": 0}')!.val == '0'
	assert json.decode[StructType[string]]('{"val": 1}')!.val == '1'
	assert json.decode[StructType[string]]('{"val": 2}')!.val == '2'
	assert json.decode[StructType[string]]('{"val": "true"}')!.val == 'true'
	assert json.decode[StructType[string]]('{"val": "false"}')!.val == 'false'
	assert json.decode[StructType[string]]('{"val": true}')!.val == 'true'
	assert json.decode[StructType[string]]('{"val": false}')!.val == 'false'

	assert json.decode[StructType[bool]]('{"val": ""}')!.val == false
	assert json.decode[StructType[bool]]('{"val": "0"}')!.val == false
	assert json.decode[StructType[bool]]('{"val": "1"}')!.val == true
	assert json.decode[StructType[bool]]('{"val": "2"}')!.val == true
	assert json.decode[StructType[bool]]('{"val": 0}')!.val == false
	assert json.decode[StructType[bool]]('{"val": 1}')!.val == true
	assert json.decode[StructType[bool]]('{"val": 2}')!.val == true
	assert json.decode[StructType[bool]]('{"val": "true"}')!.val == true
	assert json.decode[StructType[bool]]('{"val": "false"}')!.val == false
	assert json.decode[StructType[bool]]('{"val": true}')!.val == true
	assert json.decode[StructType[bool]]('{"val": false}')!.val == false

	assert json.decode[StructType[int]]('{"val": ""}')!.val == 0
	assert json.decode[StructType[int]]('{"val": "0"}')!.val == 0
	assert json.decode[StructType[int]]('{"val": "1"}')!.val == 1
	assert json.decode[StructType[int]]('{"val": "2"}')!.val == 2
	assert json.decode[StructType[int]]('{"val": 0}')!.val == 0
	assert json.decode[StructType[int]]('{"val": 1}')!.val == 1
	assert json.decode[StructType[int]]('{"val": 2}')!.val == 2
	assert json.decode[StructType[int]]('{"val": "true"}')!.val == 0
	assert json.decode[StructType[int]]('{"val": "false"}')!.val == 0
	assert json.decode[StructType[int]]('{"val": true}')!.val == 1
	assert json.decode[StructType[int]]('{"val": false}')!.val == 0

	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val == fixed_time
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.year == 2001
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.month == 1
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.day == 5
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.hour == 0
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.minute == 0
	assert json.decode[StructType[time.Time]]('{"val": "2001-01-05"}')!.val.second == 0
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11 13:54:25.000"}')!.val == fixed_time
	assert json.decode[StructType[time.Time]]('{"val": 1647006865}')!.val == fixed_time
	assert json.decode[StructType[time.Time]]('{"val": "1647006865"}')!.val == fixed_time
	if x := json.decode[StructType[time.Time]]('{"val": "invalid time"}') {
		assert false
	} else {
		// dump(err)
		assert true
	}
}

// fn test_array() {
// 	// assert json.decode[StructType[[]string]]('{}')! == StructType[[]string]{}
// 	// assert json.decode[StructType[[]string]]('{"val":[]}')! == StructType[[]string]{
// 	// 	val: []
// 	// }
// 	// assert json.decode[StructType[[]string]]('{"val":["0"]}')! == StructType[[]string]{
// 	// 	val: ['0']
// 	// }
// 	// assert json.decode[StructType[[]string]]('{"val":["1"]}')! == StructType[[]string]{
// 	// 	val: ['1']
// 	// }

// 	// assert json.encode(StructType[[]int]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]int]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]int]{ val: [0] }) == '{"val":[0]}'
// 	// assert json.encode(StructType[[]int]{ val: [1] }) == '{"val":[1]}'
// 	// assert json.encode(StructType[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

// 	// assert json.encode(StructType[[]byte]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]byte]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]byte]{ val: [byte(0)] }) == '{"val":[0]}'
// 	// assert json.encode(StructType[[]byte]{ val: [byte(1)] }) == '{"val":[1]}'
// 	// assert json.encode(StructType[[]byte]{ val: [byte(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

// 	// assert json.encode(StructType[[]i64]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]i64]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]i64]{ val: [i64(0)] }) == '{"val":[0]}'
// 	// assert json.encode(StructType[[]i64]{ val: [i64(1)] }) == '{"val":[1]}'
// 	// assert json.encode(StructType[[]i64]{ val: [i64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

// 	// assert json.encode(StructType[[]u64]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]u64]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]u64]{ val: [u64(0)] }) == '{"val":[0]}'
// 	// assert json.encode(StructType[[]u64]{ val: [u64(1)] }) == '{"val":[1]}'
// 	// assert json.encode(StructType[[]u64]{ val: [u64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0,1,0,2,3,2,5,1]}'

// 	// assert json.encode(StructType[[]f64]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]f64]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]f64]{ val: [f64(0)] }) == '{"val":[0.0]}'
// 	// assert json.encode(StructType[[]f64]{ val: [f64(1)] }) == '{"val":[1.0]}'
// 	// assert json.encode(StructType[[]f64]{ val: [f64(0), 1, 0, 2, 3, 2, 5, 1] }) == '{"val":[0.0,1.0,0.0,2.0,3.0,2.0,5.0,1.0]}'

// 	// assert json.encode(StructType[[]bool]{}) == '{"val":[]}'
// 	// assert json.encode(StructType[[]bool]{ val: [] }) == '{"val":[]}'
// 	// assert json.encode(StructType[[]bool]{ val: [true] }) == '{"val":[true]}'
// 	// assert json.encode(StructType[[]bool]{ val: [false] }) == '{"val":[false]}'
// 	// assert json.encode(StructType[[]bool]{ val: [false, true, false] }) == '{"val":[false,true,false]}'

// 	// array_of_struct := [StructType[bool]{
// 	// 	val: true
// 	// }, StructType[bool]{
// 	// 	val: false
// 	// }]
// 	// assert json.encode(StructType[[]StructType[bool]]{ val: array_of_struct }) == '{"val":[{"val":true},{"val":false}]}'
// }
