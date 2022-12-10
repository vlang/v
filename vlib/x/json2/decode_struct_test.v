import x.json2 as json
import time

const random_time = time.Time{
	year: 2022
	month: 3
	day: 11
	hour: 13
	minute: 54
	second: 25
	unix: 0
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
	assert json.decode[StructType[int]]('{"val": "true"}')!.val == 1
	assert json.decode[StructType[int]]('{"val": "false"}')!.val == 0
	assert json.decode[StructType[int]]('{"val": true}')!.val == 1
	assert json.decode[StructType[int]]('{"val": false}')!.val == 0
}
