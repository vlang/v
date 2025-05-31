import x.json2.decoder2
import json as old_json
import benchmark
import time

// ./v -prod crun vlib/x/json/tests/c.v
// ./v wipe-cache && ./v -prod -cc gcc crun vlib/x/json2/decoder2/tests/bench.v
const max_iterations = 1_000_000
// const max_iterations = 10 // trying figure out it is slower in small loop. I guess it is `fulfill_nodes` related. Any suggestion?

pub struct Stru {
	val  int
	val2 string
	val3 Stru2
}

pub struct Stru2 {
	a         int
	churrasco string
}

type SumTypes = Stru | bool | int | string | time.Time
type StringAlias = string
type IntAlias = int

enum Enum {
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

fn main() {
	json_data := '{"_type": "Stru", "val": 1, "val2": "lala", "val3": {"a": 2, "churrasco": "leleu"}}'
	json_data1 := '{"val": "2"}'
	json_data2 := '{"val": 2}'

	println('Starting benchmark...')
	println('max_iterations: ${max_iterations}')
	println('\n***Structure and maps***')

	mut b := benchmark.start()

	// Stru **********************************************************

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[Stru](json_data)!
	}

	b.measure('decoder2.decode[Stru](json_data)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(Stru, json_data)!
	}

	b.measure('old_json.decode(Stru, json_data)!\n')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[SumTypes](json_data)!
	}

	b.measure('decoder2.decode[SumTypes](json_data)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(SumTypes, json_data)!
	}

	b.measure('old_json.decode(SumTypes, json_data)!\n')

	// StructType[string] **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[StructType[string]](json_data1)!
	}

	b.measure('decoder2.decode[StructType[string]](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(StructType[string], json_data1)!
	}

	b.measure('old_json.decode(StructType[string], json_data1)!\n')

	// StructTypeOption[string] **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[StructTypeOption[string]](json_data1)!
	}

	b.measure('decoder2.decode[StructTypeOption[string]](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(StructTypeOption[string], json_data1)!
	}

	b.measure('old_json.decode(StructTypeOption[string], json_data1)!\n')

	// StructType[int] **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[StructType[int]](json_data2)!
	}

	b.measure('decoder2.decode[StructType[int]](json_data2)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(StructType[int], json_data2)!
	}

	b.measure('old_json.decode(StructType[int], json_data2)!\n')

	// map[string]string **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[map[string]string](json_data1)!
	}

	b.measure('decoder2.decode[map[string]string](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(map[string]string, json_data1)!
	}

	b.measure('old_json.decode(map[string]string, json_data1)!\n')

	// array **********************************************************

	println('\n***arrays***')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[[]int]('[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!
	}

	b.measure("decoder2.decode[[]int]('[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!")

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode([]int, '[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!
	}

	b.measure("old_json.decode([]int, '[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]')!\n")

	println('\n***simple types***')

	// int **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[int]('2')!
	}

	b.measure("decoder2.decode[int]('2')!")

	// bool **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[bool]('true')!
	}

	b.measure("decoder2.decode[bool]('true')!")

	// time.Time **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[time.Time]('"2022-03-11T13:54:25"')!
	}

	b.measure("decoder2.decode[time.Time]('2022-03-11T13:54:25')!")

	// string **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[string]('"abcdefghijklimnopqrstuv"')!
	}

	b.measure('decoder2.decode[string](\'"abcdefghijklimnopqrstuv"\')!')

	// alias **********************************************************

	println('\n***alias***')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[IntAlias]('2')!
	}

	b.measure('decoder2.decode[IntAlias](2)!')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[StringAlias]('"abcdefghijklimnopqrstuv"')!
	}

	b.measure('decoder2.decode[StringAlias](\'"abcdefghijklimnopqrstuv"\')!')

	println('\n***Sumtypes***')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[SumTypes]('2')!
	}

	b.measure('decoder2.decode[SumTypes](2)!')

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[SumTypes]('"abcdefghijklimnopqrstuv"')!
	}

	b.measure('decoder2.decode[SumTypes](\'"abcdefghijklimnopqrstuv"\')!')
}
