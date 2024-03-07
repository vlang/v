import x.new_json
import json as old_json
import benchmark
import time

// ./v -prod crun vlib/x/json/tests/c.v
const max_iterations = 100_000
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

type SumTypes = StructType[string] | bool | int | string | time.Time

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
	json_data := '{"val": 1, "val2": "lala", "val3": {"a": 2, "churrasco": "leleu"}}'
	json_data1 := '{"val": "2"}'
	// json_data2 := '{"val": 2}'

	println(new_json.decode[Stru](json_data)!)

	mut b := benchmark.start()

	// Stru **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := new_json.decode[Stru](json_data)!
	}

	b.measure('new_json.decode[Stru](json_data)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(Stru, json_data)!
	}

	b.measure('old_json.decode(Stru, json_data)!\n')

	// StructType[string] **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := new_json.decode[StructType[string]](json_data1)!
	}

	b.measure('new_json.decode[StructType[string]](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(StructType[string], json_data1)!
	}

	b.measure('old_json.decode(StructType[string], json_data1)!\n')

	// StructTypeOption[string] **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := new_json.decode[StructTypeOption[string]](json_data1)!
	}

	b.measure('new_json.decode[StructTypeOption[string]](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(StructTypeOption[string], json_data1)!
	}

	b.measure('old_json.decode(StructTypeOption[string], json_data1)!\n')

	// map[string]string **********************************************************
	for i := 0; i < max_iterations; i++ {
		_ := new_json.decode[map[string]string](json_data1)!
	}

	b.measure('new_json.decode[map[string]string](json_data1)!')

	for i := 0; i < max_iterations; i++ {
		_ := old_json.decode(map[string]string, json_data1)!
	}

	b.measure('old_json.decode(map[string]string, json_data1)!\n')
}
