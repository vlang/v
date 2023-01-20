import os
import json
import x.json2
import time
import benchmark

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enumerates

type SumTypes = StructType[string] | bool | int | string | time.Time

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

struct Person {
	name       string
	age        int
	created_at time.Time
}

// recommendations
// MAX_ITERATIONS=90_000 ./v run vlib/v/tests/bench/bench_json_vs_json2.v
// MAX_ITERATIONS=90_000 ./v -no-bounds-checking -prod -cc clang-15 crun vlib/v/tests/bench/bench_json_vs_json2.v

fn main() {
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()
	s := '{"name":"Bilbo Baggins","age":99,"created_at":1670840340}'
	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		p := json2.decode[Person](s)!
		if p.age != 99 {
			println('error: ${p}')
		}
	}
	b.measure('json2.decode')

	for _ in 0 .. max_iterations {
		p := json.decode(Person, s)!
		if p.age != 99 {
			println('error: ${p}')
		}
	}
	b.measure('json.decode')

	// encoding measurements:
	p := json.decode(Person, s)!

	for _ in 0 .. max_iterations {
		es := json2.encode(p)
		if es[0] != `{` {
			println('json2.encode error: ${es}')
		}
	}
	b.measure('json2.encode')

	for _ in 0 .. max_iterations {
		es := json.encode(p)
		if es[0] != `{` {
			println('json.encode error: ${es}')
		}
	}
	b.measure('json.encode')
	benchmark_measure_encode_by_type()
	benchmark_measure_encode_by_alias_type()
	benchmark_measure_decode_by_type()
}

fn benchmark_measure_encode_by_type() {
	println('')
	println('benchmark_measure_encode_by_type')
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		json2.encode(StructType[string]{})
	}
	b.measure('json2.encode [string]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[time.Time]{})
	}
	b.measure('json2.encode [time.Time]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[int]{})
	}
	b.measure('json2.encode [int]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[f64]{})
	}
	b.measure('json2.encode [f64]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[bool]{})
	}
	b.measure('json2.encode [bool]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[[]int]{})
	}
	b.measure('json2.encode [[]int]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[StructType[int]]{
			val: StructType[int]{}
		})
	}
	b.measure('json2.encode [StructType[int]]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[Enumerates]{})
	}
	b.measure('json2.encode [Enumerates]')
}

fn benchmark_measure_encode_by_alias_type() {
	println('')
	println('benchmark_measure_encode_by_alias_type')
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		json2.encode(StructType[StringAlias]{})
	}
	b.measure('json2.encode [StringAlias]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[TimeAlias]{})
	}
	b.measure('json2.encode [TimeAlias]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[IntAlias]{})
	}
	b.measure('json2.encode [IntAlias]')

	for _ in 0 .. max_iterations {
		json2.encode(StructType[BoolAlias]{})
	}
	b.measure('json2.encode [BoolAlias]')

	for _ in 0 .. max_iterations {
		json.encode(StructType[StructAlias]{})
	}
	b.measure('json2.encode [StructAlias]')
}

fn benchmark_measure_decode_by_type() {
	println('')
	println('benchmark_measure_decode_by_type')
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		json2.decode[StructType[string]]('{"val": ""}') or { panic('') }
	}
	b.measure('json2.decode [string]')

	for _ in 0 .. max_iterations {
		json2.decode[StructType[bool]]('{"val": ""}') or { panic('') }
	}
	b.measure('json2.decode [bool]')

	for _ in 0 .. max_iterations {
		json2.decode[StructType[int]]('{"val": ""}') or { panic('') }
	}
	b.measure('json2.decode [int]')

	for _ in 0 .. max_iterations {
		json2.decode[StructType[time.Time]]('{"val": ""}') or { panic('') }
	}
	b.measure('json2.decode [time.Time]')
}
