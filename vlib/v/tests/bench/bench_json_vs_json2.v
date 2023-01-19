import os
import json
import x.json2
import time
import benchmark

type SumTypes = int | string | time.Time

struct StructType[T] {
mut:
	val T
}

struct Person {
	name       string
	age        int
	created_at time.Time
}

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
}

fn benchmark_measure_encode_by_type() {
	println('')
	max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()
	types := ['int', 'bool', 'string', 'time.Time']
	// dump(types)
	mut b := benchmark.start()
	for typ in types {
		for _ in 0 .. max_iterations {
			match typ {
				'int' {
					json2.encode(StructType[int]{})
					continue
				}
				'bool' {
					json2.encode(StructType[bool]{})
					continue
				}
				'string' {
					json2.encode(StructType[string]{})
					continue
				}
				'time.Time' {
					json2.encode(StructType[time.Time]{})
					continue
				}
				else {
					// json2.encode(StructType[string]{ })
					// break
				}
			}
		}
		b.measure('json2.encode [${typ}]')
	}
}

fn benchmark_measure_decode_by_type() {
	// max_iterations := os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

	// mut b := benchmark.start()

	// for _ in 0 .. max_iterations {
	// 	p := json2.decode[Person](s)!
	// 	if p.age != 99 {
	// 		println('error: ${p}')
	// 	}
	// }
	// b.measure('json2.decode')
}
