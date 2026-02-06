import os
import json
import x.json2
import time
import benchmark

// recommendations:
// ./v wipe-cache && MAX_ITERATIONS=100_000 ./v run vlib/v/tests/bench/bench_json_vs_json2.v
// ./v wipe-cache && MAX_ITERATIONS=100_000 ./v -prod crun vlib/v/tests/bench/bench_json_vs_json2.v
// ./v wipe-cache && ./v -gc boehm_leak -o testcase_leak -prod vlib/v/tests/bench/bench_json_vs_json2.v && ./testcase_leak 2>leaks.txt
// ./v wipe-cache && ./v -prod vlib/v/tests/bench/bench_json_vs_json2.v -o b_out && valgrind --track-origins=yes ./b_out

const max_iterations = os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

fn main() {
	benchmark_measure_json_vs_json2_on_complex_struct()!
	println('')
	benchmark_measure_decode_by_type()!
	println('')
	benchmark_measure_encode_by_type()!
	println('')
	benchmark_measure_encode_by_alias_type()!
}

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enum

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

struct Person {
	name       string
	age        int
	created_at time.Time
}

fn benchmark_measure_json_vs_json2_on_complex_struct() ! {
	println(@FN)
	dump('👈')
	s := '{"name":"Bilbo Baggins","age":99,"created_at":1670840340}'
	mut b := benchmark.start()
	for _ in 0 .. max_iterations {
		p := json2.decode[Person](s)!
		if p.age != 99 {
			return error('json2.decode ${p}')
		}
	}
	b.measure('json2.decode')
	for _ in 0 .. max_iterations {
		p := json.decode(Person, s)!
		if p.age != 99 {
			return error('json.decode ${p}')
		}
	}
	b.measure('json.decode\n')

	measure_json_encode_old_vs_new(json.decode(Person, s)!)!
}

fn benchmark_measure_encode_by_type() ! {
	println(@FN)
	dump('👈')

	big_multiple_type_string := '✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t✔な🐈\t'
	println('✔な🐈\t = ${big_multiple_type_string.len}')
	measure_json_encode_old_vs_new(StructType[string]{big_multiple_type_string})!

	big_emoji_string := '🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀🐈🐟🐧🐀💀'
	println('🐈🐟🐧🐀💀 = ${big_emoji_string.len}')
	measure_json_encode_old_vs_new(StructType[string]{big_emoji_string})!

	big_no_ancii_string := 'ひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがなひらがな'
	println('ひらがな = ${big_no_ancii_string.len}')
	measure_json_encode_old_vs_new(StructType[string]{big_no_ancii_string})!

	big_string := 'jhsbhjhajbujhfbdjhgbxdljgbxdlkjgbxdlkgjbdlfjbszldjkfbdljgbzsljfzsbkfdjsbfljhsdhbfljzhsdbfljzshfblszdjfbjzhdsbfjzsdhbfljsdhbfljzsdfblzjsdfbzsjdfbhljzsdhfbljzsbfjsdbfjshdbfljzsdhbfljzsdhbfljszdbhfljzsbfljhzsbdfljhzbsdljfbsdljfbzlsjfhdbzdsljhfbszdljhfbzsldjfhbszdljhfbzsdljfhbzsdjhfbdsljhfbljsdhbflsjdhjhsbh jhajbujhfbdjhgbxdljgbxdlkjgbxdlkgjbdlfjbszldjkfbdljgbzsljfzsbkfdjsbfljhsdhbfljzhsdbfljzshfblszdjfbjzhdsbfjzsdhbfljsdhbfljzsdfblzjsdfbzsjdfbhljzsdhfbljzsbfjsdbfjshdbfljzsdhbfljzsdhbfljszdbhfljzsbfljhzsbdfljhzbsdljfbsdljfbzlsjfhdbzdsljhfbszdljhfbzsldjfhbszdljhfbzsdljfhbzsdjhfbdsljhfbljsdhbflsjdh'

	println('big string length = ${big_string.len}')
	measure_json_encode_old_vs_new(StructType[string]{big_string})!

	println('empty string')
	measure_json_encode_old_vs_new(StructType[string]{})!

	println('empty time.Time')
	measure_json_encode_old_vs_new(StructType[time.Time]{})!
	println('time.utc()')
	measure_json_encode_old_vs_new(StructType[time.Time]{time.utc()})!
	println('time.now()')
	measure_json_encode_old_vs_new(StructType[time.Time]{time.now()})!
	measure_json_encode_old_vs_new(StructType[int]{})!
	measure_json_encode_old_vs_new(StructType[u64]{u64(-1)})! // 18446744073709551615
	measure_json_encode_old_vs_new(StructType[f64]{})!
	measure_json_encode_old_vs_new(StructType[bool]{false})!

	println('empty array')
	measure_json_encode_old_vs_new(StructType[[]int]{})!

	println('array with 10 elements [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]')
	measure_json_encode_old_vs_new(StructType[[]int]{[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]})!
	measure_json_encode_old_vs_new(StructType[StructType[int]]{ val: StructType[int]{} })!
	measure_json_encode_old_vs_new(StructType[Enum]{})!
	measure_json_encode_old_vs_new(StructType[SumTypes]{1})!
}

fn benchmark_measure_encode_by_alias_type() ! {
	println(@FN)
	dump('👈')
	measure_json_encode_old_vs_new(StructType[StringAlias]{})!
	measure_json_encode_old_vs_new(StructType[TimeAlias]{})!
	measure_json_encode_old_vs_new(StructType[IntAlias]{})!
	measure_json_encode_old_vs_new(StructType[BoolAlias]{})!
	measure_json_encode_old_vs_new(StructType[StructAlias]{})!
}

fn benchmark_measure_decode_by_type() ! {
	println(@FN)
	dump('👈')
	mut b := benchmark.start()

	vs := '{"val": ""}'
	for _ in 0 .. max_iterations {
		d := json2.decode[StructType[string]](vs)!
		if d.val != '' {
			return error('json2.decode ${d}')
		}
	}
	b.measure('json2.decode StructType[string]')
	for _ in 0 .. max_iterations {
		d := json.decode(StructType[string], vs)!
		if d.val != '' {
			return error('json.decode ${d}')
		}
	}
	b.measure(' json.decode StructType[string]\n')

	vb := '{"val": true}'
	for _ in 0 .. max_iterations {
		d := json2.decode[StructType[bool]](vb) or { panic('') }
		if !d.val {
			return error('json2.decode ${d}')
		}
	}
	b.measure('json2.decode StructType[bool]')
	for _ in 0 .. max_iterations {
		d := json.decode(StructType[bool], vb) or { panic('') }
		if !d.val {
			return error('json.decode ${d}')
		}
	}
	b.measure(' json.decode StructType[bool]\n')

	v0 := '{"val": 0}'
	for _ in 0 .. max_iterations {
		d := json2.decode[StructType[int]](v0)!
		if d.val != 0 {
			return error('json2.decode ${d}')
		}
	}
	b.measure('json2.decode StructType[int]')
	for _ in 0 .. max_iterations {
		d := json.decode(StructType[int], v0)!
		if d.val != 0 {
			return error('json.decode ${d}')
		}
	}
	b.measure(' json.decode StructType[int]\n')

	vt := '{"val": "2015-01-06 15:47:32"}'
	for _ in 0 .. max_iterations {
		d := json2.decode[StructType[time.Time]](vt)!
		if d.val.year != 2015 {
			return error('json2.decode ${d}')
		}
	}
	b.measure('json2.decode StructType[time.Time]')
	for _ in 0 .. max_iterations {
		d := json.decode(StructType[time.Time], vt)!
		if d.val.year != 1970 { // note json.decode here is buggy
			return error('json2.decode ${d}')
		}
	}
	b.measure(' json.decode StructType[time.Time]\n')
}

fn measure_json_encode_old_vs_new[T](val T) ! {
	typename := typeof[T]().name
	mut b := benchmark.start()
	for _ in 0 .. max_iterations {
		e := json2.encode(val)
		if e[0] != `{` {
			return error('json2.encode ${e}')
		}
	}
	b.measure('json2.encode ${typename}')
	for _ in 0 .. max_iterations {
		e := json.encode(val)
		if e[0] != `{` {
			return error('json.encode ${e}')
		}
	}
	b.measure(' json.encode ${typename}\n')
}
