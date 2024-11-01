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
	val4 int
	val5 int
	val6 int
	val7 int
	val8 int
	val9 int
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
	json_data := '{"_type": "Stru", "val": 1, "val2": "lala", "val3": {"a": 2, "churrasco": "leleu"}, "val4": 2147483000, "val5": 2147483000, "val6": 2147483000, "val7": 2147483000, "val8": 2147483000, "val9": 2147483000}'
	json_data1 := '{"val": "2"}'
	json_data2 := '{"val": 2}'
	json_data_timestamp := '{"val": "2022-03-11T13:54:25Z"}'

	mut http_request := 'HTTP/1.1 200 OK\r\n'
	http_request += 'Content-Type: application/json\r\n'
	http_request += 'Host: localhost:8080\r\n'
	http_request += 'User-Agent: curl/7.68.0\r\n'
	http_request += 'Accept: */*\r\n'
	http_request += 'Connection: close\r\n'
	http_request += 'Content-Length: ${json_data.len}\r\n'
	http_request += '\r\n'
	body_position := http_request.len
	http_request += json_data // pos: 150

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

	// time.Time **********************************************************

	for i := 0; i < max_iterations; i++ {
		_ := decoder2.decode[StructType[time.Time]](json_data_timestamp)!
	}

	b.measure('decoder2.decode[StructType[time.Time]](json_data_timestamp)!')

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
		_ := decoder2.decode[time.Time]('"2022-03-11T13:54:25Z"')!
	}

	b.measure("decoder2.decode[time.Time]('2022-03-11T13:54:25Z')!")

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

	// // Uncomment this when #22710 is fixed
	// for i := 0; i < max_iterations; i++ {
	// 	_ := decoder2.decode[json2.Any](json_data2)!
	// }

	// b.measure('decoder2.decode[json2.Any](json_data)!')

	for i := 0; i < max_iterations; i++ {
		mut decoder := decoder2.Decoder{
			json_str: unsafe { http_request.str + body_position }
			json_len: json_data.len
		}

		decoder.check_json_format()!
		decoder2.check_if_json_match[Stru](json_data)!

		mut result := Stru{}
		decoder.current_node = decoder.values_info.head
		decoder.decode_value(mut &result)!
	}

	b.measure('raw decode from HTTP request')

	for i := 0; i < max_iterations; i++ {
		json_string_from_http_request := http_request[body_position..]

		_ := decoder2.decode[Stru](json_string_from_http_request)!
	}

	b.measure('decode from HTTP request')
}
