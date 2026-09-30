// vtest vflags: -w

module main

import json2

pub struct Data {
	name string
	data [][]f64
}

fn test_main() {
	json_data := '{"name":"test","data":[[1,2,3],[4,5,6]]}'
	info := json2.decode[Data](json_data)!
	assert info == Data{
		name: 'test'
		data: [[1.0, 2, 3], [4.0, 5, 6]]
	}
}
