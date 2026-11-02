import json2

const data = '
{
  "comments": {
    "26788945": {
      "id": "26788945",
      "message": "some comment 1"
    },
    "26788946": {
      "id": "26788946",
      "message": "some comment 2"
    },
    "26788947": {
      "id": "26788947",
      "message": "some comment 3"
    }
  },
  "comments2": {
    "26788945": true,
    "26788946": false,
    "26788947": true
  },
  "comments3": {
    "26788945": 1,
    "26788946": 2,
    "26788947": 3
  }
}
'

pub struct Comment {
	id      string
	message string
}

struct Comments {
mut:
	comments  map[string]Comment
	comments2 map[string]bool
	comments3 map[string]int
}

fn test_decode_string_keyed_maps() {
	mut root := json2.decode[Comments](data)!
	assert root.comments.len == 3
	assert root.comments['26788945']!.id == '26788945'
	assert root.comments['26788946']!.id == '26788946'
	assert root.comments['26788947']!.id == '26788947'

	assert root.comments2['26788945']! == true
	assert root.comments2['26788946']! == false
	assert root.comments2['26788947']! == true

	assert root.comments3['26788945']! == 1
	assert root.comments3['26788946']! == 2
	assert root.comments3['26788947']! == 3
}

struct IntegerKeyFields {
	names    map[int]string
	numbers  map[int]int
	nested   map[int]map[int]string
	optional map[int]?int
}

fn test_decode_integer_map_keys() {
	names := json2.decode[map[int]string]('{"1":"a","-2":"b"}')!
	assert names.len == 2
	assert names[1] == 'a'
	assert names[-2] == 'b'
	numbers := json2.decode[map[int]int]('{"7":3}')!
	assert numbers[7] == 3
	fields := json2.decode[IntegerKeyFields]('{"names":{"1":"a"},"numbers":{"7":3},"nested":{"2":{"3":"c"}},"optional":{"4":null,"5":6}}')!
	assert fields.names[1] == 'a'
	assert fields.numbers[7] == 3
	assert fields.nested[2][3] == 'c'
	assert fields.optional.len == 2
	assert 4 in fields.optional
	assert fields.optional[4] == none
	present := fields.optional[5]
	assert present? == 6
}

fn test_decode_wide_integer_map_keys() {
	signed := json2.decode[map[i64]string]('{"-9223372036854775808":"min","9223372036854775807":"max"}')!
	assert signed[i64(-9223372036854775807) - 1] == 'min'
	assert signed[i64(9223372036854775807)] == 'max'
	unsigned := json2.decode[map[u64]string]('{"18446744073709551615":"max"}')!
	assert unsigned[u64(18446744073709551615)] == 'max'
	middle := json2.decode[map[u32]int]('{"4294967295":8}')!
	assert middle[u32(4294967295)] == 8
	small := json2.decode[map[u8]int]('{"255":7}')!
	assert small[u8(255)] == 7
}

fn test_decode_rune_map_keys() {
	runes := json2.decode[map[rune]int]('{"a":2}')!
	assert runes[rune(97)] == 2
}
