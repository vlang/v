import json

struct OmitEmptyStruct {
	bug Struct [omitempty]
}

type MyAlias = string

struct OmitEmptyAlias {
	bug MyAlias [omitempty]
}

struct Struct {
	name string
}

struct OmitEmptyMap {
	bug map[string]string [omitempty]
}

struct OmitEmptyArray {
	bug []string [omitempty]
}

type MySum = int | string

struct OmitEmptySumType {
	bug MySum [omitempty]
}

struct FNumStruct {
	f_num f64
}

struct OmitEmptyFNumStruct {
	bug FNumStruct [omitempty]
}

fn main() {
	test_struct()
	test_alias()
	test_map()
	test_sumtype()
	test_array()
}

fn test_struct() {
	test2 := OmitEmptyStruct{
		bug: Struct{}
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
	test := OmitEmptyStruct{
		bug: Struct{
			name: 'mybug'
		}
	}
	encoded := json.encode(test)
	dump(encoded)
}

fn test_fnum_struct() {
	test := OmitEmptyFNumStruct{
		bug: FNumStruct{}
	}
	encode := json.encode(test)
	dump(encode)
	test2 := OmitEmptyFNumStruct{
		bug: FNumStruct{
			f_num: 1.5
		}
	}
	encoded := json.encode(test)
	dump(encoded)
}

fn test_alias() {
	test3 := OmitEmptyAlias{
		bug: ''
	}
	encoded3 := json.encode(test3)
	dump(encoded3)
	test4 := OmitEmptyAlias{
		bug: 'foo'
	}
	encoded4 := json.encode(test4)
	dump(encoded4)
}

fn test_map() {
	test3 := OmitEmptyMap{
		bug: {}
	}
	encoded3 := json.encode(test3)
	dump(encoded3)
	test4 := OmitEmptyMap{
		bug: {
			'foo': 'bar'
		}
	}
	encoded4 := json.encode(test4)
	dump(encoded4)
}

fn test_sumtype() {
	test3 := OmitEmptySumType{}
	encoded3 := json.encode(test3)
	dump(encoded3)
	test4 := OmitEmptySumType{
		bug: 1
	}
	encoded4 := json.encode(test4)
	dump(encoded4)
}

fn test_array() {
	test3 := OmitEmptyArray{
		bug: []
	}
	encoded3 := json.encode(test3)
	dump(encoded3)
	test4 := OmitEmptyArray{
		bug: ['1']
	}
	encoded4 := json.encode(test4)
	dump(encoded4)
}
