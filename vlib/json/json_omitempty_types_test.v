import json

struct OmitEmptyStruct {
	bug Struct @[omitempty]
}

type MyAlias = string

struct OmitEmptyAlias {
	bug MyAlias @[omitempty]
}

struct Struct {
	name string
}

struct OmitEmptyMap {
	bug map[string]string @[omitempty]
}

struct OmitEmptyArray {
	bug []string @[omitempty]
}

type MySum = int | string

struct OmitEmptySumType {
	bug MySum @[omitempty]
}

struct FNumStruct {
	f_num f64
}

struct OmitEmptyFNumStruct {
	bug FNumStruct @[omitempty]
}

fn test_struct() {
	test := OmitEmptyStruct{
		bug: Struct{}
	}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptyStruct{
		bug: Struct{
			name: 'mybug'
		}
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}

fn test_fnum_struct() {
	test := OmitEmptyFNumStruct{
		bug: FNumStruct{}
	}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptyFNumStruct{
		bug: FNumStruct{
			f_num: 1.5
		}
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}

fn test_alias() {
	test := OmitEmptyAlias{
		bug: ''
	}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptyAlias{
		bug: 'foo'
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}

fn test_map() {
	test := OmitEmptyMap{
		bug: {}
	}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptyMap{
		bug: {
			'foo': 'bar'
		}
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}

fn test_sumtype() {
	test := OmitEmptySumType{}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptySumType{
		bug: 1
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}

fn test_array() {
	test := OmitEmptyArray{
		bug: []
	}
	encoded := json.encode(test)
	dump(encoded)
	test2 := OmitEmptyArray{
		bug: ['1']
	}
	encoded2 := json.encode(test2)
	dump(encoded2)
}
