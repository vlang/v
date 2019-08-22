import json 

struct User {
	age  int
	nums []int
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3]}'
	u := json.decode(User, s) or {
		exit(1) 
	}
	assert u.age == 10
	assert u.nums.len == 3
	assert u.nums[0] == 1 
	assert u.nums[1] == 2 
	assert u.nums[2] == 3 
}

struct Color {
    space string
    point string [raw]
}

fn test_raw_json_field() {
    color := json.decode(Color, '{"space": "YCbCr", "point": {"Y": 123}}') or {
        println('text')
        return
    }
    assert color.point == '{"Y":123}'
    assert color.space == 'YCbCr'
}

