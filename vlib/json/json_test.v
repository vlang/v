import json

struct User {
	age       	int
	nums      	[]int
	last_name 	string 	[json:lastName]
	is_registered	bool 	[json:IsRegistered]
    typ int  [json:'type']
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3], "type": 0, "lastName": "Johnson", "IsRegistered": true}'
	u := json.decode(User, s) or {
		exit(1)
	}
	assert u.age == 10
	assert u.last_name == 'Johnson'
	assert u.is_registered == true
	assert u.nums.len == 3
	assert u.nums[0] == 1
	assert u.nums[1] == 2
	assert u.nums[2] == 3
    assert u.typ == 0
}

fn test_encode_user(){
	usr := User{ age: 10, nums: [1,2,3], last_name: 'Johnson', is_registered: true, typ: 0}
	expected := '{"age":10,"nums":[1,2,3],"lastName":"Johnson","IsRegistered":true,"type":0}'
	out := json.encode(usr)
	assert out == expected
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
