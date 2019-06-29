import json 

struct User {
	age  int
	nums []int
}

fn test_parse_user() {
	s := '{"age": 10, "nums": [1,2,3]}'
	u := json.decode(User, s) or {
		exit(1) 
		return
	}
	assert u.age == 10
	assert u.nums.len == 3
	assert u.nums[0] == 1 
	assert u.nums[1] == 2 
	assert u.nums[2] == 3 
}


