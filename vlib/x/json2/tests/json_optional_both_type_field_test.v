import x.json2
import rand

struct Response[T] {
	code int
	msg  string
	data ?T
}

struct UserInfo {
	name string
	age  int @[json: 'type']
}

fn get_optional_data() ?UserInfo {
	return if rand.u8() >= 0 { UserInfo{'Jay Chou', 46} } else { none }
}

fn response_to_string[T](response Response[T]) string {
	return json2.encode(response)
}

fn test_main() {
	data := get_optional_data()
	response := Response{1, 'success', data}
	res := response_to_string(response)
	assert res == '{"code":1,"msg":"success","data":{"name":"Jay Chou","type":46}}'
}
