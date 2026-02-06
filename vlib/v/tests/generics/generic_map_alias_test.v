import x.json2

pub struct AppJsonValue {
pub:
	file  string
	md5   string
	creat i64
}

pub type AppJson = map[string]AppJsonValue

fn test_main() {
	app_json1 := map[string]AppJsonValue{}
	res1 := json2.encode(app_json1)
	assert '${res1}' == '{}'

	app_json2 := AppJson{}
	res2 := json2.encode(app_json2)
	assert '${res2}' == '{}'
}
