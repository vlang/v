import x.json2 as json

struct AnyStruct[T] {
	val T
}

struct OptAnyStruct[T] {
	val ?T
}

fn test_values() {
	assert typeof(json.decode[AnyStruct[json.Any]]('{"val":5}')!.val).idx == typeof[json.Any]().idx
	assert typeof(json.decode[OptAnyStruct[json.Any]]('{}')!.val).idx == typeof[?json.Any]().idx
	assert typeof(json.decode[AnyStruct[[]json.Any]]('{"val":[5,10]}')!.val).idx == typeof[[]json.Any]().idx
}
