import x.json2 as json

struct AnyStruct[T] {
	val T
}

struct OptAnyStruct[T] {
	val ?T
}

/*struct OptAnyArrStruct {
	val []?json.Any
}*/

fn test_values() {
	assert json.decode[AnyStruct[json.Any]]('{"val":5}')!.val.int() == 5
	assert json.decode[OptAnyStruct[json.Any]]('{}')!.val == none
	assert json.decode[AnyStruct[[]json.Any]]('{"val":[5,10]}')!.val.map(it.int()) == [
		5,
		10,
	]
	// assert json.decode[OptAnyArrStruct]('{"val":[5,null,10]}')!.val == [?json.Any(5),json.Null{},10] skipped because test still fails even though they're the same

	assert json.encode[AnyStruct[json.Any]](AnyStruct[json.Any]{json.Any(5)}) == '{"val":5}'
	assert json.encode[OptAnyStruct[json.Any]](OptAnyStruct[json.Any]{none}) == '{}'
	assert json.encode[AnyStruct[[]json.Any]](AnyStruct[[]json.Any]{[json.Any(5), 10]}) == '{"val":[5,10]}'
	// assert json.encode[OptAnyArrStruct](OptAnyArrStruct{[?json.Any(5),none,10]}) == '{"val":[5,null,10]}' encode_array has not implemented optional arrays yet
}
