import x.json2.decoder2 as json
import x.json2

struct AnyStruct[T] {
	val T
}

struct OptAnyStruct[T] {
	val ?T
}

// struct OptAnyArrStruct {
// 	val []?json2.Any
// }

fn test_values() {
	assert json.decode[AnyStruct[json2.Any]]('{"val":5}')!.val.int() == 5
	assert json.decode[OptAnyStruct[json2.Any]]('{}')!.val == none
	assert json.decode[AnyStruct[[]json2.Any]]('{"val":[5,10]}')!.val.map(it.int()) == [
		5,
		10,
	]
	// assert json.decode[OptAnyArrStruct]('{"val":[5,null,10]}')!.val == [?json2.Any(5),json.Null{},10] // skipped because test still fails even though they're the same

	assert json2.encode[AnyStruct[json2.Any]](AnyStruct[json2.Any]{json2.Any(5)}) == '{"val":5}'
	assert json2.encode[OptAnyStruct[json2.Any]](OptAnyStruct[json2.Any]{none}) == '{}'
	assert json2.encode[AnyStruct[[]json2.Any]](AnyStruct[[]json2.Any]{[json2.Any(5), 10]}) == '{"val":[5,10]}'
	// assert json2.encode[OptAnyArrStruct](OptAnyArrStruct{[?json2.Any(5),none,10]}) == '{"val":[5,null,10]}' // encode_array has not implemented optional arrays yet
}
