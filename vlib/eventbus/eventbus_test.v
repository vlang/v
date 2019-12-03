import (
	eventbus
)

fn test_eventbus(){
	mut eb := eventbus.new()
	eb.subscriber.subscribe_once("on_test", on_test)
	assert eb.has_subscriber("on_test") == true
	assert eb.subscriber.is_subscribed("on_test") == true
	mut params := eventbus.Params{}
	params.put_string("eventbus", "vevent")
	eb.publish("on_test", params)
	assert eb.has_subscriber("on_test") == false
	assert eb.subscriber.is_subscribed("on_test") == false
	eb.subscriber.subscribe_once("on_test", on_test)
	assert eb.has_subscriber("on_test") == true
	assert eb.subscriber.is_subscribed("on_test") == true
	eb.clear_all()
	assert eb.has_subscriber("on_test") == false
	assert eb.subscriber.is_subscribed("on_test") == false
}

fn test_params(){
	mut params := eventbus.Params{}
	params.put_string("string", "vevent")
	params.put_int("int", 20)
	params.put_bool("bo", true)
	eventbus.put_array(mut params, "array", [1,2,3])
	eventbus.put_map(mut params, "map", "", {"hello": "world"})

	assert params.get_string("string") == "vevent"
	assert params.get_int("int") == 20
	assert params.get_bool("bo") == true
	arr := eventbus.get_array(params, "array", 0)
	assert arr[0] == 1
	m := params.get_string_map("map") 
	assert m["hello"] == "world"
}

fn on_test(p eventbus.Params) {
	assert p.get_string("eventbus") == "vevent"
}
