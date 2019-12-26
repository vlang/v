import (
	eventbus
)

fn test_eventbus(){
	mut eb := eventbus.new()
	eb.subscriber.subscribe_once("on_test", on_test)
	
	assert eb.has_subscriber("on_test")
	assert eb.subscriber.is_subscribed("on_test")

	mut params := eventbus.Params{}
	params.put_string("eventbus", "vevent")

	eb.publish("on_test", eb, params)

	assert !eb.has_subscriber("on_test")
	assert !eb.subscriber.is_subscribed("on_test")

	eb.subscriber.subscribe("on_test", on_test)

	assert eb.has_subscriber("on_test")
	assert eb.subscriber.is_subscribed("on_test")

	eb.clear_all()

	assert !eb.has_subscriber("on_test")
	assert !eb.subscriber.is_subscribed("on_test")
}

fn test_params(){
	mut params := eventbus.Params{}

	params.put_string("string", "some_string")
	params.put_int("int", 20)
	params.put_bool("bool", true)
	arr := [1,2,3]
	params.put_array("array", arr)
	mp :=  {"hello": "world"}
	params.put_map("map", mp)

	assert params.get_string("string") == "some_string"
	assert params.get_int("int") == 20
	assert params.get_bool("bool") == true

	g_arr := params.get_array("array", 0)
	assert g_arr[0] == 1 

	g_m := params.get_map("map", "")
	assert g_m["hello"] == "world"
}

fn on_test(sender voidptr, p eventbus.Params) {
	mut eb := *(*eventbus.EventBus(sender))
	
	eb.subscriber.subscribe("on_test_2", on_test_2)
	eb.clear_all()
	assert !eb.has_subscriber("on_test_2")
	assert !eb.subscriber.is_subscribed("on_test_2")

	assert p.get_string("eventbus") == "vevent"
}

fn on_test_2(sender voidptr, p eventbus.Params){}