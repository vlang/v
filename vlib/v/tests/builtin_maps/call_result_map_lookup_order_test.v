// vtest vflags: -enable-globals

__global (
	call_map_phase     int
	call_map_receivers int
	call_map_keys      int
)

struct CallMapHolder {
	items map[string]int
}

fn call_map_holder() &CallMapHolder {
	call_map_receivers++
	call_map_phase = 2
	return &CallMapHolder{
		items: {
			'2': 21
		}
	}
}

fn call_map_value() map[string]int {
	call_map_receivers++
	call_map_phase = 3
	return {
		'3': 34
	}
}

fn call_map_key() string {
	call_map_keys++
	return call_map_phase.str()
}

fn test_call_result_map_field_evaluates_receiver_before_key_once() {
	call_map_phase = 0
	call_map_receivers = 0
	call_map_keys = 0
	value := call_map_holder().items[call_map_key()]
	assert value == 21
	assert call_map_phase == 2
	assert call_map_receivers == 1
	assert call_map_keys == 1
}

fn test_call_result_map_value_is_addressable_and_evaluated_once() {
	call_map_phase = 0
	call_map_receivers = 0
	call_map_keys = 0
	value := call_map_value()[call_map_key()]
	assert value == 34
	assert call_map_phase == 3
	assert call_map_receivers == 1
	assert call_map_keys == 1
	assert call_map_value()['missing'] == 0
	assert call_map_receivers == 2
}
