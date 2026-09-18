struct RvalueReceiver {
	value int
}

fn (receiver &RvalueReceiver) get() int {
	return receiver.value
}

fn new_rvalue_receiver(value int) RvalueReceiver {
	return RvalueReceiver{
		value: value
	}
}

fn test_pointer_receiver_method_can_be_called_on_a_struct_rvalue() {
	assert new_rvalue_receiver(42).get() == 42
}
