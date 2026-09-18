module tests

struct ReceiverRequest {}

struct ReceiverResponse {}

fn (items []ReceiverRequest) receiver_kind() string {
	return 'request:${items.len}'
}

fn (items []ReceiverResponse) receiver_kind() string {
	return 'response:${items.len}'
}

fn request_receiver_kind() string {
	batch := [ReceiverRequest{}]
	return batch.receiver_kind()
}

fn response_receiver_kind() string {
	batch := [ReceiverResponse{}, ReceiverResponse{}]
	return batch.receiver_kind()
}

fn test_same_named_array_receiver_uses_exact_checker_type() {
	assert request_receiver_kind() == 'request:1'
	assert response_receiver_kind() == 'response:2'
}
