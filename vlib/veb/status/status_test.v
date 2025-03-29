module status

struct TestStatus {
	Status
}

fn test_status() {
	mut status := TestStatus{}
	status = TestStatus{new_status('ok', .ok)}
	assert status.is_ok() == true
	assert status.is_error() == false
	assert status.is_valid() == true
	assert status.is_success() == true

	status = TestStatus{new_status('not found', .not_found)}
	assert status.is_ok() == false
	assert status.is_error() == true
	assert status.is_valid() == true
	assert status.is_success() == false

	status = TestStatus{new_status('error', .internal_server_error)}
	assert status.is_ok() == false
	assert status.is_error() == true
	assert status.is_valid() == true
	assert status.is_success() == false
}
