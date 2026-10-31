module urllib

fn test_join_host_port() {
	assert join_host_port('localhost', '8080') == 'localhost:8080'
	assert join_host_port('::1', '80') == '[::1]:80'
	assert join_host_port('example.com', '') == 'example.com'
}
