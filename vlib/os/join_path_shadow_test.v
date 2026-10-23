module os

fn test_join_path_local_function_value_shadows_os_function() {
	join_path := fn (left string, right string) string {
		return left + ':' + right
	}
	assert join_path('left', 'right') == 'left:right'
}
