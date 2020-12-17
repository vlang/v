module library

[export: 'add_1']
pub fn add_1(x int, y int) int {
	return my_private_function(x + y)
}

fn my_private_function(x int) int {
	return 1 + x
}
