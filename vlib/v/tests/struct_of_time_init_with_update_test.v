import time

fn test_struct_of_time_init_with_update() {
	utc := time.utc()

	t := time.Time{
		...utc
	}
	println(utc)
	println(t)
	assert t == utc
}
