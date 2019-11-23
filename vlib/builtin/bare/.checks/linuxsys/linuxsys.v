module main

__global fd [2]int
__global buffer [16]byte

fn check_read_write_pipe () {
	println ("checking pipe read/write")
	fd[0] = -1
	fd[1] = -1

	assert fd[0] == -1
	assert fd[1] == -1

	a := pipe(intptr(fd))

	assert a != -1

	assert fd[0] != -1
	assert fd[1] != -1

	test_data := "test_data"
	b := test_data.len + 1
	mut c := write (fd[1], test_data.str, u64(b))

	assert c == b

	c = read(fd[0], byteptr(buffer), u64(b))

	assert c == b

	assert buffer[b-1] == 0

	for i in 0..b {
		assert test_data[i] == buffer[i]
	}

	assert -1 != close(fd[0])
	assert -1 != close(fd[1])

	println ("pipe read/write passed")
}

fn main () {
	check_read_write_pipe ()
	exit(0)
}

