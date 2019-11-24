module main

__global fd [2]int
__global buffer [16]byte

const (
	sample_text_file1 = ""
)

fn check_read_write_pipe() {
	/*
		Checks the following system calls:
			sys_pipe
			sys_write
			sys_read
			sys_close
	*/
	println ("checking pipe read/write")
	fd[0] = -1
	fd[1] = -1

	assert fd[0] == -1
	assert fd[1] == -1

	a := sys_pipe(intptr(fd))

	assert a != -1

	assert fd[0] != -1
	assert fd[1] != -1

	test_data := "test_data"
	b := test_data.len + 1
	mut c := sys_write (fd[1], test_data.str, u64(b))

	assert c == b

	c = sys_read(fd[0], byteptr(buffer), u64(b))

	assert c == b

	assert buffer[b-1] == 0

	for i in 0..b {
		assert test_data[i] == buffer[i]
	}

	assert 0 == sys_close(fd[0])
	assert 0 == sys_close(fd[1])

	assert 0 != sys_close(-1)

	println ("pipe read/write passed")
}

fn check_read_file() {
	/*
		Checks the following system calls:
			sys_read
			sys_write
			sys_close
	*/
	test_file := "sample_text1.txt"
	sample_text := "Do not change this text.\n"
	println ("checking read file")
	fd := sys_open(test_file.str, int(fcntl.o_rdonly), 0)
	assert fd > 0
	n := sample_text.len
	c := sys_read(fd, buffer, u64(n*2))
	assert c == n
	for i in 0..n {
		assert sample_text[i] == buffer[i]
	}
	assert 0 == sys_close(fd)
	println("read file passed")
}

fn main() {
	check_read_write_pipe()
	check_read_file()
	sys_exit(0)
}

