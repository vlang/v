module main

__global fd [2]int
__global buffer [16]byte

const (
	sample_text_file1 = ""
)

fn fork_test (test_fn fn(), name string) {
	//print ("checking")
	// a := "$name"
	println (name)
	child := sys_fork()
	if child == 0 {
		test_fn()
		sys_exit(0)
	}
//	pid := sys_wait(0)
//	assert
}

fn check_read_write_pipe() {
	//	Checks the following system calls:
	//		sys_pipe
	//		sys_write
	//		sys_read
	//		sys_close
	//
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
			sys_open
	*/
	test_file := "sample_text1.txt"
	sample_text := "Do not change this text.\n"
	println ("checking read file")
	fd, ec := sys_open(test_file.str, .o_rdonly, 0)
	assert fd > 0
	assert ec == .enoerror
	n := sample_text.len
	c := sys_read(fd, buffer, u64(n*2))
	assert c == n
	for i in 0..n {
		assert sample_text[i] == buffer[i]
	}
	assert 0 == sys_close(fd)

	println("read file passed")
}

fn check_open_file_fail() {
	println ("checking 'open file fail'")
	fd1, ec1 := sys_open("./nofilehere".str, .o_rdonly, 0)
	assert fd1 == -1
	assert ec1 == .enoent
	println ("'open file fail' check passed")
}

/*
fn check_print() {
	println ("checking print and println")

	a := sys_pipe(intptr(fd))
	assert a != -1
	assert fd[0] != -1
	assert fd[1] != -1

	//sys_dup2
	println ("print and println passed")
}
*/

fn main() {
	check_read_write_pipe()
	check_read_file()
	// check_print()
	check_open_file_fail()
	sys_exit(0)
}

