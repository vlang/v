module main

__global fd [2]int
__global buffer [128]byte

const (
	sample_text_file1 = ""
)

fn check_fork_minimal () {
	child := sys_fork()
	ec := 100
	if child == 0 {
		println("child")
		sys_exit(ec)
	}
	siginfo := [
		int(0), 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0]

	e := sys_waitid(.p_pid, child, intptr(siginfo.data) , .wexited, 0)

	assert e == .enoerror
	//println(i64_tos(buffer,80,siginfo[sig_index.si_code],16))
	assert siginfo[sig_index.si_code] == int(wi_si_code.cld_exited)
	assert siginfo[sig_index.si_pid] == child
	assert siginfo[sig_index.si_status] == ec
	assert siginfo[sig_index.si_signo] == int(signo.sigchld)
	assert siginfo[sig_index.si_uid] == sys_getuid()

	println("fork minimal check passed")
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

	assert a == .enoerror

	assert fd[0] != -1
	assert fd[1] != -1

	test_data := "test_data"
	b := test_data.len + 1
	c1, e1 := sys_write (fd[1], test_data.str, u64(b))

	assert e1 == .enoerror
	assert c1 == b

	c2, e2 := sys_read(fd[0], byteptr(buffer), u64(b))

	assert e2 == .enoerror
	assert c2 == b

	assert buffer[b-1] == 0

	for i in 0..b {
		assert test_data[i] == buffer[i]
	}

	assert sys_close(fd[0]) == .enoerror
	assert sys_close(fd[1]) == .enoerror

	assert sys_close(-1) == .ebadf

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
	c, e := sys_read(fd, buffer, u64(n*2))
	assert e == .enoerror
	assert c == n
	for i in 0..n {
		assert sample_text[i] == buffer[i]
	}
	assert sys_close(fd) == .enoerror

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

fn check_munmap_fail() {
	println ("checking 'munmap fail'")

	ec := sys_munmap(-16384,8192)
	assert ec == .einval
	//es := i64_tos(buffer2,80,int(ec),16)
	//println(es)

	println ("'munmap fail' check passed")
}

fn check_mmap_one_page() {
	println ("checking check_mmap_one_page")

	mp := int(mm_prot.prot_read) | int(mm_prot.prot_write)
	mf := int(map_flags.map_private) | int(map_flags.map_anonymous)
	mut a, e := sys_mmap(0, u64(linux_mem.page_size), mm_prot(mp), map_flags(mf), -1, 0)

	assert e == .enoerror
	assert a != byteptr(-1)

	for i in 0..int(linux_mem.page_size) {
		b := i & 0xFF
		a[i] = b
		assert a[i] == b
	}

	ec := sys_munmap(a, u64(linux_mem.page_size))
	assert ec == .enoerror

	println ("check_mmap_one_page passed")
}

fn check_mm_pages() {
	println ("checking check_mm_pages")
	for i in 0 .. int(linux_mem.page_size)-4 {
		assert u32(1) == mm_pages(u64(i))
	}
	for i in int(linux_mem.page_size)-3 .. (int(linux_mem.page_size)*2)-4 {
		assert u32(2) == mm_pages(u64(i))
	}
	for i in (int(linux_mem.page_size)*2)-3 .. (int(linux_mem.page_size)*3)-4 {
		assert u32(3) == mm_pages(u64(i))
	}
	println ("check_mm_pages passed")
}

//pub fn mm_alloc(size u64) (voidptr, errno)

fn check_mm_alloc() {
	println ("checking mm_alloc")

	for i in 1 .. 2000 {
		size := u64(i*1000)
		pages := mm_pages(size)
		mut a, e := mm_alloc(size)

		//ads := i64_tos(buffer,80,i64(a),16)
		//println(ads)
		//es := i64_tos(buffer,80,i64(e),16)
		//println(es)

		assert e == .enoerror
		ap := intptr(a-4)
		assert *ap == int(pages)
		assert e == .enoerror
		assert !isnil(a)

		if (i%111) == 0 {
			for j in 0 .. int(size) {
				b := j & 0xFF
				a[j] = b
				assert b == int(a[j])
			}
		}

		mfa := mm_free(a)

		//mfas := i64_tos(buffer,80,i64(mfa),16)
		//println(mfas)

		assert mfa == .enoerror
	}
	println ("mm_alloc passed")
}

fn check_int_array_ro() {
	println ("trying check_int_array_ro")
	a := [100,110,120,130]
	assert a.len == 4
	assert a[0] == 100
	assert a[1] == 110
	assert a[2] == 120
	assert a[3] == 130
	println ("check_int_array_ro passed")
}

fn check_int_array_rw() {
	println ("trying check_int_array_rw")
	mut a := [-10,-11,-12,-13]
	assert a.len == 4
	assert a[0] == -10
	assert a[1] == -11
	assert a[2] == -12
	assert a[3] == -13
	for i in 0..a.len {
		b := -a[i] * 10
		a[i] = b
		assert a[i] == b
	}
	assert a[3] == 130

	println ("check_int_array_rw passed")
}

fn check_int64_array_ro() {
	println ("trying check_int64_array_ro")
	a := [i64(1000),1100,1200,1300,1400]
	assert a.len == 5
	assert a[0] == 1000
	assert a[1] == 1100
	assert a[2] == 1200
	assert a[3] == 1300
	assert a[4] == 1400
	println ("check_int64_array_ro passed")
}

fn check_voidptr_array_ro() {
	println ("trying check_voidptr_array_ro")
	a := [
		voidptr(10000),
		voidptr(11000),
		voidptr(12000),
		voidptr(13000),
		voidptr(14000),
		voidptr(15000)
	]
	assert a.len == 6
	assert a[0] == voidptr(10000)
	assert a[1] == voidptr(11000)
	assert a[2] == voidptr(12000)
	assert a[3] == voidptr(13000)
	assert a[4] == voidptr(14000)
	assert a[5] == voidptr(15000)
	println ("check_voidptr_array_ro passed")
}

fn check_voidptr_array_rw() {
	println ("trying check_voidptr_array_rw")
	mut a := [
		voidptr(-1),
		voidptr(-1),
		voidptr(-1),
		voidptr(-1),
		voidptr(-1),
		voidptr(-1)
	]
	assert a.len == 6

	assert a[0] == voidptr(-1)
	assert a[1] == voidptr(-1)
	assert a[2] == voidptr(-1)
	assert a[3] == voidptr(-1)
	assert a[4] == voidptr(-1)
	assert a[5] == voidptr(-1)

	a[0] = voidptr(100000)
	assert a[0] == voidptr(100000)

	a[1] = voidptr(110000)
	assert a[1] == voidptr(110000)

	a[2] = voidptr(120000)
	assert a[2] == voidptr(120000)

	a[3] = voidptr(130000)
	assert a[3] == voidptr(130000)

	a[4] = voidptr(140000)
	assert a[4] == voidptr(140000)

	a[5] = voidptr(150000)
	assert a[5] == voidptr(150000)
	println ("check_voidptr_array_rw passed")
}


fn main() {
	check_read_write_pipe()
	check_read_file()
	// check_print()
	check_open_file_fail()
	check_munmap_fail()
	check_mmap_one_page()
	check_mm_pages()
	check_mm_alloc()
	check_int_array_ro()
	check_int_array_rw()
	check_int64_array_ro()
	check_voidptr_array_ro()
	check_voidptr_array_rw()
	check_fork_minimal()
	sys_exit(0)
}
