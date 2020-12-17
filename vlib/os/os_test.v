import os

const (
	// tfolder will contain all the temporary files/subfolders made by
	// the different tests. It would be removed in testsuite_end(), so
	// individual os tests do not need to clean up after themselves.
	tfolder = os.join_path(os.temp_dir(), 'v', 'tests', 'os_test')
)

fn testsuite_begin() {
	eprintln('testsuite_begin, tfolder = $tfolder')
	os.rmdir_all(tfolder)
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)
	os.chdir(tfolder)
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup)
	os.rmdir_all(tfolder)
	assert !os.is_dir(tfolder)
	// eprintln('testsuite_end  , tfolder = $tfolder removed.')
}

fn test_open_file() {
	filename := './test1.txt'
	hello := 'hello world!'
	os.open_file(filename, 'r+', 0o666) or {
		assert err == 'No such file or directory'
		os.File{}
	}
	mut file := os.open_file(filename, 'w+', 0o666) or { panic(err) }
	file.write_str(hello)
	file.close()
	assert hello.len == os.file_size(filename)
	read_hello := os.read_file(filename) or { panic('error reading file $filename') }
	assert hello == read_hello
	os.rm(filename)
}

fn test_open_file_binary() {
	filename := './test1.dat'
	hello := 'hello \n world!'
	os.open_file(filename, 'r+', 0o666) or {
		assert err == 'No such file or directory'
		os.File{}
	}
	mut file := os.open_file(filename, 'wb+', 0o666) or { panic(err) }
	bytes := hello.bytes()
	file.write_bytes(bytes.data, bytes.len)
	file.close()
	assert hello.len == os.file_size(filename)
	read_hello := os.read_bytes(filename) or { panic('error reading file $filename') }
	assert bytes == read_hello
	os.rm(filename)
}

// fn test_file_get_line() {
// 	filename := './fgetline.txt'
// 	os.write_file(filename, 'line 1\nline 2')
// 	mut f := os.open_file(filename, 'r', 0) or {
// 		assert false
// 		return
// 	}
// 	line1 := f.get_line() or {
// 		''
// 	}
// 	line2 := f.get_line() or {
// 		''
// 	}
// 	f.close()
// 	//
// 	eprintln('line1: $line1 $line1.bytes()')
// 	eprintln('line2: $line2 $line2.bytes()')
// 	assert line1 == 'line 1\n'
// 	assert line2 == 'line 2'
// }
fn test_create_file() {
	filename := './test1.txt'
	hello := 'hello world!'
	mut f := os.create(filename) or { panic(err) }
	f.write_str(hello)
	f.close()
	assert hello.len == os.file_size(filename)
	os.rm(filename)
}

fn test_is_file() {
	// Setup
	work_dir := os.join_path(os.getwd(), 'is_file_test')
	os.mkdir_all(work_dir)
	tfile := os.join_path(work_dir, 'tmp_file')
	// Test things that shouldn't be a file
	assert os.is_file(work_dir) == false
	assert os.is_file('non-existent_file.tmp') == false
	// Test file
	tfile_content := 'temporary file'
	os.write_file(tfile, tfile_content)
	assert os.is_file(tfile)
	// Test dir symlinks
	$if windows {
		assert true
	} $else {
		dsymlink := os.join_path(work_dir, 'dir_symlink')
		os.system('ln -s $work_dir $dsymlink')
		assert os.is_file(dsymlink) == false
	}
	// Test file symlinks
	$if windows {
		assert true
	} $else {
		fsymlink := os.join_path(work_dir, 'file_symlink')
		os.system('ln -s $tfile $fsymlink')
		assert os.is_file(fsymlink)
	}
}

fn test_write_and_read_string_to_file() {
	filename := './test1.txt'
	hello := 'hello world!'
	os.write_file(filename, hello)
	assert hello.len == os.file_size(filename)
	read_hello := os.read_file(filename) or { panic('error reading file $filename') }
	assert hello == read_hello
	os.rm(filename)
}

// test_write_and_read_bytes checks for regressions made in the functions
// read_bytes, read_bytes_at and write_bytes.
fn test_write_and_read_bytes() {
	file_name := './byte_reader_writer.tst'
	payload := [byte(`I`), `D`, `D`, `Q`, `D`]
	mut file_write := os.create(os.real_path(file_name)) or {
		eprintln('failed to create file $file_name')
		return
	}
	// We use the standard write_bytes function to write the payload and
	// compare the length of the array with the file size (have to match).
	file_write.write_bytes(payload.data, 5)
	file_write.close()
	assert payload.len == os.file_size(file_name)
	mut file_read := os.open(os.real_path(file_name)) or {
		eprintln('failed to open file $file_name')
		return
	}
	// We only need to test read_bytes because this function calls
	// read_bytes_at with second parameter zeroed (size, 0).
	rbytes := file_read.read_bytes(5)
	// eprintln('rbytes: $rbytes')
	// eprintln('payload: $payload')
	assert rbytes == payload
	// check that trying to read data from EOF doesn't error and returns 0
	mut a := []byte{len: 5}
	nread := file_read.read_bytes_into(5, mut a) or {
		eprintln(err)
		int(-1)
	}
	assert nread == 0
	file_read.close()
	// We finally delete the test file.
	os.rm(file_name)
}

fn test_create_and_delete_folder() {
	folder := './test1'
	os.mkdir(folder) or { panic(err) }
	assert os.is_dir(folder)
	folder_contents := os.ls(folder) or { panic(err) }
	assert folder_contents.len == 0
	os.rmdir(folder)
	folder_exists := os.is_dir(folder)
	assert folder_exists == false
}

fn walk_callback(file string) {
	if file == '.' || file == '..' {
		return
	}
	assert file == 'test_walk' + os.path_separator + 'test1'
}

fn test_walk() {
	folder := 'test_walk'
	os.mkdir(folder) or { panic(err) }
	file1 := folder + os.path_separator + 'test1'
	os.write_file(file1, 'test-1')
	os.walk(folder, walk_callback)
	os.rm(file1)
	os.rmdir(folder)
}

fn test_cp() {
	old_file_name := 'cp_example.txt'
	new_file_name := 'cp_new_example.txt'
	os.write_file(old_file_name, 'Test data 1 2 3, V is awesome #$%^[]!~⭐')
	os.cp(old_file_name, new_file_name) or { panic('$err: errcode: $errcode') }
	old_file := os.read_file(old_file_name) or { panic(err) }
	new_file := os.read_file(new_file_name) or { panic(err) }
	assert old_file == new_file
	os.rm(old_file_name)
	os.rm(new_file_name)
}

fn test_mv() {
	work_dir := os.join_path(os.getwd(), 'mv_test')
	os.mkdir_all(work_dir)
	// Setup test files
	tfile1 := os.join_path(work_dir, 'file')
	tfile2 := os.join_path(work_dir, 'file.test')
	tfile3 := os.join_path(work_dir, 'file.3')
	tfile_content := 'temporary file'
	os.write_file(tfile1, tfile_content)
	os.write_file(tfile2, tfile_content)
	// Setup test dirs
	tdir1 := os.join_path(work_dir, 'dir')
	tdir2 := os.join_path(work_dir, 'dir2')
	tdir3 := os.join_path(work_dir, 'dir3')
	mkdir(tdir1)
	mkdir(tdir2)
	// Move file with no extension to dir
	os.mv(tfile1, tdir1)
	mut expected := os.join_path(tdir1, 'file')
	assert os.exists(expected) && !is_dir(expected) == true
	// Move dir with contents to other dir
	os.mv(tdir1, tdir2)
	expected = os.join_path(tdir2, 'dir')
	assert os.exists(expected) && is_dir(expected) == true
	expected = os.join_path(tdir2, 'dir', 'file')
	assert os.exists(expected) && !is_dir(expected) == true
	// Move dir with contents to other dir (by renaming)
	os.mv(os.join_path(tdir2, 'dir'), tdir3)
	expected = tdir3
	assert os.exists(expected) && is_dir(expected) == true
	assert os.is_dir_empty(tdir2) == true
	// Move file with extension to dir
	os.mv(tfile2, tdir2)
	expected = os.join_path(tdir2, 'file.test')
	assert os.exists(expected) && !is_dir(expected) == true
	// Move file to dir (by renaming)
	os.mv(os.join_path(tdir2, 'file.test'), tfile3)
	expected = tfile3
	assert os.exists(expected) && !is_dir(expected) == true
}

fn test_cp_r() {
	// fileX -> dir/fileX
	// NB: clean up of the files happens inside the cleanup_leftovers function
	os.write_file('ex1.txt', 'wow!')
	os.mkdir('ex') or { panic(err) }
	os.cp_all('ex1.txt', 'ex', false) or { panic(err) }
	old := os.read_file('ex1.txt') or { panic(err) }
	new := os.read_file('ex/ex1.txt') or { panic(err) }
	assert old == new
	os.mkdir('ex/ex2') or { panic(err) }
	os.write_file('ex2.txt', 'great!')
	os.cp_all('ex2.txt', 'ex/ex2', false) or { panic(err) }
	old2 := os.read_file('ex2.txt') or { panic(err) }
	new2 := os.read_file('ex/ex2/ex2.txt') or { panic(err) }
	assert old2 == new2
	// recurring on dir -> local dir
	os.cp_all('ex', './', true) or { panic(err) }
}

fn test_tmpdir() {
	t := os.temp_dir()
	assert t.len > 0
	assert os.is_dir(t)
	tfile := t + os.path_separator + 'tmpfile.txt'
	os.rm(tfile) // just in case
	tfile_content := 'this is a temporary file'
	os.write_file(tfile, tfile_content)
	tfile_content_read := os.read_file(tfile) or { panic(err) }
	assert tfile_content_read == tfile_content
	os.rm(tfile)
}

fn test_is_writable_folder() {
	tmp := os.temp_dir()
	f := os.is_writable_folder(tmp) or {
		eprintln('err: $err')
		false
	}
	assert f
}

fn test_make_symlink_check_is_link_and_remove_symlink() {
	$if windows {
		// TODO
		assert true
		return
	}
	folder := 'tfolder'
	symlink := 'tsymlink'
	os.rm(symlink)
	os.rm(folder)
	os.mkdir(folder) or { panic(err) }
	folder_contents := os.ls(folder) or { panic(err) }
	assert folder_contents.len == 0
	os.system('ln -s $folder $symlink')
	assert os.is_link(symlink) == true
	os.rm(symlink)
	os.rm(folder)
	folder_exists := os.is_dir(folder)
	assert folder_exists == false
	symlink_exists := os.is_link(symlink)
	assert symlink_exists == false
}

// fn test_fork() {
// pid := os.fork()
// if pid == 0 {
// println('Child')
// }
// else {
// println('Parent')
// }
// }
// fn test_wait() {
// pid := os.fork()
// if pid == 0 {
// println('Child')
// exit(0)
// }
// else {
// cpid := os.wait()
// println('Parent')
// println(cpid)
// }
// }
fn test_symlink() {
	$if windows {
		return
	}
	os.mkdir('symlink') or { panic(err) }
	os.symlink('symlink', 'symlink2') or { panic(err) }
	assert os.exists('symlink2')
	// cleanup
	os.rm('symlink')
	os.rm('symlink2')
}

fn test_is_executable_writable_readable() {
	file_name := 'rwxfile.exe'
	mut f := os.create(file_name) or {
		eprintln('failed to create file $file_name')
		return
	}
	f.close()
	$if !windows {
		os.chmod(file_name, 0o600) // mark as readable && writable, but NOT executable
		assert os.is_writable(file_name)
		assert os.is_readable(file_name)
		assert !os.is_executable(file_name)
		os.chmod(file_name, 0o700) // mark as executable too
		assert os.is_executable(file_name)
	} $else {
		assert os.is_writable(file_name)
		assert os.is_readable(file_name)
		assert os.is_executable(file_name)
	}
	// We finally delete the test file.
	os.rm(file_name)
}

fn test_ext() {
	assert os.file_ext('file.v') == '.v'
	assert os.file_ext('file') == ''
}

fn test_is_abs() {
	assert os.is_abs_path('/home/user') == true
	assert os.is_abs_path('v/vlib') == false
	$if windows {
		assert os.is_abs_path('C:\\Windows\\') == true
	}
}

fn test_join() {
	$if windows {
		assert os.join_path('v', 'vlib', 'os') == 'v\\vlib\\os'
	} $else {
		assert os.join_path('v', 'vlib', 'os') == 'v/vlib/os'
	}
}

fn test_dir() {
	$if windows {
		assert os.dir('C:\\a\\b\\c') == 'C:\\a\\b'
		assert os.dir('C:\\a\\b\\') == 'C:\\a\\b'
	} $else {
		assert os.dir('/var/tmp/foo') == '/var/tmp'
		assert os.dir('/var/tmp/') == '/var/tmp'
	}
	assert os.dir('os') == '.'
}

fn test_base() {
	$if windows {
		assert os.base('v\\vlib\\os') == 'os'
		assert os.base('v\\vlib\\os\\') == 'os'
	} $else {
		assert os.base('v/vlib/os') == 'os'
		assert os.base('v/vlib/os/') == 'os'
	}
	assert os.base('filename') == 'filename'
}

fn test_uname() {
	u := os.uname()
	assert u.sysname.len > 0
	assert u.nodename.len > 0
	assert u.release.len > 0
	assert u.version.len > 0
	assert u.machine.len > 0
}

// tests for write_file_array and read_file_array<T>:
const (
	maxn = 3
)

struct IntPoint {
	x int
	y int
}

fn test_write_file_array_bytes() {
	fpath := './abytes.bin'
	mut arr := []byte{len: maxn}
	for i in 0 .. maxn {
		arr[i] = 65 + byte(i)
	}
	os.write_file_array(fpath, arr)
	rarr := os.read_bytes(fpath) or { panic(err) }
	assert arr == rarr
	// eprintln(arr.str())
	// eprintln(rarr.str())
}

fn test_write_file_array_structs() {
	fpath := './astructs.bin'
	mut arr := []IntPoint{len: maxn}
	for i in 0 .. maxn {
		arr[i] = IntPoint{65 + i, 65 + i + 10}
	}
	os.write_file_array(fpath, arr)
	rarr := os.read_file_array<IntPoint>(fpath)
	assert rarr == arr
	assert rarr.len == maxn
	// eprintln( rarr.str().replace('\n', ' ').replace('},', '},\n'))
}

fn test_stdout_capture() {
	/*
	mut cmd := os.Command{
	path:'cat'
	redirect_stdout: true
}
cmd.start()
for !cmd.eof {
	line := cmd.read_line()
	println('line="$line"')
}
cmd.close()
	*/
}
