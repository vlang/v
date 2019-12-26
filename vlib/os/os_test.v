import os

fn test_aaa_setup(){
	cleanup_leftovers() assert true
}

fn test_setenv() {
  os.setenv('foo', 'bar', true)
  assert os.getenv('foo') == 'bar'

  // `setenv` should not set if `overwrite` is false
  os.setenv('foo', 'bar2', false)
  assert os.getenv('foo') == 'bar'

  // `setenv` should overwrite if `overwrite` is true
  os.setenv('foo', 'bar2', true)
  assert os.getenv('foo') == 'bar2'
}

fn test_unsetenv() {
  os.setenv('foo', 'bar', true)
  os.unsetenv('foo')
  assert os.getenv('foo') == ''
}

fn test_write_and_read_string_to_file() {
  filename := './test1.txt'
  hello := 'hello world!'
  os.write_file(filename, hello)
  assert hello.len == os.file_size(filename)

  read_hello := os.read_file(filename) or {
    panic('error reading file $filename')
  }
  assert hello == read_hello

  os.rm(filename)
}

// test_write_and_read_bytes checks for regressions made in the functions
// read_bytes, read_bytes_at and write_bytes.
fn test_write_and_read_bytes() {
        file_name :=  './byte_reader_writer.tst'
        payload   :=  [`I`, `D`, `D`, `Q`, `D`]

        mut file_write := os.create(os.realpath(file_name)) or {
                eprintln('failed to create file $file_name')
                return
        }

        // We use the standard write_bytes function to write the payload and
        // compare the length of the array with the file size (have to match).
        file_write.write_bytes(payload.data, 5)

        file_write.close()

        assert payload.len == os.file_size(file_name)

        mut file_read := os.open(os.realpath(file_name)) or {
          eprintln('failed to open file $file_name')
          return
        }

        // We only need to test read_bytes because this function calls
        // read_bytes_at with second parameter zeroed (size, 0).
        red_bytes := file_read.read_bytes(5)

        file_read.close()

        assert red_bytes.str() == payload.str()

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
    assert file == 'test_walk'+os.path_separator+'test1'
}

fn test_walk() {
    folder := 'test_walk'
    os.mkdir(folder) or { panic(err) }

    file1 := folder+os.path_separator+'test1'

    os.write_file(file1,'test-1')

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

fn test_cp_r() {
  //fileX -> dir/fileX
  // NB: clean up of the files happens inside the cleanup_leftovers function
  os.write_file('ex1.txt', 'wow!')
  os.mkdir('ex') or { panic(err) }
  os.cp_r('ex1.txt', 'ex', false) or { panic(err) }
  old := os.read_file('ex1.txt') or { panic(err) }
  new := os.read_file('ex/ex1.txt') or { panic(err) }
  assert old == new
  os.mkdir('ex/ex2') or { panic(err) }
  os.write_file('ex2.txt', 'great!')
  os.cp_r('ex2.txt', 'ex/ex2', false) or { panic(err) }
  old2 := os.read_file('ex2.txt') or { panic(err) }
  new2 := os.read_file('ex/ex2/ex2.txt') or { panic(err) }
  assert old2 == new2
  //recurring on dir -> local dir
  os.cp_r('ex', './', true) or { panic(err) }
}

fn test_tmpdir(){
	t := os.tmpdir()
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


fn test_make_symlink_check_is_link_and_remove_symlink() {
   $if windows {
       // TODO
       assert true
       return
   }

   folder  := 'tfolder'
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

//fn test_fork() {
//  pid := os.fork()
//  if pid == 0 {
//    println('Child')
//  }
//  else {
//    println('Parent')
//  }
//}

//fn test_wait() {
//  pid := os.fork()
//  if pid == 0 {
//    println('Child')
//    exit(0)
//  }
//  else {
//    cpid := os.wait()
//    println('Parent')
//    println(cpid)
//  }
//}

fn test_zzz_cleanup(){
	cleanup_leftovers() assert true
}

// this function is called by both test_aaa_setup & test_zzz_cleanup
// it ensures that os tests do not polute the filesystem with leftover
// files so that they can be run several times in a row.
fn cleanup_leftovers(){
	// possible leftovers from test_cp
	os.rm('cp_example.txt')
	os.rm('cp_new_example.txt')
	
	// possible leftovers from test_cp_r
	os.rm('ex/ex2/ex2.txt')
	os.rmdir('ex/ex2')
	os.rm('ex/ex1.txt')
	os.rmdir('ex')
	os.rm('ex2/ex2.txt')
	os.rmdir('ex2')
	os.rm('ex1.txt')
	os.rm('ex2.txt')
}
