import os

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

        file_write := os.create(os.realpath(file_name)) or {
                eprintln('failed to create file $file_name')
                return
        }

        // We use the standard write_bytes function to write the payload and
        // compare the length of the array with the file size (have to match).
        file_write.write_bytes(payload.data, 5)

        file_write.close()

        assert payload.len == os.file_size(file_name)

        file_read := os.open(os.realpath(file_name)) or {
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
  os.mkdir(folder)

  folder_contents := os.ls(folder) or { panic(err) }
  assert folder_contents.len == 0

  os.rmdir(folder)

  folder_exists := os.dir_exists(folder)

  assert folder_exists == false
}

fn test_dir() {
	$if windows {
		assert os.dir('C:\\a\\b\\c') == 'C:\\a\\b' 
 
	} $else { 
		assert os.dir('/var/tmp/foo') == '/var/tmp' 
	} 
} 

fn walk_callback(file string) {
    if file == '.' || file == '..' {
        return
    }
    assert file == 'test_walk'+os.path_separator+'test1'
}

fn test_walk() {
    folder := 'test_walk'
    os.mkdir(folder)
    
    file1 := folder+os.path_separator+'test1'
    
    os.write_file(file1,'test-1')
    
    os.walk(folder, walk_callback)
	
	os.rm(file1)
	os.rmdir(folder)
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
