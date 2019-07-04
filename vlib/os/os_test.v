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
    return
  }
  assert hello == read_hello

  os.rm(filename)
}
