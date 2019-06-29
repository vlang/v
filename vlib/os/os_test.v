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

