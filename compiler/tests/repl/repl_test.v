import os

fn test_repl() {
	test_files := os.walk_ext('.', '.repl')
	wd := os.getwd() + '/'  
  for file in test_files {
    content := os.read_file(file) or {
      assert false
      break
    }
    input_temporary_filename := 'input_temporary_filename.txt'
    input := content.all_before('===output===\n')
    output := content.all_after('===output===\n')
    os.write_file(input_temporary_filename, input)
    defer {
      os.rm(input_temporary_filename)
    }
    vexec := os.executable()
    r := os.exec('$vexec < $input_temporary_filename') or {
      assert false
      break
    }
    result := r.output.replace('>>> ', '').replace('>>>', '').replace('... ', '').all_after('Use Ctrl-C or `exit` to exit\n').replace( wd, '' )
    assert result == output
    if result != output {
      println(file)
      println('Got : $result')
      println('Expected : $output')
    }
  }
}
