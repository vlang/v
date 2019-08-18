import os

fn test_repl() {
	test_files := os.walk_ext('.', '.repl')

  for file in test_files {
    content := os.read_file(file) or {
      assert false
      break
    }
    input := content.all_before('===output===\n').replace('$', '\\$')
    output := content.all_after('===output===\n')
    r := os.exec('echo "$input" | ./v') or {
      assert false
      break
    }
    result := r.output.replace('>>> ', '').replace('>>>', '').all_after('Use Ctrl-C or `exit` to exit\n')
    assert result == output
    if result != output {
      println(file)
      println('Got : $result')
      println('Expected : $output')
    }
  }
}