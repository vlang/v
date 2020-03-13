import os
import term

fn test_all() {
	files := os.ls('.') or {
		panic(err)
	}
	for file in files {
		if !file.ends_with('.vv') {
			continue
		}
		print(file + ' ')
		program := file.replace('.vv', '.v')
		os.cp(file, program) or {
			panic(err)
		}
		os.rm('exe')
		x := os.exec('v -o exe -cflags "-w" -cg -backend experimental $program') or {
			panic(err)
		}
		println(x.output.limit(30))
		os.rm(program)
		res := os.exec('./exe') or {
			println('nope')
			panic(err)
		}
		// println('============')
		// println(res.output)
		// println('============')
		mut expected := os.read_file(program.replace('.v', '') + '.out') or {
			panic(err)
		}
		expected = expected.trim_space()
		found := res.output.trim_space()
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			println('expected:')
			println(expected)
			println('\nfound:')
			println(found)
			println('============')
		}
		else {
			println(term.green('OK'))
		}
	}
}
