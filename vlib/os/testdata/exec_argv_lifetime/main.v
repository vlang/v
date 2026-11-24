import os

fn require(ok bool, message string) {
	if !ok {
		eprintln(message)
		exit(71)
	}
}

fn main() {
	if os.args.len > 1 {
		match os.args[1] {
			'echo' {
				println(os.args.len - 2)
				for arg in os.args[2..] { println('${arg.len}:${arg}') }
				return
			}
			'fail' {
				println('child failure output')
				exit(23)
			}
			'stderr' {
				eprintln('child stderr')
				return
			}
			else {}
		}
	}
	marker := os.args[0] + '.shell-injection'
	executable := os.executable()
	samples := ['', 'two words', 'λ漢字😀', '"double quotes"', "'single quotes'", 'trailing\\',
		'line one\nline two', '\tindented', '; touch ${marker}', '$(touch ${marker})',
		'`touch ${marker}`', '& echo injected', '| echo injected', '> ${marker}',
		'$PATH %PATH% * ? [abc] {a,b} ! # ~']
	mut expected := '${samples.len}\n'
	for sample in samples { expected += '${sample.len}:${sample}\n' }
	arguments := [executable, 'echo', ...samples]
	for _ in 0 .. 4 {
		result := os.exec(arguments)
		require(result.exit_code == 0, 'argument child failed: ${result.output}')
		require(result.output == expected, 'argument bytes changed: ${result.output}')
	}
	for i in 0 .. samples.len {
		require(arguments[i + 2] == samples[i], 'caller argument changed')
	}
	require(!os.exists(marker), 'argument was interpreted as shell syntax')
	plain := os.exec(['true'])
	require(plain.exit_code == 0 && plain.output == '', 'PATH lookup failed: ${plain.output}')
	failed := os.exec([executable, 'fail'])
	require(failed.exit_code == 23 && failed.output == 'child failure output\n', 'child exit status/output changed')
	stderr := os.exec([executable, 'stderr'])
	require(stderr.exit_code == 0 && stderr.output == 'child stderr\n', 'child stderr capture changed')
	empty := os.exec([]string{})
	require(empty.exit_code == -1 && empty.output == 'exec requires at least one argument', 'empty command contract changed')
	missing := executable + '.missing'
	for _ in 0 .. 4 {
		result := os.exec([missing, '', 'unused argument'])
		require(result.exit_code == -1 && result.output == 'exec("${missing}") failed', 'launch failure contract changed')
	}
	println('exec argument lifetime passed')
}
