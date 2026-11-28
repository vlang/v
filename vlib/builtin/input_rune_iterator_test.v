// vtest build: linux
// vtest retry: 2
import os

const input_rune_probe_arg = '--vtest-input-rune-iterator'

fn testsuite_begin() {
	if os.args.len != 2 || os.args[1] != input_rune_probe_arg {
		return
	}
	for i, r in input_rune_iterator() {
		println('> i: ${i:04} | r: `${r}`')
	}
	exit(0)
}

fn test_input_rune_iterator_with_unicode_input() ! {
	input := os.join_path(os.vtmp_dir(), 'input_rune_iterator_${os.getpid()}.txt')
	defer {
		os.rm(input) or {}
	}
	// A suffix after NUL makes reading past the terminator visible.
	os.write_file(input, 'Проба Abc 你好 🌍 123\x00extra')!
	mut p := os.new_process(os.executable())
	p.set_args([input_rune_probe_arg])
	p.set_redirect_stdio()
	p.set_stdin_path(input)
	p.run()
	defer {
		p.close()
	}
	solines := p.stdout_slurp().trim_space().replace('\r', '')
	stderr := p.stderr_slurp()
	p.wait()
	assert p.code == 0, stderr
	assert solines.len > 100
	assert solines == '> i: 0000 | r: `П`
> i: 0001 | r: `р`
> i: 0002 | r: `о`
> i: 0003 | r: `б`
> i: 0004 | r: `а`
> i: 0005 | r: ` `
> i: 0006 | r: `A`
> i: 0007 | r: `b`
> i: 0008 | r: `c`
> i: 0009 | r: ` `
> i: 0010 | r: `你`
> i: 0011 | r: `好`
> i: 0012 | r: ` `
> i: 0013 | r: `🌍`
> i: 0014 | r: ` `
> i: 0015 | r: `1`
> i: 0016 | r: `2`
> i: 0017 | r: `3`'
}
