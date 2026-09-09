// vtest build: linux
// vtest retry: 2
import os
import time

fn test_input_rune_iterator_with_unicode_input() {
	mut p := os.new_process(@VEXE)
	p.set_args(['-e', 'for i, r in input_rune_iterator() { println("> i: \${i:04} | r: `\${r}`") }'])
	p.set_redirect_stdio()
	p.run()
	spawn fn [mut p] () {
		time.sleep(10 * time.millisecond)
		dump(p.pid)
		p.stdin_write('Проба Abc 你好 🌍 123')
		time.sleep(10 * time.millisecond)
		p.stdin_write('\0x00') // 0 should break the input stream
		time.sleep(10 * time.millisecond)
		eprintln('>>> done')
	}()
	mut olines := []string{}
	for p.is_alive() {
		if oline := p.pipe_read(.stdout) {
			olines << oline
		}
		time.sleep(1 * time.millisecond)
	}
	p.close()
	p.wait()
	assert p.code == 0
	eprintln('done')
	solines := olines.join('').trim_space().replace('\r', '')
	eprintln('solines.len: ${solines.len} | solines: ${solines}')
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
