import os

const vexe = @VEXE
const expected = '> i: 0000 | r: `П`
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
> i: 0017 | r: `3`
> i: 0018 | r: `\n`'

fn test_input_rune_iterator() {
	os.chdir(@VROOT)!
	cmd := '${os.quoted_path(vexe)} -e \'for i, r in input_rune_iterator() { println("> i: \${i:04} | r: `\${r}`") }\' < ${os.quoted_path('vlib/v/tests/runes.txt')}'
	eprintln('cmd: ${cmd}')
	res := os.execute(cmd)
	dump(res)
	assert res.exit_code == 0
	found := res.output.trim_space().replace('\r', '')
	assert found == expected
}
