import os

fn test_every_atomics_asm_block_clobbers_memory() {
	mut blocks := 0
	mut checked := 0
	for name in ['atomics.amd64.v', 'atomics.i386.v'] {
		lines := os.read_lines(os.join_path(@DIR, name))!
		mut fn_name := ''
		mut in_asm := false
		mut has_memory := false
		for line in lines {
			if line.starts_with('pub fn ') {
				fn_name = line.all_after('pub fn ').all_before('(')
			} else if line.trim_space().starts_with('asm volatile') {
				in_asm = true
				blocks++
				has_memory = false
			} else if in_asm && line.trim_space() == 'memory' {
				has_memory = true
			} else if in_asm && line == '\t}' {
				assert has_memory, '${name}: the asm block in `${fn_name}` does not clobber memory'
				in_asm = false
				checked++
			}
		}
	}
	assert blocks > 0
	assert checked == blocks
}
