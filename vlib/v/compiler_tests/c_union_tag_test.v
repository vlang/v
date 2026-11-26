import os

fn test_header_owned_c_union_tags_preserve_layout_and_native_abi() ! {
	work := os.join_path(os.vtmp_dir(), 'c_union_tags_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'c_union_tags', 'main.c.v')
	for role in ['debug', 'prod'] {
		program := os.join_path(work, role) + $if windows { '.exe' } $else { '' }
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none']
		if role == 'prod' { command << '-prod' }
		command << ['-o', program, source]
		compiled := os.exec(command)
		assert compiled.exit_code == 0, '${role}: ${compiled.output}'
		result := os.exec([program])
		assert result.exit_code == 0, '${role}: ${result.output}'
		assert result.output.trim_space() == 'C union tags preserve native layout and ABI'
	}
}

fn test_headerless_c_union_keeps_union_layout() ! {
	work := os.join_path(os.vtmp_dir(), 'c_union_tag_headerless_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	os.write_file(source, 'union C.local_reply {
mut:
 word u64
 bytes [8]u8
}
fn main() {
 assert sizeof(C.local_reply) == 8
 mut value := C.local_reply{}
 unsafe {
  value.word = u64(0xa5a5a5a5a5a5a5a5)
  for byte in value.bytes { assert byte == 0xa5 }
 }
 println("headerless C union layout passed")
}')!
	program := os.join_path(work, 'program') + $if windows { '.exe' } $else { '' }
	compiled := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-o', program, source])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([program])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'headerless C union layout passed'
}
