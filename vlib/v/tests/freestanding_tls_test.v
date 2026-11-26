import encoding.binary
import os

fn freestanding_tls_header(data []u8) int {
	assert data.len >= 64
	assert data[..4] == [u8(0x7f), `E`, `L`, `F`]
	assert data[4] == 2 && data[5] == 1
	start := int(binary.little_endian_u64_at(data, 32))
	stride := int(binary.little_endian_u16_at(data, 54))
	count := int(binary.little_endian_u16_at(data, 56))
	mut tls := -1
	for i in 0 .. count {
		entry := start + stride * i
		assert entry >= 0 && entry + 56 <= data.len
		typ := binary.little_endian_u32_at(data, entry)
		assert typ != 3, 'freestanding output must not have an ELF interpreter'
		if typ == 7 { tls = entry }
		if typ == 2 {
			offset := int(binary.little_endian_u64_at(data, entry + 8))
			length := int(binary.little_endian_u64_at(data, entry + 32))
			for j := 0; j + 16 <= length; j += 16 {
				assert binary.little_endian_u64_at(data, offset + j) != 1, 'freestanding output must not have DT_NEEDED libraries'
			}
		}
	}
	assert tls >= 0
	return tls
}

fn test_linux_freestanding_tls_initialization_clone_and_invalid_images() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_tls_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_tls', 'main.v')
		base := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals', '-d', 'vlibc']
		mut original := []u8{}
		mut tls := -1
		for role in ['default', 'offset', 'prod'] {
			program := os.join_path(work, role)
			mut command := base.clone()
			if role == 'prod' { command << '-prod' }
			if role == 'offset' {
				script := os.join_path(os.dir(source), 'offset.ld')
				// This fixture places TLS among readonly sections deliberately;
				// suppress the linker's unrelated default RELRO grouping.
				command << ['-ldflags', '-Wl,-T,${script} -Wl,-z,norelro']
			}
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			data := os.read_bytes(program)!
			header := freestanding_tls_header(data)
			assert binary.little_endian_u64_at(data, header + 48) >= 64
			if role == 'offset' {
				assert binary.little_endian_u64_at(data, header + 16) % 64 == 32
			}
			result := os.exec([program])
			assert result.exit_code == 0, '${role}: ${result.output}'
			assert result.output.trim_space() == 'freestanding TLS passed'
			if role == 'default' {
				original = data.clone()
				tls = header
			}
		}
		for role in ['filesz', 'alignment', 'overflow', 'mapping_failure'] {
			mut data := original.clone()
			match role {
				'filesz' {
					memsz := binary.little_endian_u64_at(data, tls + 40)
					binary.little_endian_put_u64_at(mut data, memsz + 1, tls + 32)
				}
				'alignment' { binary.little_endian_put_u64_at(mut data, 3, tls + 48) }
				'overflow' { binary.little_endian_put_u64_at(mut data, ~u64(0), tls + 40) }
				else { binary.little_endian_put_u64_at(mut data, u64(1) << 62, tls + 40) }
			}
			program := os.join_path(work, role)
			os.write_file_array(program, data)!
			os.chmod(program, 0o700)!
			result := os.exec([program])
			assert result.exit_code == 127, '${role}: ${result.output}'
			assert result.output.contains('unable to initialize freestanding TLS'), '${role}: ${result.output}'
		}
		rejected := os.exec([...base, '-cc', 'tcc', '-o', os.join_path(work, 'unsupported'), source])
		assert rejected.exit_code != 0
		assert rejected.output.contains('requires a C compiler with native TLS support'), rejected.output
	}
}

fn test_freestanding_shared_library_uses_host_threads_tls() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_tls_host_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_tls')
		library := os.join_path(work, 'libtls.so')
		compiled := os.exec([@VEXE, '-freestanding', '-gc', 'none', '-enable-globals', '-shared',
			'-o', library, os.join_path(fixtures, 'library.v')])
		assert compiled.exit_code == 0, compiled.output
		host := os.join_path(work, 'host')
		linked := os.exec(['cc', '-pthread', '-o', host, os.join_path(fixtures, 'host.c'), '-ldl'])
		assert linked.exit_code == 0, linked.output
		result := os.exec([host, library])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'host threads TLS passed'
	}
}
