module pref

pub enum Arch {
	_auto
	amd64
	arm64
	arm32
	rv64
	rv32
	i386
	js_node
	js_browser
	js_freestanding
	wasm32
	_max
}

pub fn get_host_arch() Arch {
	// Note: we can not use `$if arch` here, because V skips cgen for the non
	// current comptime branches by default, so there is a bootstrapping
	// problem => the __V_architecture macro is used to resolve it.
	// TODO: think about how to solve it for non C backends, perhaps we
	// need a comptime `$if native {` too, and/or a mechanism to always
	// generate all branches for specific functions?
	if C.__V_architecture <= int(Arch._auto) || C.__V_architecture >= int(Arch._max) {
		return Arch.amd64
	}
	return unsafe { Arch(C.__V_architecture) }
}

pub fn arch_from_string(arch_str string) !Arch {
	match arch_str {
		'amd64', 'x86_64', 'x64', 'x86' {
			return .amd64
		}
		'aarch64', 'arm64' {
			return .arm64
		}
		'aarch32', 'arm32', 'arm' {
			return .arm32
		}
		'rv64', 'riscv64', 'risc-v64', 'riscv', 'risc-v' {
			return .rv64
		}
		'rv32', 'riscv32' {
			return .rv32
		}
		'x86_32', 'x32', 'i386', 'IA-32', 'ia-32', 'ia32' {
			return .i386
		}
		'js', 'js_node' {
			return .js_node
		}
		'js_browser' {
			return .js_browser
		}
		'js_freestanding' {
			return .js_freestanding
		}
		'wasm32', 'wasm' {
			return .wasm32
		}
		'' {
			return ._auto
		}
		else {
			return error('invalid arch: ${arch_str}')
		}
	}
}
