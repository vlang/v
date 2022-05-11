import os

fn main() {
	if os.args.len <= 1 {
		eprintln('please specify a C++ compiler')
		exit(1)
	}
	cc := os.args[1]
	if os.execute('${os.quoted_path(cc)} -v').exit_code != 0 {
		eprintln('please specify a valid C++ compiler')
		exit(1)
	}
	cc_type, cc_version, cc_os := get_cc_info(cc)
	triple := '$cc_type-$cc_version-$cc_os'
	println('compiler: $triple')

	search_paths := get_search_paths(cc)
	atomic_path := find_file(search_paths, 'atomic') or {
		eprintln(err)
		exit(2)
	}

	bitsatomicbase_path := find_file(search_paths, 'bits/atomic_base.h') or {
		if cc_os == 'linux' {
			eprintln(err)
			exit(2)
		}
		'no_file' // bits/atomic_base.h is only used on linux
	}

	patch_atomic(os.join_path(os.dir(@FILE), 'atomic.h'), atomic_path) or {
		eprintln(err)
		exit(2)
	}

	if bitsatomicbase_path != 'no_file' {
		patch_bitsatomicbase(os.join_path(os.dir(@FILE), 'bitsatomicbase.h'), bitsatomicbase_path) or {
			eprintln(err)
			exit(2)
		}
	}

	println('$atomic_path:::$bitsatomicbase_path')
}

fn get_cc_info(cc string) (string, string, string) {
	cc_type := if cc.contains('clang') {
		'clang'
	} else if cc.contains('g') {
		'gcc'
	} else {
		eprintln('only gcc and clang are supported')
		exit(1)
		'none'
	}

	lines := os.execute('${os.quoted_path(cc)} -v').output.split('\n')

	// gcc and clang both have the same way way to say what version they have and what the host target triple is
	cc_version := lines.filter(it.contains('$cc_type version '))[0].all_after('$cc_type version ').all_before('.')

	cc_os := lines.filter(it.starts_with('Target: '))[0].all_after('Target: ').split('-')[2]

	return cc_type, cc_version, if cc_os.contains('darwin') {
		'darwin' // remove 20.6.0 from darwin20.6.0
	} else {
		cc_os
	}
}

fn get_search_paths(cc string) []string {
	result := os.execute('${os.quoted_path(cc)} -v -x c++ /dev/null').output
	lines := result.split('\n')
	search_path := lines[lines.index('#include <...> search starts here:') + 1..lines.index('End of search list.')]
	return search_path.map(os.real_path(it.all_before('(').trim_space()))
}

fn find_file(search_paths []string, file string) ?string {
	for search_path in search_paths {
		if os.exists(os.join_path(search_path, file)) {
			return os.join_path(search_path, file)
		}
	}
	return error('$file not found')
}

fn patch_atomic(outfile string, infile string) ? {
	lines := os.read_file(infile)?.split('\n')
	outlines := lines.filter(!it.contains('atomic(const atomic&) = delete;'))
	outtext := outlines.join('\n').replace('#include <bits/atomic_base.h>', '#include "bitsatomicbase.h"')
	os.write_file(outfile, outtext)?
}

fn patch_bitsatomicbase(outfile string, infile string) ? {
	lines := os.read_file(infile)?.split('\n')
	outlines := lines.filter(!it.contains('__atomic_base(const __atomic_base&) = delete;'))
	outtext := outlines.join('\n').replace('#include <bits/atomic_base.h>', '#include "bitsatomicbase.h"')
	os.write_file(outfile, outtext)?
}
