import os

fn test_freestanding_real_path_matches_hosted_filesystem_resolution() ! {
	$if !(linux || macos) || !(amd64 || arm64) {
		return
	}
	arch := $if amd64 { 'amd64' } $else { 'arm64' }
	root := os.join_path(os.vtmp_dir(), 'freestanding_realpath_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'real', 'inner'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'real', 'inner', 'entry.scm'), 'datum')!
	os.write_file(os.join_path(root, 'real', 'outer'), 'outer')!
	os.symlink('.', os.join_path(root, 'again'))!
	os.symlink('real/inner', os.join_path(root, 'relative'))!
	os.symlink(os.join_path(root, 'real', 'inner'), os.join_path(root, 'absolute'))!
	os.symlink('missing', os.join_path(root, 'dangling'))!
	os.symlink('cycle', os.join_path(root, 'cycle'))!
	os.symlink('growing/..', os.join_path(root, 'growing'))!
	os.symlink('right', os.join_path(root, 'left'))!
	os.symlink('left', os.join_path(root, 'right'))!
	long_target := './'.repeat(300) + 'real/inner'
	os.symlink(long_target, os.join_path(root, 'long_target'))!
	for i in 0 .. 42 {
		target := if i == 41 { 'real/inner' } else { 'chain${i + 1}' }
		os.symlink(target, os.join_path(root, 'chain${i}'))!
	}
	// Concatenate raw spellings deliberately: join_path normalizes the very
	// dot, slash and symlink-order cases this test must preserve.
	mut paths := [
		root + '/real/inner/entry.scm',
		root + '/again/again/real/inner/entry.scm',
		root + '/relative/../outer',
		root + '/absolute/./entry.scm',
		root + '/relative//../inner/',
		root + '/long_target/entry.scm',
		root + '/long_target/' + './'.repeat(300) + 'entry.scm',
		root + '/real/' + './'.repeat(2100) + 'inner/entry.scm',
		root + '/chain41/entry.scm',
		root + '/chain11/entry.scm',
		root + '/chain10/entry.scm',
		root + '/chain9/entry.scm',
		root + '/chain8/entry.scm',
		root + '/chain2/entry.scm',
		root + '/chain1/entry.scm',
		root + '/chain0/entry.scm',
		root + '/dangling',
		root + '/dangling/../real',
		root + '/cycle',
		root + '/growing',
		root + '/left',
		root + '/cycle/..',
		root + '/real/inner/entry.scm/',
		root + '/real/inner/entry.scm/.',
		root + '/real/inner/entry.scm/..',
		root + '/missing/../real',
		root + '/' + 'x'.repeat(256),
		root + '/' + 'x'.repeat(4096),
		root + '/real/inner/../../../../../../../../..',
		'.',
		'..',
		'/',
		'////',
		'',
		(os.path_rel(os.getwd(), root)!) + '/relative/../outer',
	]
	// Darwin's initial spelling and expanded pending suffix have distinct bounds.
	// The hosted oracle also preserves Linux's acceptance of these long spellings.
	relative := (os.path_rel(os.getwd(), root)!) + '/real/inner/entry.scm'
	for limit in [1023, 1024, 1025] {
		suffix := 'real/inner/entry.scm'
		paths << root + '/'.repeat(limit - root.len - suffix.len) + suffix
		relative_padding := limit - relative.len
		paths << './'.repeat(relative_padding / 2) +
			(if relative_padding % 2 == 1 { '/' } else { '' }) + relative
		paths << '/'.repeat(limit)
		padding := limit - long_target.len - 1 - 'entry.scm'.len
		paths << root + '/long_target/' + './'.repeat(padding / 2) +
			(if padding % 2 == 1 { '/' } else { '' }) + 'entry.scm'
	}
	mut arguments := []string{}
	for path in paths { arguments << [path, os.real_path(path)] }
	source := os.join_path(@VEXEROOT, 'vlib/os/testdata/freestanding_realpath/main.c.v')
	for optimized in [false, true] {
		for mode in ['hosted', 'bare', 'syscalls', 'autofree'] {
			binary := os.join_path(root, '${mode}_${optimized}')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-arch',
				arch]
			if optimized { command << '-prod' }
			if mode in ['bare', 'autofree'] { command << '-freestanding' }
			if mode == 'autofree' { command << '-autofree' }
			if mode == 'syscalls' {
				command << '-syscalls'
				$if linux {
					command << ['-d', 'realpath_track_alloc', '-cflags', '-DREALPATH_TRACK_ALLOC',
						'-ldflags', '-Wl,--wrap=malloc,--wrap=calloc,--wrap=realloc,--wrap=free']
				}
			}
			command << ['-o', binary, source]
			built := os.exec(command)
			assert built.exit_code == 0, '${mode}: ${built.output}'
			ran := os.exec([binary, ...arguments])
			assert ran.exit_code == 0, '${mode}: ${ran.output}'
			assert ran.output.trim_space() == 'freestanding real_path passed', ran.output
		}
	}
}
