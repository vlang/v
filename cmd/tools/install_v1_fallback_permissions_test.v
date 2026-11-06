import os
import strings
import v.cmdexec

fn test_fallback_installer_preserves_special_permission_bits() ! {
	$if windows {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v1_installer_permissions_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'bin')
	os.mkdir_all(bin)!
	stat := os.join_path(bin, 'stat')
	// Model GNU stat's permission-only mode and BSD stat's full mode. The BSD
	// low field deliberately omits special bits, as OpenBSD stat does.
	os.write_file(stat, r'#!/bin/sh
case "$V1_TEST_STAT_STYLE:$1:$2" in
    gnu:-c:%u|bsd:-f:%u) printf "%s\n" "$V1_TEST_STAT_OWNER" ;;
    gnu:-c:%a) printf "%s\n" "$V1_TEST_STAT_MODE" ;;
    bsd:-f:%Op) printf "%s\n" "$V1_TEST_STAT_MODE" ;;
    bsd:-f:%Lp) printf "%o\n" "$((0$V1_TEST_STAT_MODE & 0777))" ;;
    *) exit 1 ;;
esac
')!
	os.chmod(stat, 0o755)!
	installer := os.read_file(os.join_path(@VEXEROOT, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	helpers := installer.all_before('\neffective_uid=\n')
	assert helpers.len < installer.len
	script := os.join_path(dir, 'check.sh')
	os.write_file(script, helpers + r'
effective_uid=$(id -u)
if [ "$V1_TEST_CHECK" = private ]; then
    validate_private_directory "$V1_TEST_DIRECTORY"
else
    validate_trusted_temp_root "$V1_TEST_DIRECTORY"
fi
')!
	// Each row is mode, whether it is a trusted temporary root, and whether it
	// is a private cache directory. Private storage must reject special bits.
	cases := [
		['700', 'yes', 'yes'],
		['755', 'yes', 'no'],
		['777', 'no', 'no'],
		['720', 'no', 'no'],
		['1777', 'yes', 'no'],
		['2700', 'yes', 'no'],
		['4700', 'yes', 'no'],
	]
	owner := os.geteuid().str()
	for style in ['gnu', 'bsd'] {
		for row in cases {
			mode := if style == 'bsd' { '4' + strings.pad_left(row[0], 4, '0') } else { row[0] }
			for check in ['root', 'private'] {
				result := cmdexec.run_in_merged_with_env('/bin/sh', [script, 'unused', 'unused'],
					@VEXEROOT, {
						'PATH':               bin + os.path_delimiter + os.getenv('PATH')
						'V1_TEST_STAT_STYLE': style
						'V1_TEST_STAT_OWNER': owner
						'V1_TEST_STAT_MODE':  mode
						'V1_TEST_DIRECTORY':  dir
						'V1_TEST_CHECK':      check
					})
				expected := row[if check == 'root' { 1 } else { 2 }] == 'yes'
				assert (result.exit_code == 0) == expected, '${style} ${mode} ${check}: ${result.output}'
			}
		}
	}
}
