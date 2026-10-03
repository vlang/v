import os

fn test_enum_assignment_with_returning_match_branch_in_parallel_checker() {
	root := os.join_path(os.vtmp_dir(), 'enum_branch_exit_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'enum Choice { never auto always }
struct Args { mut: choice Choice }
fn choose(value string, mut args Args) ! {
	args.choice = match value {
		"never" { .never }
		"auto" { .auto }
		"always" { .always }
		else { return error("unrecognized") }
	}
}
fn main() {
	mut args := Args{}
	choose("always", mut args) or { panic(err) }
	assert args.choice == .always
	choose("invalid", mut args) or { assert err.msg() == "unrecognized" }
}
')!
	for ownership in ['', '-ownership -d ownership'] {
		for mode in ['', '-no-parallel'] {
			result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache ${ownership} ${mode} run ${os.quoted_path(source)}')
			assert result.exit_code == 0, result.output
		}
	}
}
