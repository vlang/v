import os

// v-testing's SKILL.md says which asserts get the "a `-prod` build drops it" notice: a channel
// receive in every build, a call that modifies its receiver or takes a `mut` argument under
// `-prod` as well, and nothing for an operand that is only read for its type.
const skill_assert_source = "
fn bump(mut a []int) bool {
	a << 1
	return true
}

fn main() {
	ch := chan int{cap: 1}
	ch <- 1
	assert <-ch == 1
	mut values := [1, 2, 3]
	assert values.pop() == 3
	assert bump(mut values)
	empty := chan int{}
	assert typeof(<-empty).name == 'int'
	assert sizeof(<-empty) == sizeof(int)
	got := values.len
	assert got == 3
}
"

fn skill_assert_notices(flags []string) []string {
	root := os.join_path(os.vtmp_dir(), 'skill_assert_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, skill_assert_source.trim_left('\n')) or { panic(err) }
	result := os.exec([@VEXE, ...flags, '-check', source])
	assert result.exit_code == 0, result.output
	return result.output.split_into_lines().filter(it.contains('notice: assert contains'))
}

fn test_assert_skill_claim_in_every_build() {
	notices := skill_assert_notices([])
	assert notices.len == 1, notices.join('\n')
	assert notices[0].contains('a channel receive')
	assert notices[0].contains(':9:'), notices[0]
}

fn test_assert_skill_claim_under_prod() {
	notices := skill_assert_notices(['-prod'])
	assert notices.len == 3, notices.join('\n')
	assert notices[0].contains('a channel receive')
	assert notices[1].contains('a call that modifies its receiver')
	assert notices[2].contains('a call with a `mut` argument')
}
