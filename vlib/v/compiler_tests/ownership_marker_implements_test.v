import arrays
import os

fn test_ownership_markers_and_signed_integer_parsing() {
	root := os.join_path(os.vtmp_dir(), 'ownership_markers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import strconv

fn main() {
	negative := "-12345".to_owned()
	negative_address := unsafe { uint(negative.str) }
	assert negative.int() == -12345
	assert negative.int() == -12345
	assert strconv.parse_int(negative, 10, 64)! == -12345
	assert negative == "-12345"
	assert unsafe { uint(negative.str) == negative_address }
	positive := "+23456".to_owned()
	positive_address := unsafe { uint(positive.str) }
	assert positive.int() == 23456
	assert strconv.parse_int(positive, 10, 64)! == 23456
	assert positive == "+23456"
	assert unsafe { uint(positive.str) == positive_address }
	unsigned := "34567".to_owned()
	assert unsigned.int() == 34567
	assert strconv.parse_int(unsigned, 10, 64)! == 34567
	assert unsigned == "34567"
	zero := "-0".to_owned()
	assert zero.int() == 0
	assert zero == "-0"
	minimum := "-9223372036854775808".to_owned()
	assert strconv.parse_int(minimum, 10, 64)! == min_i64
	assert minimum == "-9223372036854775808"
	invalid := "-12x".to_owned()
	if _ := strconv.common_parse_int(invalid, 10, 64, true, false) {
		assert false
	}
	assert invalid == "-12x"
	assert invalid.int() == -12
	overflow := "-9223372036854775809".to_owned()
	if _ := strconv.common_parse_int(overflow, 10, 64, true, true) {
		assert false
	}
	assert overflow == "-9223372036854775809"
	println("ok")
	check_ownership_markers()
}
')!
	os.write_file(os.join_path(root, 'markers.v'), 'struct Resource implements Owned {
	value int
}

struct Number implements Copy {
	value int
}

struct Tracked implements Drop {
	count &int
}

fn (mut value Tracked) drop() {
	unsafe { *value.count += 1 }
}

fn create_tracked(count &int) {
	value := Tracked{count: count}
	assert *value.count == 0
}

fn check_ownership_markers() {
	resource := Resource{value: 3}
	assert resource.value == 3
	number := Number{value: 7}
	number_copy := number
	assert number.value == number_copy.value
	mut count := 0
	create_tracked(&count)
	assert count == 1
	println("ownership markers")
}
')!

	for mode in [['-no-parallel'], []string{}] {
		result := os.exec([@VEXE, '-no-memory-limit', '-nocache', '-ownership', '-d', 'ownership',
			'-cc', 'clang', ...mode, 'run', root])
		assert result.exit_code == 0, result.output
		assert result.output.split_into_lines() == ['ok', 'ownership markers'], result.output
	}
}

fn test_ownership_marker_names_do_not_hide_declared_noninterfaces() {
	root := os.join_path(os.vtmp_dir(), 'ownership_marker_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	markers := ['Owned', 'Copy', 'Drop']
	// Custom flags can limit the diagnostics emitted by a single check.
	batch_size := if os.getenv('VFLAGS').trim_space() in ['', '-gc none'] { markers.len } else { 1 }
	for batch in arrays.chunk(markers, batch_size) {
		declarations := batch.map('struct ${it} {}\nstruct ${it}Resource implements ${it} {}')
		uses := batch.map('\t_ = ${it}Resource{}')
		os.write_file(source, '${declarations.join('\n\n')}\n\nfn main() {\n${uses.join('\n')}\n}\n')!
		result := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-check', source])
		assert result.exit_code != 0, result.output
		for marker in batch {
			assert result.output.contains('`${marker}` is not an interface type'), result.output
		}
	}
}
