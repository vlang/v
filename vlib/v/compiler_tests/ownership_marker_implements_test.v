import arrays
import os

fn test_ownership_markers_are_valid_implements_entries() {
	root := os.join_path(os.vtmp_dir(), 'ownership_markers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Resource implements Owned {
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

fn main() {
	resource := Resource{value: 3}
	assert resource.value == 3
	number := Number{value: 7}
	number_copy := number
	assert number.value == number_copy.value
	mut count := 0
	create_tracked(&count)
	assert count == 1
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		result := os.execute('${os.quoted_path(@VEXE)} -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'ok', result.output
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
