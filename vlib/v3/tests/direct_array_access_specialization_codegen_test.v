import os

const direct_array_access_v3_dir = os.dir(os.dir(@FILE))
const direct_array_access_vlib_dir = os.dir(direct_array_access_v3_dir)

fn direct_array_access_specialized_fn_body(c_source string, marker string) string {
	mut search_from := 0
	for search_from < c_source.len {
		rel := c_source[search_from..].index(marker) or { return '' }
		start := search_from + rel
		open_rel := c_source[start..].index('{') or { return '' }
		semicolon_rel := c_source[start..].index(';') or { -1 }
		if semicolon_rel >= 0 && semicolon_rel < open_rel {
			search_from = start + marker.len
			continue
		}
		body_start := start + open_rel
		mut depth := 0
		for i in body_start .. c_source.len {
			if c_source[i] == `{` {
				depth++
			} else if c_source[i] == `}` {
				depth--
				if depth == 0 {
					return c_source[start..i + 1]
				}
			}
		}
		return ''
	}
	return ''
}

fn direct_array_access_matching_lines(c_source string) string {
	mut matches := []string{}
	for line in c_source.split_into_lines() {
		if line.contains('at_') || line.contains('at(') {
			matches << line
		}
	}
	return matches.join('\n')
}

fn test_direct_array_access_survives_generic_specialization() {
	root := os.join_path(os.temp_dir(), 'v3_direct_array_access_specialization_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}

	v3_bin := os.join_path(root, 'v3')
	build :=
		os.execute('${@VEXE} -prealloc -path "${direct_array_access_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${direct_array_access_v3_dir}/v3.v')
	assert build.exit_code == 0, build.output

	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, '@[direct_array_access]
fn at[T](values []T) T {
	return values[1]
}

fn main() {
	println(at[int]([1, 2]))
}
') or {
		panic(err)
	}
	c_path := os.join_path(root, 'main.c')
	gen := os.execute('${v3_bin} ${source_path} -b c -o ${c_path}')
	assert gen.exit_code == 0, gen.output
	c_source := os.read_file(c_path) or { panic(err) }
	body := direct_array_access_specialized_fn_body(c_source, 'at_T_v_int(')
	assert body.len > 0, direct_array_access_matching_lines(c_source)
	assert body.contains(').data'), body
	assert !body.contains('array_get('), body
}
