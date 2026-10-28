module types

import os

const attribute_narrowing_prefix = "@[attribute]
struct Route { path string }
@[attribute]
struct Get { path string }
@[attribute]
struct Limit { path int }
@[Route('/r')]
@[Get('/g')]
@[Limit(7)]
fn target() {}
fn takes_string(value string) {}
fn takes_int(value int) {}
"

fn check_attribute_narrowing_source(body string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'attribute_narrowing_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, attribute_narrowing_prefix + '\nfn main() {\n' + body + '\n}\n') or {
		panic(err)
	}
	return os.exec([@VEXE, '-check', path])
}

fn test_attribute_narrowing_checks_every_expression_shape() {
	for expression in [
		'x := attr.path\nprintln(x)',
		'mut x := ""\nx = attr.path\nprintln(x)',
		'assert attr.path == "/r"',
		'println("path=\${attr.path}")',
		'if attr.path == "/r" { println("found") }',
		'println(attr.path.len)',
	] {
		result := check_attribute_narrowing_source('$for attr in target.attributes {\n' + expression + '\n}')
		assert result.exit_code != 0, expression
		assert result.output.contains('attribute field `path` requires narrowing'), result.output
	}
	unknown := check_attribute_narrowing_source('$for attr in target.attributes {\nx := attr.missing\nprintln(x)\n}')
	assert unknown.exit_code != 0
	assert unknown.output.contains('unknown field `missing` from attr'), unknown.output
}

fn test_attribute_narrowing_tracks_boolean_branches_and_field_types() {
	result := check_attribute_narrowing_source('$for attr in target.attributes {
		println(attr.str())
		$if attr is Limit { takes_int(attr.path) }
		$if attr is Route || attr is Get { takes_string(attr.path) }
		$if attr.name == "Route" && attr is Route { takes_string(attr.path) }
		$if attr.has_arg && attr is Route { takes_string(attr.path) }
		$if !(attr is Route) {} $else { takes_string(attr.path) }
		$if attr is Route {
			$if attr is Get {} $else { takes_string(attr.path) }
		}
	}')
	assert result.exit_code == 0, result.output
}

fn test_attribute_narrowing_requires_every_possible_schema_to_agree() {
	for condition in ['attr is Route || attr is Limit', 'attr !is Get'] {
		result := check_attribute_narrowing_source('$for attr in target.attributes {\n$if ' + condition + ' { takes_string(attr.path) }\n}')
		assert result.exit_code != 0
		assert result.output.contains('attribute field `path`'), result.output
	}
	condition := check_attribute_narrowing_source('$for attr in target.attributes {\n$if attr is Route {\n$if attr.path == "/r" {}\n}\n}')
	assert condition.exit_code != 0
	assert condition.output.contains('unknown field `path` from attr'), condition.output
}
