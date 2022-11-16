import json
import os

fn test_generics_fn_infer_nested_generic_fn() {
	mut spawns := []ItemSpawn{}
	load_item_spawns(mut spawns)
	println(spawns)
	assert spawns.len == 0
}

fn load_item_spawns(mut spawns []ItemSpawn) {
	walk<ItemSpawn>('./data/items/spawns/', mut spawns)
}

fn parse_json<T>(file string, mut array []T) {
	data := os.read_file(file) or {
		panic('error reading file ${file}')
		return
	}
	decoded_data := json.decode([]T, data) or {
		eprintln('Failed to parse item spawns')
		return
	}
	for d in decoded_data {
		array << d
	}
}

fn walk<T>(path string, mut array []T) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or { return }
	for file in files {
		p := path + os.path_separator + file
		if os.is_dir(p) && !os.is_link(p) {
			walk(p, mut array)
		} else if os.exists(p) {
			parse_json(p, mut array)
		}
	}
}

struct ItemSpawn {
	id     int
	amount int = 1
	x      int
	y      int
}
