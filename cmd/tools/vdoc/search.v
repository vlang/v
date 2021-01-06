module main

struct SearchModuleResult {
	description string
	link        string
}

struct SearchResult {
	prefix      string
	badge       string
	description string
	link        string
}

fn (cfg DocConfig) render_search_index() {
	mut js_search_index := strings.new_builder(200)
	mut js_search_data := strings.new_builder(200)
	js_search_index.write('var searchModuleIndex = [')
	js_search_data.write('var searchModuleData = [')
	for i, title in cfg.search_module_index {
		data := cfg.search_module_data[i]
		js_search_index.write('"$title",')
		js_search_data.write('["$data.description","$data.link"],')
	}
	js_search_index.writeln('];')
	js_search_index.write('var searchIndex = [')
	js_search_data.writeln('];')
	js_search_data.write('var searchData = [')
	for i, title in cfg.search_index {
		data := cfg.search_data[i]
		js_search_index.write('"$title",')
		// array instead of object to reduce file size
		js_search_data.write('["$data.badge","$data.description","$data.link","$data.prefix"],')
	}
	js_search_index.writeln('];')
	js_search_data.writeln('];')
	out_file_path := os.join_path(cfg.output_path, 'search_index.js')
	os.write_file(out_file_path, js_search_index.str() + js_search_data.str())
}


fn (mut cfg DocConfig) collect_search_index() {
	if cfg.output_type != .html {
		return
	}
	for doc in cfg.docs {
		mod := doc.head.name
		cfg.search_module_index << mod
		comments := if cfg.include_examples {
			doc.head.merge_comments()
		} else {
			doc.head.merge_comments_without_examples()
		}
		cfg.search_module_data << SearchModuleResult{
			description: trim_doc_node_description(comments)
			link: cfg.get_file_name(mod)
		}
		for _, dn in doc.contents {
			cfg.create_search_results(mod, dn)
		}
	}
}

fn (mut cfg DocConfig) create_search_results(mod string, dn doc.DocNode) {
	if dn.kind == .const_group {
		return
	}
	comments := if cfg.include_examples {
		dn.merge_comments()
	} else {
		dn.merge_comments_without_examples()
	}
	dn_description := trim_doc_node_description(comments)
	cfg.search_index << dn.name
	cfg.search_data << SearchResult{
		prefix: if dn.parent_name != '' {
			'$dn.kind ($dn.parent_name)'
		} else {
			'$dn.kind '
		}
		description: dn_description
		badge: mod
		link: cfg.get_file_name(mod) + '#' + get_node_id(dn)
	}
	for child in dn.children {
		cfg.create_search_results(mod, child)
	}
}
