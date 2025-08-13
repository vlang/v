module main

import document as doc
import v.vmod
import os
import rand
import term

fn (mut vd VDoc) process_all_examples(contents []doc.DocNode) {
	for cn in contents {
		if cn.content.len > 0 {
			vd.run_examples(cn)
		}
		vd.process_all_examples(cn.children)
	}
}

const normalised_default_vmodules_path = os.vmodules_dir().replace('\\', '/')

fn get_mod_name_by_file_path(file_path string) string {
	mut mcache := vmod.get_cache()
	dn_folder := os.dir(os.real_path(file_path)).replace('\\', '/').trim_string_right('/src')
	vmodpath := mcache.get_by_folder(dn_folder)
	normal_folder := dn_folder.replace('\\', '/')
	vmod_folder := vmodpath.vmod_folder.replace('\\', '/')
	mut relative_mod_path := normal_folder
	relative_mod_path = relative_mod_path.trim_string_left(vmod_folder).trim_string_left('/')
	relative_mod_path = relative_mod_path.trim_string_left(normalised_default_vmodules_path)
	relative_mod_path = relative_mod_path.trim_string_left('vlib/')
	mod_name := relative_mod_path.replace('/', '.').trim('.')
	return mod_name
}

fn (mut vd VDoc) run_examples(dn doc.DocNode) {
	if dn.comments.len == 0 || vd.cfg.run_examples == .skip {
		return
	}
	voptions := match vd.cfg.run_examples {
		.run { ' -g run ' }
		.check { '-N -W -check' }
		.skip { '' }
	}
	examples := dn.examples()
	if examples.len == 0 {
		return
	}
	efolder := os.vtmp_dir()
	mut example_program_source_files := []string{}
	defer {
		for sfile in example_program_source_files {
			os.rm(sfile) or {}
		}
	}
	mut failures := 0
	mut oks := 0
	for example in examples {
		code := example.all_after('Example:').all_after('example:').trim_space()
		mod_name := get_mod_name_by_file_path(dn.file_path)
		vsource_path := os.join_path(efolder, 'example_${rand.ulid()}.v')
		// eprintln('>>> example dn.file_path: ${dn.file_path} | mod_name: ${mod_name} | vsource_path: ${vsource_path} | code: `${code}`')
		import_clause := if mod_name in ['builtin', ''] { '' } else { 'import ${mod_name}\n' }
		source := '${import_clause}fn main() {\n\t${code}\n}\n'
		os.write_file(vsource_path, source) or { continue }
		cmd := '${os.quoted_path(vexe)} ${voptions} ${os.quoted_path(vsource_path)}'
		res := os.execute(cmd)
		if res.exit_code != 0 {
			eprintln('${dn_to_location(dn)}:${term.ecolorize(term.red, 'error in documentation example')}')
			eprintln('     cmd: ${cmd}')
			eprintln('  result:')
			eprintln(res.output)
			failures++
			continue
		}
		example_program_source_files << vsource_path
		oks++
	}
	vd.example_failures += failures
	vd.example_oks += oks
}
