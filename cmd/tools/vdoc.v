module main

import markdown
import net
import net.urllib
import os
import os.cmdline
import strings
import v.doc
import v.util
import v.vmod

enum OutputType {
	html
	markdown
	json
	plaintext
	stdout
}

struct DocConfig {
	pub_only bool = true
	show_loc bool = false // for plaintext
	serve_http bool = false // for html
	is_multi bool = false
	include_readme bool = false
mut:
	opath string
	src_path string
	docs []doc.Doc
	manifest vmod.Manifest
}

fn slug(title string) string {
	return title.replace(' ', '-')
}

fn open_url(url string) {
	$if windows {
		os.system('start $url')
	}

	$if macos {
		os.system('open $url')
	}

	$if linux {
		os.system('xdg-open $url')
	}
}

fn (mut cfg DocConfig) serve_html() {
	docs := cfg.multi_render(.html)
	def_name := docs.keys()[0]
	server := net.listen(8046) or {
		panic(err)
	}
	println('Serving docs on: http://localhost:8046')
	open_url('http://localhost:8046')

	for {
		con := server.accept() or {
			server.close() or { }
			panic(err)
		}

		s := con.read_line()
		first_line := s.all_before('\n')
		mut filename := def_name

		if first_line.len != 0 {
			data := first_line.split(' ')
			url := urllib.parse(data[1]) or { return }
			filename = if url.path == '/' { def_name } else { url.path.trim_left('/') }
		}

		html := docs[filename]

		con.write('HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n$html') or {
			con.close() or { return }
			return
		}

		con.close() or { return }
	}
}

fn get_src_link(repo_url string, file_name string, line_nr int) string {
	mut url := urllib.parse(repo_url) or {
		return ''
	}
	if url.path.len <= 1 || file_name.len == 0 {
		return ''
	}
	url.path = url.path.trim_right('/') + match url.host {
		'github.com' { '/blob/master/$file_name' }
		'gitlab.com' { '/-/blob/master/$file_name' }
		'git.sir.ht' { '/tree/master/$file_name' }
		else { '' }
	}
	if url.path == '/' { return '' }
	url.fragment = 'L$line_nr'
	return url.str()
}

fn escape(str string) string {
	return str.replace_each(['"', '\\"', '\r\n', '\\n', '\n', '\\n'])
}

fn (cfg DocConfig) gen_json(idx int) string {
	dcs := cfg.docs[idx]
	mut jw := strings.new_builder(200)
	jw.writeln('{\n\t"module_name": "$dcs.head.name",\n\t"description": "${escape(dcs.head.comment)}",\n\t"contents": [')
	for i, cn in dcs.contents {
		name := cn.name[dcs.head.name.len+1..]
		jw.writeln('\t\t{')
		jw.writeln('\t\t\t"name": "$name",')
		jw.writeln('\t\t\t"signature": "${escape(cn.content)}",')
		jw.writeln('\t\t\t"description": "${escape(cn.comment)}"')
		jw.write('\t\t}')
		if i < dcs.contents.len-1 { jw.writeln(',') }
	}
	jw.writeln('\n\t],')
	jw.write('\t"generator": "vdoc",\n\t"time_generated": "${dcs.time_generated.str()}"\n}')
	return jw.str()
}

fn (cfg DocConfig) gen_html(idx int) string {
	dcs := cfg.docs[idx]
	mut hw := strings.new_builder(200)
	mut toc := strings.new_builder(200)
	mut doc_node_html := fn (dd doc.DocNode, link string, head bool) string {
		mut dnw := strings.new_builder(200)
		head_tag := if head { 'h1' } else { 'h2' }
		md_content := markdown.to_html(dd.comment)
		dnw.writeln('<section id="${slug(dd.name)}" class="doc-node">')
		if dd.name != 'README' {
			dnw.write('<$head_tag>${dd.name} <a href="#${slug(dd.name)}">#</a></$head_tag>')
		}
		if head {
			dnw.write(md_content)
		} else {
			dnw.writeln('<code class="code-snippet">${dd.content}</code>')
			dnw.writeln(md_content)
			if link.len != 0 {
				dnw.writeln('<p>[<a href="#$link">Source</a>]</p>')
			}
		}
		dnw.writeln('</section>')
		return dnw.str()
	}
	// generate toc first
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		toc.write('<li><a href="#${slug(cn.name)}">${cn.name}</a>')
		children := dcs.contents.find_children_of(cn.name)
		if children.len != 0 {
			toc.writeln('        <ul>')
			for child in children {
				toc.writeln('<li><a href="#${slug(child.name)}">${child.name}</a></li>')
			}
			toc.writeln('</ul>')
		}
		toc.writeln('</li>')
	}	// write head
	hw.write('<!DOCTYPE html><html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${dcs.head.name} | vdoc</title>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700&display=swap" rel="stylesheet">')
	// write css
	hw.write(r'<style>/*! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css */html{line-height:1.15;-webkit-text-size-adjust:100%}body{margin:0}main{display:block}h1{font-size:2em;margin:.67em 0}hr{box-sizing:content-box;height:0;overflow:visible}pre{font-family:monospace,monospace;font-size:1em}a{background-color:transparent}abbr[title]{border-bottom:none;text-decoration:underline;text-decoration:underline dotted}b,strong{font-weight:bolder}code,kbd,samp{font-family:monospace,monospace;font-size:1em}small{font-size:80%}sub,sup{font-size:75%;line-height:0;position:relative;vertical-align:baseline}sub{bottom:-.25em}sup{top:-.5em}img{border-style:none}button,input,optgroup,select,textarea{font-family:inherit;font-size:100%;line-height:1.15;margin:0}button,input{overflow:visible}button,select{text-transform:none}[type=button],[type=reset],[type=submit],button{-webkit-appearance:button}[type=button]::-moz-focus-inner,[type=reset]::-moz-focus-inner,[type=submit]::-moz-focus-inner,button::-moz-focus-inner{border-style:none;padding:0}[type=button]:-moz-focusring,[type=reset]:-moz-focusring,[type=submit]:-moz-focusring,button:-moz-focusring{outline:1px dotted ButtonText}fieldset{padding:.35em .75em .625em}legend{box-sizing:border-box;color:inherit;display:table;max-width:100%;padding:0;white-space:normal}progress{vertical-align:baseline}textarea{overflow:auto}[type=checkbox],[type=radio]{box-sizing:border-box;padding:0}[type=number]::-webkit-inner-spin-button,[type=number]::-webkit-outer-spin-button{height:auto}[type=search]{-webkit-appearance:textfield;outline-offset:-2px}[type=search]::-webkit-search-decoration{-webkit-appearance:none}::-webkit-file-upload-button{-webkit-appearance:button;font:inherit}details{display:block}summary{display:list-item}[hidden],template{display:none}blockquote,dd,dl,figure,h1,h2,h3,h4,h5,h6,hr,p,pre{margin:0}button{background-color:transparent;background-image:none;padding:0}button:focus{outline:1px dotted;outline:5px auto -webkit-focus-ring-color}fieldset,ol,ul{margin:0;padding:0}ol,ul{list-style:none}html{font-family:Inter,system-ui,-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica Neue,Arial,Noto Sans,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol,Noto Color Emoji;line-height:1.5}*,:after,:before{box-sizing:border-box;border:0 solid #e2e8f0}hr{border-top-width:1px}img{border-style:solid}textarea{resize:vertical}input::placeholder,textarea::placeholder{color:#a0aec0}[role=button],button{cursor:pointer}table{border-collapse:collapse}h1,h2,h3,h4,h5,h6{font-size:inherit;font-weight:inherit}a{color:inherit;text-decoration:inherit}button,input,optgroup,select,textarea{padding:0;line-height:inherit;color:inherit}code,kbd,pre,samp{font-family:SFMono-Regular,Menlo,Monaco,Consolas,Liberation Mono,Courier New,monospace}audio,canvas,embed,iframe,img,object,svg,video{display:block;vertical-align:middle}img,video{max-width:100%;height:auto}.container{width:100%}.flex{display:flex}.hidden{display:none}.font-bold{font-weight:700}.resize{resize:both}.text-gray-600{--text-opacity:1;color:#718096;color:rgba(113,128,150,var(--text-opacity))}.doc-container{display:flex;flex-direction:column-reverse;min-height:100vh;margin-top:10%}.doc-nav{position:fixed;left:0;top:0;right:0;display:flex;flex-direction:column;--bg-opacity:1;background-color:#2b6cb0;background-color:rgba(43,108,176,var(--bg-opacity));--text-opacity:1;color:#fff;color:rgba(255,255,255,var(--text-opacity));padding:1rem}.doc-nav .heading{display:flex;justify-content:space-between;align-items:center}.doc-nav .heading h2{font-size:1.875rem;font-weight:500}.doc-nav .heading .version{font-size:1.5rem;font-weight:500;margin-right:auto;--text-opacity:1;color:#bee3f8;color:rgba(190,227,248,var(--text-opacity))}.doc-nav nav{flex-direction:column;--text-opacity:1;color:#fff;color:rgba(255,255,255,var(--text-opacity));padding-left:2rem;padding-right:2rem;padding-bottom:3rem;margin-top:.5rem;margin-left:-2rem;margin-right:-2rem;height:100%;overflow-y:auto;max-height:100vh}.doc-nav nav .section{text-decoration:none;display:flex;flex-direction:row;align-items:center;justify-content:space-between;font-size:1.25rem;font-weight:500;margin-bottom:1rem}.doc-nav nav .section svg{width:2rem}.doc-nav nav a{font-size:1.125rem;word-break:break-all}.doc-nav nav a:hover{text-decoration:underline;--text-opacity:1;color:#bee3f8;color:rgba(190,227,248,var(--text-opacity))}.doc-nav nav ul{display:flex;flex-direction:column}.doc-nav nav ul li:not(:last-child){margin-bottom:.5rem}.doc-nav nav ul li ul{padding-left:.75rem;border-left-width:2px;margin-top:1rem;--border-opacity:1;border-color:#2c5282;border-color:rgba(44,82,130,var(--border-opacity))}.doc-content{--bg-opacity:1;background-color:#fff;background-color:rgba(255,255,255,var(--bg-opacity));width:100%;padding:2rem}.doc-content .doc-node{padding-bottom:2rem}.doc-content p,.doc-content ul li{font-size:1.125rem;margin-bottom:1rem}.doc-content ul li p{margin-bottom:0}.doc-content p,.doc-content p img{display:inline-block}.doc-content code{--bg-opacity:1;background-color:#e2e8f0;background-color:rgba(226,232,240,var(--bg-opacity));padding:.25rem;border-radius:.25rem}.doc-content a{--text-opacity:1;color:#2b6cb0;color:rgba(43,108,176,var(--text-opacity));text-decoration:underline}.doc-content code.code-snippet,.doc-content pre code{margin-bottom:2rem;margin-top:.5rem;white-space:pre;overflow-x:auto;display:inline-block;padding:1rem;width:100%;border-radius:.5rem;-webkit-overflow-scrolling:touch}.doc-content ul{list-style-type:disc;list-style-position:inside}.doc-content h1{font-size:2.25rem}.doc-content h2{font-size:1.875rem}.doc-content h3{font-size:1.5rem}.doc-content h4{font-size:1.25rem}.doc-content h5{font-size:1.125rem}.doc-content h6{font-size:1rem}.doc-content h1,.doc-content h2,.doc-content h3,.doc-content h4,.doc-content h5,.doc-content h6{font-weight:700;border-bottom-width:2px;padding-bottom:.5rem;margin-bottom:1rem}.doc-content h1 a,.doc-content h2 a,.doc-content h3 a,.doc-content h4 a,.doc-content h5 a,.doc-content h6 a{display:none;text-decoration:none;--text-opacity:1;color:#a0aec0;color:rgba(160,174,192,var(--text-opacity))}.doc-content h1:hover a,.doc-content h2:hover a,.doc-content h3:hover a,.doc-content h4:hover a,.doc-content h5:hover a,.doc-content h6:hover a{display:inline-block}.doc-toc{padding:2rem;font-size:1.25rem;width:100%;max-height:100vh;height:100%}.doc-toc .section{--text-opacity:1;color:#718096;color:rgba(113,128,150,var(--text-opacity));text-decoration:none;display:flex;flex-direction:row-reverse;align-items:center;justify-content:flex-end;font-size:1.25rem;font-weight:500;margin-bottom:1rem}.doc-toc .section svg{margin-right:.5rem;width:1.3rem}.doc-toc a{--text-opacity:1;color:#2b6cb0;color:rgba(43,108,176,var(--text-opacity));text-decoration:none}.doc-toc a:hover{text-decoration:underline}.doc-toc ul{display:flex;flex-direction:column;overflow-y:auto;height:100%;padding-bottom:2rem}.doc-toc ul li:not(:last-child){margin-bottom:.5rem}.doc-toc ul li ul{padding-left:.75rem;margin-top:1rem;border-left-width:2px;padding-bottom:0;--border-opacity:1;border-color:#cbd5e0;border-color:rgba(203,213,224,var(--border-opacity));overflow-y:visible}@media (min-width:640px){.container{max-width:640px}}@media (min-width:768px){.container{max-width:768px}}@media (min-width:1024px){.container{max-width:1024px}.doc-container{flex-direction:row;margin-top:0}.doc-nav{bottom:0;flex-direction:column;width:16.666667%;height:100vh;padding-left:2rem;padding-right:2rem}.doc-nav .heading{align-items:flex-end}.doc-nav .heading h2{font-size:2.25rem;margin-bottom:-.25rem}.doc-nav .heading .version{margin-right:0}.doc-nav nav{margin-top:2rem;margin-bottom:-3rem}.doc-content{width:50%;margin-left:16.666667%}.doc-content h1{font-size:3rem}.doc-content h2{font-size:2.25rem}.doc-content h3{font-size:1.875rem}.doc-content h4{font-size:1.5rem}.doc-content h5{font-size:1.25rem}.doc-content h6,.doc-toc{font-size:1.125rem}.doc-toc{position:fixed;width:16.666667%;margin-top:0;left:66.66667%}.lg\:flex{display:flex}.lg\:hidden{display:none}}@media (min-width:1280px){.container{max-width:1280px}}</style></head><body>')

	version := if cfg.manifest.version.len != 0 { '<span class="version">${cfg.manifest.version}</span>' } else { '' }
	repo_link := if cfg.manifest.repo_url.len != 0 {
		'<a href="${cfg.manifest.repo_url}" class="section">
			Repository
			<svg fill="currentColor" width="13%" viewBox="0 0 20 20"><path d="M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z" clip-rule="evenodd" fill-rule="evenodd"></path></svg>
		</a>'
	} else { '' }
	header_name := if cfg.is_multi && cfg.docs.len > 1 { os.file_name(os.real_path(cfg.src_path)) } else { dcs.head.name }
	// write nav
	hw.write('<header class="doc-nav">
		<div class="heading">
			<h2>${header_name}</h2>
			${version}
			<button id="toggleMenu" class="lg:hidden">
				<svg fill="currentColor" width="2rem" viewBox="0 0 20 20"><path d="M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z" clip-rule="evenodd" fill-rule="evenodd"></path></svg>
			</button>
		</div>
		<nav class="toggleable hidden lg:flex">
			${repo_link}
			<p class="section">
				Contents
				<svg fill="currentColor" width="13%" viewBox="0 0 20 20"><path d="M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1z" clip-rule="evenodd" fill-rule="evenodd"></path></svg>
			</p><ul>')

	if cfg.is_multi && cfg.docs.len > 1 {
		for doc in cfg.docs {
			class_css := if doc.head.name == dcs.head.name { 'class="font-bold" ' } else { '' }
			href_name := if doc.head.name == 'README' { 'index' } else { doc.head.name }
			hw.write('<li><a ${class_css}href="./${href_name}.html">${doc.head.name}</a>')
		}
	} else {
		hw.writeln(toc.str())
	}

	hw.write('</ul>\n</nav>\n</header>')
	hw.write('<div class="doc-container">\n<div class="doc-content">\n')
	hw.write(doc_node_html(dcs.head, '', true))
	for cn in dcs.contents {
		if cn.parent_type !in ['void', ''] { continue }
		hw.write(doc_node_html(cn, get_src_link(cfg.manifest.repo_url, os.file_name(cn.file_path), cn.pos.line), false))

		children := dcs.contents.find_children_of(cn.name)

		if children.len != 0 {
			for child in children {
				hw.write(doc_node_html(child, get_src_link(cfg.manifest.repo_url, os.file_name(child.file_path), child.pos.line), false))
			}
		}
	}
	hw.write('\n</div>\n')

	if cfg.is_multi && cfg.docs.len > 1 && dcs.head.name != 'README' {
		hw.write('<div class="doc-toc">\n<p class="section">Contents</p>\n<ul>\n${toc.str()}</ul>\n</div>')
	}

	hw.write('</div>
		<script>
			var toggle = document.getElementById("toggleMenu");
			toggle.addEventListener("click", function (ev) {
				document.querySelectorAll(".toggleable").forEach(function(el) {
					el.classList.toggle("hidden");
					el.classList.toggle("flex");
				})
			});
		</script>
	</body>
	</html>')
	return hw.str()
}

fn (cfg DocConfig) gen_plaintext(idx int) string {
	dcs := cfg.docs[idx]
	mut pw := strings.new_builder(200)

	head_lines := '='.repeat(dcs.head.content.len)
	pw.writeln('${dcs.head.content}\n$head_lines\n')

	for cn in dcs.contents {
		pw.writeln(cn.content)
		if cn.comment.len > 0 {
			pw.writeln('\n' + cn.comment)
		}
		if cfg.show_loc {
			pw.writeln('Location: ${cn.file_path}:${cn.pos.line}:${cn.pos.col}\n\n')
		}
	}

	pw.writeln('Generated on $dcs.time_generated')
	return pw.str()
}

fn (cfg DocConfig) gen_markdown(idx int, with_toc bool) string {
	dcs := cfg.docs[idx]
	mut hw := strings.new_builder(200)
	mut cw := strings.new_builder(200)

	hw.writeln('# ${dcs.head.content}\n${dcs.head.comment}\n')
	if with_toc {
		hw.writeln('## Contents')
	}

	for cn in dcs.contents {
		name := cn.name.all_after(dcs.head.name+'.')

		if with_toc {
			hw.writeln('- [#$name](${slug(name)})')
		}
		cw.writeln('## $name')
		cw.writeln('```v\n${cn.content}\n```${cn.comment}\n')
		cw.writeln('[\[Return to contents\]](#Contents)\n')
	}

	cw.writeln('#### Generated by vdoc. Last generated: ${dcs.time_generated.str()}')
	return hw.str() + '\n' + cw.str()
}

fn (cfg DocConfig) multi_render(output_type OutputType) map[string]string {
	mut docs := map[string]string
	for i, doc in cfg.docs {
		mut name := if doc.head.name == 'README' {
			'index'
		} else if cfg.docs.len == 1 && !os.is_dir(cfg.opath) {
			os.dir(os.file_name(cfg.opath))
		} else {
			doc.head.name
		}
		name = name + match output_type {
			.html { '.html' }
			.markdown { '.md' }
			.json { '.json' }
			else { '.txt' }
		}
		docs[name] = match output_type {
			.html { cfg.gen_html(i) }
			.markdown { cfg.gen_markdown(i, true) }
			.json { cfg.gen_json(i) }
			else { cfg.gen_plaintext(i) }
		}
	}
	return docs
}

fn (mut config DocConfig) generate_docs_from_file() {
	if config.opath.len != 0 {
		config.opath = os.join_path(os.real_path(os.base_dir(config.opath)), os.file_name(config.opath))
	}
	println(config.opath)
	mut output_type := OutputType.plaintext
	// identify output type
	if config.serve_http {
		output_type = .html
	} else if config.opath.len == 0 {
		output_type = .stdout
	} else {
		ext := os.file_ext(config.opath)
		if ext in ['.md', '.markdown'] || config.opath in [':md:', ':markdown:'] {
			output_type = .markdown
		} else if ext in ['.html', '.htm'] || config.opath == ':html:' {
			output_type = .html
		} else if ext == '.json' || config.opath == ':json:' {
			output_type = .json
		} else {
			output_type = .plaintext
		}
	}
	if config.include_readme && output_type != .html {
		eprintln('vdoc: Including README.md for doc generation is supported on HTML output only.')
		exit(1)
	}
	mut manifest_path := os.join_path(if os.is_dir(config.src_path) { config.src_path } else { os.base_dir(config.src_path) }, 'v.mod')
	if os.exists(manifest_path) && 'vlib' !in config.src_path {
		if manifest := vmod.from_file(manifest_path) {
			config.manifest = manifest
		}
	}
	if 'vlib' in config.src_path {
		config.manifest.version = util.v_version
	}
	readme_path := if 'vlib' in config.src_path {
		os.join_path(os.base_dir(@VEXE), 'README.md')
	} else {
		os.join_path(config.src_path, 'README.md')
	}
	// check README.md
	if os.exists(readme_path) && config.include_readme {
		println('Found README.md...')
		readme_contents := os.read_file(readme_path) or { '' }
		config.docs << doc.Doc{
			head: doc.DocNode{
				name: 'README',
				comment: readme_contents
			}
		}
	}
	if config.is_multi {
		dirs := get_modules_list(config.src_path)
		for dirpath in dirs {
			dcs := doc.generate(dirpath, config.pub_only, 'vlib' !in config.src_path) or {
				panic(err)
			}
			config.docs << dcs
		}
	} else {
		dcs := doc.generate(config.src_path, config.pub_only, 'vlib' !in config.src_path) or {
			panic(err)
		}
		config.docs << dcs
	}
	if config.serve_http {
		config.serve_html()
		return
	}
	outputs := config.multi_render(output_type)
	if output_type == .stdout || (config.opath.starts_with(':') && config.opath.ends_with(':')) {
		first := outputs.keys()[0]
		println(outputs[first])
	} else {
		if !os.is_dir(config.opath) {
			config.opath = os.base_dir(config.opath)
		}
		if config.is_multi {
			config.opath = os.join_path(config.opath, '_docs')
			if !os.exists(config.opath) {
				os.mkdir(config.opath) or {
					panic(err)
				}
			}
		}
		for file_name, content in outputs {
			opath := os.join_path(config.opath, file_name)
			println('Generating ${opath}...')
			os.write_file(opath, content)
		}
	}
}

fn lookup_module(mod string) ?string {
	mod_path := mod.replace('.', '/')
	vexe_path := os.base_dir(@VEXE)
	compile_dir := os.real_path(os.base_dir('.'))
	modules_dir := os.join_path(compile_dir, 'modules', mod_path)
	vlib_path := os.join_path(vexe_path, 'vlib', mod_path)
	vmodules_path := os.join_path(os.home_dir(), '.vmodules', mod_path)
	paths := [modules_dir, vlib_path, vmodules_path]
	for path in paths {
		if os.is_dir_empty(path) { continue }
		return path
	}
	return error('vdoc: Module "${mod}" not found.')
}

fn get_modules_list(path string) []string {
	files := os.walk_ext(path, 'v')
	mut dirs := []string{}

	for file in files {
		if 'test' in file || 'bare' in file || 'uiold' in file || 'vweb' in file { continue }
		dirname := os.base_dir(file)
		if dirname in dirs { continue }
		dirs << dirname
	}

	dirs.sort()

	return dirs
}

fn main() {
	args_after_doc := cmdline.options_after(os.args[1..], ['doc'])
	opts := cmdline.only_options(os.args[1..])
	args := cmdline.only_non_options(args_after_doc)
	if args.len == 0 || args[0] == 'help' {
		os.system('v help doc')
		exit(0)
	}
	mut config := DocConfig{
		src_path: args[0],
		opath: if args.len >= 2 { args[1] } else { '' },
		pub_only: '-all' !in opts,
		show_loc: '-loc' in opts,
		serve_http: '-serve' in opts,
		is_multi: '-multi' in opts,
		include_readme: '-include-readme' in opts,
		manifest: vmod.Manifest{ repo_url: '' }
	}
	is_path := config.src_path.ends_with('.v') || config.src_path.split('/').len > 1 || config.src_path == '.'
	if !is_path {
		mod_path := lookup_module(config.src_path) or {
			eprintln(err)
			exit(1)
		}
		config.src_path = mod_path
	}
	config.generate_docs_from_file()
}
