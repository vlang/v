module main

fn (cfg DocConfig) gen_markdown(idx int, with_toc bool) string {
	dcs := cfg.docs[idx]
	mut hw := strings.new_builder(200)
	mut cw := strings.new_builder(200)
	hw.writeln('# $dcs.head.content\n')
	if dcs.head.comments.len > 0 {
		comments := if cfg.include_examples {
			dcs.head.merge_comments()
		} else {
			dcs.head.merge_comments_without_examples()
		}
		hw.writeln('$comments\n')
	}
	if with_toc {
		hw.writeln('## Contents')
	}
	cfg.write_markdown_content(dcs.contents.arr(), mut cw, mut hw, 0, with_toc)
	footer_text := cfg.gen_footer_text(idx)
	cw.writeln('#### $footer_text')
	return hw.str() + '\n' + cw.str()
}

fn (cfg DocConfig) write_markdown_content(contents []doc.DocNode, mut cw strings.Builder, mut hw strings.Builder, indent int, with_toc bool) {
	for cn in contents {
		if with_toc && cn.name.len > 0 {
			hw.writeln(' '.repeat(2 * indent) + '- [#$cn.name](${slug(cn.name)})')
			cw.writeln('## $cn.name')
		}
		if cn.content.len > 0 {
			comments := cn.merge_comments_without_examples()
			cw.writeln('```v\n$cn.content\n```\n$comments\n')
			// Write examples if any found
			examples := cn.examples()
			if cfg.include_examples && examples.len > 0 {
				example_title := if examples.len > 1 { 'Examples' } else { 'Example' }
				cw.writeln('$example_title\n```v\n')
				for example in examples {
					cw.writeln('$example\n')
				}
				cw.writeln('```\n')
			}
			cw.writeln('[\[Return to contents\]](#Contents)\n')
		}
		cfg.write_markdown_content(cn.children, mut cw, mut hw, indent + 1, with_toc)
	}
}
