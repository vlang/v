module main

import strings
import document as doc

fn (vd &VDoc) gen_markdown(d doc.Doc, with_toc bool) string {
	cfg := vd.cfg
	mut hw := strings.new_builder(200)
	mut cw := strings.new_builder(200)
	hw.writeln('# ${d.head.content}')
	if d.head.comments.len > 0 && cfg.include_comments {
		comments := if vd.cfg.include_examples {
			d.head.merge_comments()
		} else {
			d.head.merge_comments_without_examples()
		}
		hw.writeln('${comments}')
	}
	hw.writeln('\n')
	if with_toc {
		hw.writeln('## Contents')
	}
	vd.write_markdown_content(d.contents.arr(), mut cw, mut hw, 0, with_toc)
	footer_text := gen_footer_text(d, !vd.cfg.no_timestamp)
	cw.writeln('#### ${footer_text}')
	return hw.str() + '\n' + cw.str()
}

fn (vd &VDoc) write_markdown_content(contents []doc.DocNode, mut cw strings.Builder, mut hw strings.Builder,
	indent int, with_toc bool) {
	for cn in contents {
		if with_toc && cn.name != '' {
			hw.writeln(' '.repeat(2 * indent) + '- [${slug(cn.name)}](#${cn.name})')
			cw.writeln('## ${cn.name}')
		}
		if cn.content.len > 0 {
			cw.writeln('```v\n${cn.content}\n```\n')
			if cn.comments.len > 0 {
				comments := cn.merge_comments_without_examples()
				cw.writeln('${comments}\n')
			}
			// Write examples if any found
			examples := cn.examples()
			if vd.cfg.include_examples && examples.len > 0 {
				example_title := if examples.len > 1 { 'Examples' } else { 'Example' }
				cw.writeln('${example_title}\n```v\n')
				for example in examples {
					cw.writeln('${example}\n')
				}
				cw.writeln('```\n')
			}
			cw.writeln(r'[[Return to contents]](#Contents)')
			cw.writeln('')
		}
		vd.write_markdown_content(cn.children, mut cw, mut hw, indent + 1, with_toc)
	}
}
