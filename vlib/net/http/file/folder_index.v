module file

import os
import time
import strings

const myexe = os.executable()
const myexe_prefix = os.file_name(myexe.all_before_last('.'))

fn get_folder_index_html(requested_file_path string, uri_path string, filter_myexe bool) string {
	sw := time.new_stopwatch()
	mut files := os.ls(requested_file_path) or { [] }
	if filter_myexe {
		files = files.filter(!it.contains(myexe_prefix))
	}
	mut sb := strings.new_builder(files.len * 200)
	write_page_header(mut sb, uri_path)
	write_page_crumbs(mut sb, uri_path)
	write_page_table(mut sb, uri_path, requested_file_path, mut files)
	sb.writeln('<p>Server time: <b>${time.now().format_ss()}</b>, generated in <b>${sw.elapsed().microseconds():6}µs</b></p>')
	write_page_footer(mut sb, uri_path)
	return sb.str()
}

fn write_page_header(mut sb strings.Builder, uri_path string) {
	// html boilerplate for the header
	sb.writeln('<!DOCTYPE html>')
	sb.writeln('<html lang="en">')
	sb.writeln('<head>')
	sb.writeln('<meta charset="utf-8">')
	sb.writeln('<title>Index of local folder ${uri_path}</title>')
	sb.writeln('<link rel="shortcut icon" href="data:image/x-icon;," type="image/x-icon" />')
	sb.writeln('</head>')
	sb.writeln('<body>')
}

fn write_page_footer(mut sb strings.Builder, uri_path string) {
	// html boilerplate for the footer
	sb.writeln('</body>')
	sb.writeln('</html>')
}

fn write_page_crumbs(mut sb strings.Builder, uri_path string) {
	crumbs := uri_path.split('/')
	mut crlinks := []string{cap: crumbs.len}
	for cridx, crumb in crumbs {
		cr_so_far := crumbs#[0..cridx + 1].join('/')
		// eprintln('> cr_so_far: ${cr_so_far:20} | crumb: ${crumb:20}')
		crlinks << '<a href="/${cr_so_far}">${crumb}</a>'
	}
	crlinks_text := crlinks.join('&nbsp;/&nbsp;')
	sb.writeln('<h2>Index of <a href="/">/</a>&nbsp;${crlinks_text}&nbsp;:</h2>')
}

fn write_page_table(mut sb strings.Builder, uri_path string, requested_file_path string, mut files []string) {
	files.sort()
	sb.writeln('<table>')
	sb.writeln('<tr>')
	sb.writeln('<th align="left" style="width: 100px">Size</th>')
	sb.writeln('<th align="left" style="width: 200px">Last modified</th>')
	sb.writeln('<th align="left">Name</th>')
	sb.writeln('</tr>')
	if uri_path == '' {
		sb.writeln('<tr>')
		sb.writeln('<td>---</td>')
		sb.writeln('<td>---</td>')
		sb.writeln('<td>---</td>')
		sb.writeln('</tr>')
	} else {
		sb.writeln('<tr>')
		sb.writeln('<td></td>')
		sb.writeln('<td></td>')
		sb.writeln('<td><a href="/${uri_path}/..">..</td>')
		sb.writeln('</tr>')
	}
	mut entities := []Entity{cap: files.len}
	for fname in files {
		path := os.join_path(requested_file_path, fname)
		entities << path_to_entity(path, uri_path)
	}
	entities.sort_with_compare(fn (a &Entity, b &Entity) int {
		if a.typ == b.typ {
			if a.fname < b.fname {
				return -1
			}
			if a.fname > b.fname {
				return 1
			}
			return 0
		}
		if a.typ == .directory {
			return -1
		}
		return 1
	})
	for entity in entities {
		if entity.typ == .directory {
			sb.writeln('<tr>')
			sb.writeln('<td></td>')
			sb.writeln('<td>${entity.mod_time.format_ss()}</td>')
			sb.writeln('<td><a href="${entity.url}">${entity.fname}/</a></td>')
			sb.writeln('</tr>')
		} else if entity.typ == .symbolic_link {
			sb.writeln('<tr>')
			sb.writeln('<td>${entity.size}</td>')
			sb.writeln('<td>${entity.mod_time.format_ss()}</td>')
			sb.writeln('<td><a href="${entity.url}">${entity.fname}@</a></td>')
			sb.writeln('</tr>')
		} else {
			sb.writeln('<tr>')
			sb.writeln('<td>${entity.size}</td>')
			sb.writeln('<td>${entity.mod_time.format_ss()}</td>')
			sb.writeln('<td><a href="${entity.url}">${entity.fname}</a></td>')
			sb.writeln('</tr>')
		}
	}
	sb.writeln('</table>')
}
