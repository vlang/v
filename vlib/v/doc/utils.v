module doc

import strings
import v.ast
import v.util
import v.token
import v.table
import os

// merge_comments merges all the comment contents into a single text.
pub fn merge_comments(comments []ast.Comment) string {
	mut res := []string{}
	for comment in comments {
		res << comment.text.trim_left('\x01')
	}
	return res.join('\n')
}

// get_comment_block_right_before merges all the comments starting from
// the last up to the first item of the array.
pub fn get_comment_block_right_before(comments []ast.Comment) string {
	if comments.len == 0 {
		return ''
	}
	mut comment := ''
	mut last_comment_line_nr := 0
	for i := comments.len - 1; i >= 0; i-- {
		cmt := comments[i]
		if last_comment_line_nr != 0 && cmt.pos.line_nr < last_comment_line_nr - 1 {
			// skip comments that are not part of a continuous block,
			// located right above the top level statement.
			// break
		}
		mut cmt_content := cmt.text.trim_left('\x01')
		if cmt_content.len == cmt.text.len || cmt.is_multi {
			// ignore /* */ style comments for now
			continue
			// if cmt_content.len == 0 {
			// continue
			// }
			// mut new_cmt_content := ''
			// mut is_codeblock := false
			// // println(cmt_content)
			// lines := cmt_content.split_into_lines()
			// for j, line in lines {
			// trimmed := line.trim_space().trim_left(cmt_prefix)
			// if trimmed.starts_with('- ') || (trimmed.len >= 2 && trimmed[0].is_digit() && trimmed[1] == `.`) || is_codeblock {
			// new_cmt_content += line + '\n'
			// } else if line.starts_with('```') {
			// is_codeblock = !is_codeblock
			// new_cmt_content += line + '\n'
			// } else {
			// new_cmt_content += trimmed + '\n'
			// }
			// }
			// return new_cmt_content
		}
		// eprintln('cmt: $cmt')
		cseparator := if cmt_content.starts_with('```') { '\n' } else { ' ' }
		comment = cmt_content + cseparator + comment
		last_comment_line_nr = cmt.pos.line_nr
	}
	return comment
}

// convert_pos converts the `token.Position` data into a `DocPos`.
fn (mut d Doc) convert_pos(filename string, pos token.Position) DocPos {
	if filename !in d.sources {
		d.sources[filename] = util.read_file(os.join_path(d.base_path, filename)) or { '' }
	}
	source := d.sources[filename]
	mut p := util.imax(0, util.imin(source.len - 1, pos.pos))
	column := util.imax(0, pos.pos - p - 1)
	return DocPos{
		line: pos.line_nr + 1
		col: util.imax(1, column + 1)
		len: pos.len
	}
}

// stmt_signature returns the signature of a given `ast.Stmt` node.
pub fn (mut d Doc) stmt_signature(stmt ast.Stmt) string {
	match stmt {
		ast.Module {
			return 'module $stmt.name'
		}
		ast.FnDecl {
			return stmt.stringify(d.table, d.fmt.cur_mod, d.fmt.mod2alias)
		}
		else {
			d.fmt.out = strings.new_builder(1000)
			d.fmt.stmt(stmt)
			return d.fmt.out.str().trim_space()
		}
	}
}

// stmt_name returns the name of a given `ast.Stmt` node.
pub fn (d Doc) stmt_name(stmt ast.Stmt) string {
	match stmt {
		ast.FnDecl, ast.StructDecl, ast.EnumDecl, ast.InterfaceDecl {
			return stmt.name
		}
		ast.TypeDecl {
			match stmt {
				ast.FnTypeDecl, ast.AliasTypeDecl, ast.SumTypeDecl { return stmt.name }
			}
		}
		ast.ConstDecl {
			return ''
		} // leave it blank
		else {
			return ''
		}
	}
}

// stmt_pub returns a boolean if a given `ast.Stmt` node
// is exposed to the public.
pub fn (d Doc) stmt_pub(stmt ast.Stmt) bool {
	match stmt {
		ast.FnDecl, ast.StructDecl, ast.EnumDecl, ast.InterfaceDecl, ast.ConstDecl {
			return stmt.is_pub
		}
		ast.TypeDecl {
			match stmt {
				ast.FnTypeDecl, ast.AliasTypeDecl, ast.SumTypeDecl { return stmt.is_pub }
			}
		}
		else {
			return false
		}
	}
}

// type_to_str is a wrapper function around `fmt.table.type_to_str`.
pub fn (d Doc) type_to_str(typ table.Type) string {
	return d.fmt.table.type_to_str(typ).all_after('&')
}

// expr_typ_to_string has the same function as `Doc.typ_to_str`
// but for `ast.Expr` nodes. The checker will check first the
// node and it executes the `type_to_str` method.
pub fn (mut d Doc) expr_typ_to_string(ex ast.Expr) string {
	expr_typ := d.checker.expr(ex)
	return d.type_to_str(expr_typ)
}
