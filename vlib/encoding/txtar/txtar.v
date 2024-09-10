module txtar

// Ported from https://cs.opensource.google/go/x/tools/+/master:txtar/archive.go
import strings

// Archive is a collection of files
pub struct Archive {
pub mut:
	comment string // the start of the archive; contains potentially multiple lines, before the files
	files   []File // a series of files
}

// File is a single file in an Archive. Each starting with a `-- FILENAME --` line.
pub struct File {
pub mut:
	path    string // 'abc/def.v' from the `-- abc/def.v --` header
	content string // everything after that, till the next `-- name --` line.
}

// str returns a string representation of the `a` Archive.
// It is suitable for storing in a text file.
// It is also in the same format, that txtar.parse/1 expects.
pub fn (a &Archive) str() string {
	mut sb := strings.new_builder(a.comment.len + 200 * a.files.len)
	sb.write_string(fix_nl(a.comment))
	for f in a.files {
		sb.write_string('-- ${f.path} --\n')
		sb.write_string(fix_nl(f.content))
	}
	return sb.str()
}

// parse parses the serialized form of an Archive.
// The returned Archive holds slices of data.
pub fn parse(content string) Archive {
	mut a := Archive{}
	comment, mut name, mut data := find_file_marker(content)
	a.comment = comment
	for name != '' {
		mut f := File{name, ''}
		f.content, name, data = find_file_marker(data)
		a.files << f
	}
	return a
}

const nlm = '\n-- '
const mstart = '-- '
const mend = ' --'

// find_file_marker finds the next file marker in data, extracts the file name,
// and returns the data before the marker, the file name, and the data after the marker.
// If there is no next marker, find_file_marker returns fixNL(data), '', ''.
fn find_file_marker(data string) (string, string, string) {
	mut i := 0
	for i < data.len {
		name, after := is_marker(data[i..])
		if name != '' {
			return data[..i], name, after
		}
		j := data[i..].index(nlm) or { return fix_nl(data), '', '' }
		i += j + 1 // positioned at start of new possible marker
	}
	return '', '', ''
}

// is_marker checks whether the data begins with a file marker line.
// If so, it returns the name from the line, and the data after the line.
// Otherwise it returns name == "".
fn is_marker(data string) (string, string) {
	if !data.starts_with(mstart) {
		return '', ''
	}
	mut ndata := data
	mut after := ''
	i := data.index_u8(`\n`)
	if i >= 0 {
		ndata, after = data[..i], data[i + 1..]
	}
	if !(ndata.ends_with(mend) && ndata.len >= mstart.len + mend.len) {
		return '', ''
	}
	name := ndata[mstart.len..ndata.len - mend.len].trim_space()
	return name, after
}

// fix_nl returns the data, if it is empty, or if it ends in \n.
// Otherwise it returns data + a final \n added.
fn fix_nl(data string) string {
	if data.len == 0 || data[data.len - 1] == `\n` {
		return data
	}
	return '${data}\n'
}
