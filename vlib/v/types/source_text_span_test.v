module types

import v.flat
import v.token

struct SourceTextSpanCase {
	start    int
	end      int
	expected string
}

fn test_source_text_span_clamps_offsets_and_preserves_empty_span_fallbacks() {
	source := '\t\v\f\r\n  value \t\v\f\r\n'
	cases := [
		SourceTextSpanCase{ start: -4, end: source.len + 4, expected: 'value' },
		SourceTextSpanCase{ start: 0, end: 7, expected: '' },
		SourceTextSpanCase{ start: 7, end: 12, expected: 'value' },
		SourceTextSpanCase{ start: source.len, end: source.len + 5, expected: 'fallback' },
		SourceTextSpanCase{ start: 7, end: 4, expected: 'fallback' },
	]
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('span.v', source.len)
	mut ids := []flat.NodeId{}
	for item in cases {
		ids << a.add_node(flat.Node{
			kind:  .ident
			value: 'fallback'
			pos:   token.new_span(1, item.start, item.end)
		})
	}
	mut tc := TypeChecker.new(&a)
	tc.source_texts_by_file['span.v'] = source
	for i, item in cases {
		assert tc.source_text_for_node(ids[i]) == item.expected
	}
	span := tc.source_text_for_node(ids[0])
	assert usize(span.str) == usize(source.str) + 7
	assert tc.source_text_for_node(flat.empty_node) == ''
	tc.source_texts_by_file.delete('span.v')
	assert tc.source_text_for_node(ids[0]) == 'fallback'
	a.source_files.delete(1)
	assert tc.source_text_for_node(ids[0]) == 'fallback'
}

fn test_source_text_span_preserves_bytes_outside_trim_space_cutset() {
	for source in [' \x85value\xa0 ', ' \x00value\x00 ', ' value \t value '] {
		mut a := flat.FlatAst.new()
		mut files := token.FileSet.new()
		a.source_files[1] = files.add_file('bytes.v', source.len)
		id := a.add_node(flat.Node{
			kind: .ident
			pos:  token.new_span(1, 0, source.len)
		})
		mut tc := TypeChecker.new(&a)
		tc.source_texts_by_file['bytes.v'] = source
		span := tc.source_text_for_node(id)
		assert span == source.trim_space()
		assert usize(span.str) == usize(source.str) + 1
	}
}
