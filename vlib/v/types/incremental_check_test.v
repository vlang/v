module types

import strings

fn incremental_test_entry(key string, hash u64) IncrementalEntry {
	return IncrementalEntry{
		key:       key
		hash:      hash
		length:    120
		range_len: 34
		reusable:  true
		details:   IncrementalDetails{
			diagnostics: [
				IncrementalDiagnostic{
					list:     incremental_errors
					kind:     int(TypeErrorKind.call_arg_mismatch)
					back:     3
					offset:   17
					end:      29
					meta:     4
					order:    2
					severity: 'error:'
					msg:      'a message with a tab\there,\na newline and a \\ backslash'
					details:  ['first detail', 'second\tdetail']
				},
				IncrementalDiagnostic{
					list:     incremental_pending
					kind:     int(TypeErrorKind.return_mismatch)
					back:     1
					offset:   40
					end:      41
					fn_qname: 'main.first'
				},
			]
			calls:       [IncrementalName{
				back: 5
				name: 'os.join_path'
			}]
			fn_values:   [IncrementalName{
				back: 7
				name: 'callback'
			}]
		}
	}
}

fn incremental_test_record(files string, entries []IncrementalEntry) string {
	mut b := strings.new_builder(1024)
	b.writeln(incremental_record_header)
	b.writeln('declarations\t${u64(0xfedcba9876543210).hex()}')
	b.writeln('files\t${incremental_escape(files)}')
	for entry in entries {
		write_incremental_entry(mut b, entry)
	}
	b.writeln('end\t${entries.len}')
	return b.str()
}

fn test_a_record_reads_back_what_it_was_written_with() {
	files := 'first.v\nfile with a\ttab.v\n'
	text := incremental_test_record(files, [
		incremental_test_entry('0\tfirst', 0xabcdef),
		incremental_test_entry('1\tPoint.str', 0x1234567890abcdef),
	])
	record := decode_incremental_record(text) or { panic('the record reads as none') }
	assert record.declarations == u64(0xfedcba9876543210)
	assert record.files == files
	assert record.entries.len == 2
	assert record.by_key['0\tfirst'] == 0
	assert record.by_key['1\tPoint.str'] == 1
	stored := record.entries[1]
	assert stored.hash == u64(0x1234567890abcdef)
	assert stored.length == 120
	assert stored.range_len == 34
	assert stored.reusable
	for i in 0 .. 2 {
		details := record.details(i)
		assert details == incremental_test_entry('', 0).details
	}
	// The lines of an entry, copied as they are, are the entry again.
	mut copy := strings.new_builder(256)
	copy.writeln(incremental_record_header)
	copy.writeln('declarations\t0')
	copy.writeln('files\t')
	copy.write_string(text[stored.start..stored.end])
	copy.writeln('end\t1')
	again := decode_incremental_record(copy.str()) or { panic('the copy reads as none') }
	assert again.entries.len == 1
	assert again.entries[0].key == '1\tPoint.str'
	assert again.details(0) == record.details(1)
}

fn test_a_record_cut_short_reads_as_none() {
	text := incremental_test_record('first.v\n', [incremental_test_entry('0\tfirst', 1)])
	for cut in [text.len - 2, text.index('\nf\t') or { 0 }, 5] {
		assert decode_incremental_record(text[..cut]) == none, 'cut at ${cut}'
	}
	assert decode_incremental_record('') == none
}
