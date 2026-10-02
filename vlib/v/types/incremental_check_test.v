module types

import strings
import v.token

fn incremental_test_entry(key string, hash u64) IncrementalEntry {
	return IncrementalEntry{
		key:       key
		hash:      hash
		length:    120
		range_len: 34
		reusable:  true
		generic:   hash % 2 == 1
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
		incremental_test_entry('0\tfirst', 0xabcdee),
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
	// Whether the check of the body found something generic in it.
	assert stored.generic
	assert !record.entries[0].generic
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

fn test_a_record_reads_back_the_errors_of_the_instances_after_its_entries() {
	text := incremental_test_record('first.v\n', [incremental_test_entry('0\tfirst', 1)])
	// A record without them reads as ever, and they are not known.
	record := decode_incremental_record(text) or { panic('the record reads as none') }
	assert !record.instances_known
	errors := [
		IncrementalInstanceError{
			key: '0\tfirst'
			d:   IncrementalDiagnostic{
				list:     incremental_errors
				kind:     int(TypeErrorKind.unknown_field)
				offset:   30
				end:      33
				meta:     2
				order:    1
				severity: 'error:'
				msg:      'a message with a tab\there,\na newline and a \\ backslash'
				details:  ['a detail', 'another\tone']
			}
		},
		IncrementalInstanceError{
			key: '3\tBox.str'
			d:   IncrementalDiagnostic{
				list:   incremental_errors
				kind:   int(TypeErrorKind.unknown_field)
				offset: 5
				end:    9
				msg:    'no details'
			}
		},
	]
	mut b := strings.new_builder(512)
	b.write_string(text)
	b.writeln('instances\t${errors.len}')
	for e in errors {
		write_incremental_instance_error(mut b, e.key, e.d)
	}
	with := b.str()
	again := decode_incremental_record(with) or { panic('the record with them reads as none') }
	assert again.instances_known
	assert again.instances == errors
	assert again.entries.len == 1
	assert again.details(0) == record.details(0)
	// They are not known when their lines are not all there, nor when a line
	// follows them; the entries are read all the same.
	second := with.index('\ni\t3\t') or { panic('no second error') }
	for cut in [with.len - 1, second + 1, text.len + 5] {
		cut_short := decode_incremental_record(with[..cut]) or {
			panic('cut at ${cut}: the record reads as none')
		}
		assert !cut_short.instances_known, 'cut at ${cut}'
		assert cut_short.entries.len == 1
	}
	after := decode_incremental_record(with + 'i\t0\tfirst\n') or {
		panic('the record reads as none')
	}
	assert !after.instances_known
	// A check whose instances had no errors noted that.
	none_found := decode_incremental_record(text + 'instances\t0\n') or {
		panic('the record reads as none')
	}
	assert none_found.instances_known
	assert none_found.instances.len == 0
}

// incremental_test_check is what an incremental check keeps once the check of
// the instances began, after an error of its own: the regions of two bodies of
// the file 1 and of one of the file 2.
fn incremental_test_check() &IncrementalCheck {
	return &IncrementalCheck{
		selected:        true
		instances_start: 1
		functions:       [
			IncrementalFunction{
				item:    CheckWorkItem{
					fn_idx:   40
					range_lo: 20
					file:     'main.v'
				}
				key:     '0\tshow'
				file_id: 1
				start:   100
				end:     200
			},
			IncrementalFunction{
				item:    CheckWorkItem{
					fn_idx:   60
					range_lo: 41
					file:     'main.v'
				}
				key:     '0\tcaller'
				file_id: 1
				start:   200
				end:     260
			},
			IncrementalFunction{
				item:    CheckWorkItem{
					fn_idx:   90
					range_lo: 61
					file:     'box.v'
				}
				key:     '1\tBox.str'
				file_id: 2
				start:   0
				end:     80
			},
		]
	}
}

fn incremental_test_instance_error(file string, id int, offset int, end int) TypeError {
	return TypeError{
		msg:              'type `User` has no field named `nme`'
		kind:             .unknown_field
		file:             file
		pos:              token.Pos{
			offset: i32(offset)
			end:    i32(end)
			id:     i32(id)
			meta:   3
		}
		severity:         'error:'
		details:          ['in the instance\tshow[User]']
		diagnostic_order: 7
	}
}

fn test_the_errors_of_the_instances_are_noted_in_the_regions_of_their_bodies() {
	record := incremental_test_record('main.v\nbox.v\n', [
		incremental_test_entry('0\tshow', 2),
	])
	tc := TypeChecker{
		incremental: incremental_test_check()
		errors:      [
			TypeError{
				msg: 'an error before the check of the instances'
			},
			incremental_test_instance_error('main.v', 1, 130, 133),
			incremental_test_instance_error('box.v', 2, 10, 20),
		]
	}
	text := tc.incremental_instances_record(record)
	assert text.starts_with(record)
	again := decode_incremental_record(text) or { panic('the record reads as none') }
	assert again.instances_known
	assert again.instances == [
		IncrementalInstanceError{
			key: '0\tshow'
			d:   IncrementalDiagnostic{
				list:     incremental_errors
				kind:     int(TypeErrorKind.unknown_field)
				offset:   30
				end:      33
				meta:     3
				order:    7
				severity: 'error:'
				msg:      'type `User` has no field named `nme`'
				details:  ['in the instance\tshow[User]']
			}
		},
		IncrementalInstanceError{
			key: '1\tBox.str'
			d:   IncrementalDiagnostic{
				list:     incremental_errors
				kind:     int(TypeErrorKind.unknown_field)
				offset:   10
				end:      20
				meta:     3
				order:    7
				severity: 'error:'
				msg:      'type `User` has no field named `nme`'
				details:  ['in the instance\tshow[User]']
			}
		},
	]
}

fn test_the_errors_of_the_instances_are_not_noted_when_one_is_in_no_region_of_a_body() {
	record := incremental_test_record('main.v\n', [incremental_test_entry('0\tshow', 2)])
	for err in [
		// Across two regions.
		incremental_test_instance_error('main.v', 1, 190, 210),
		// In the region of a body of another file.
		incremental_test_instance_error('other.v', 1, 130, 133),
		// With no place.
		incremental_test_instance_error('main.v', 0, 130, 133),
		// With a detail that names a place, which does not move with it.
		TypeError{
			...incremental_test_instance_error('main.v', 1, 130, 133)
			details: ['main.v:3:5: here']
		},
	] {
		tc := TypeChecker{
			incremental: incremental_test_check()
			errors:      [TypeError{}, err]
		}
		assert tc.incremental_instances_record(record) == '', '${err}'
	}
	// Nor when their check did not begin.
	mut state := incremental_test_check()
	state.instances_start = -1
	tc := TypeChecker{
		incremental: state
	}
	assert tc.incremental_instances_record(record) == ''
}
