interface NilFieldReader {
	read() int
}

type NilFieldReaderAlias = NilFieldReader

@[params]
struct NilFieldConfig {
	reader NilFieldReader
	alias  NilFieldReaderAlias
}

fn check_nil_interface_fields(config NilFieldConfig) {
	assert config.reader.type_idx() == 0
	// Inspect both words to verify the interface also has a null object pointer.
	reader_bytes := unsafe { voidptr(&config.reader).vbytes(int(sizeof(NilFieldReader))) }
	alias_bytes := unsafe { voidptr(&config.alias).vbytes(int(sizeof(NilFieldReaderAlias))) }
	assert reader_bytes.all(it == 0)
	assert alias_bytes.all(it == 0)
}

fn test_interface_fields_are_zeroed_by_unsafe_nil() {
	check_nil_interface_fields(reader: unsafe { nil }, alias: (unsafe { nil }))
	check_nil_interface_fields(NilFieldConfig{
		reader: unsafe { nil }
		alias:  (unsafe { nil })
	})
	unsafe {
		check_nil_interface_fields(reader: nil, alias: nil)
		check_nil_interface_fields(NilFieldConfig{ reader: nil, alias: nil })
	}
}

fn test_parenthesized_nil_interface_fields_preserve_block_effects() {
	mut calls := 0
	check_nil_interface_fields(
		reader: (unsafe {
			calls++
			nil
		})
		alias:  (unsafe {
			calls++
			nil
		})
	)
	assert calls == 2
	check_nil_interface_fields(NilFieldConfig{
		reader: (unsafe {
			calls++
			nil
		})
		alias:  (unsafe {
			calls++
			nil
		})
	})
	assert calls == 4
}
