module main

import os

const inner_block_address_source = "@[noinline]
fn inner_block_addresses() i64 {
	mut values := &[0]
	mut fixed := &[0, 0, 0]!
	mut table := &map[string]int{}
	{
		mut inner_values := [1, 2]
		mut inner_table := {
			'a': 1
		}
		values = &inner_values
		fixed = &[1, 2, 3]!
		table = &inner_table
		inner_values << 3
		inner_table['b'] = 2
	}
	return values.len + (*fixed)[2] + table.len
}

@[noinline]
fn store_cloned_modes(mut saved map[string][]u8, key string) {
	if key.len > 0 {
		existing := saved[key] or { []u8{} }
		mut modes := existing.clone()
		modes << `w`
		saved[key] = modes
	}
}

fn main() {
	assert inner_block_addresses() == 8
	mut saved := map[string][]u8{}
	store_cloned_modes(mut saved, 'k')
	assert saved['k'] == [u8(`w`)]
}
"

fn generated_fn_body(generated string, name string) string {
	mut body := generated
	for {
		body = body.all_after(' ${name}(')
		if body.all_before('\n').ends_with(') {') || body.len == 0 {
			break
		}
	}
	return body.all_after('{\n').all_before('\n}')
}

// An outer variable that keeps the address of a block's local, or of a `&[...]!`
// literal evaluated in the block, must not point to storage that ends with the block
// (AddressSanitizer: stack-use-after-scope in ref_array_init_test.v). A dynamic array
// or a map local holds its header, which `&local` addresses, so it moves to the heap
// like other locals do. A `.clone()` result does not keep its receiver's address.
fn test_inner_block_addresses_kept_by_outer_variables_use_heap_storage() {
	path := os.join_path(os.vtmp_dir(), 'inner_block_address_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, inner_block_address_source)!
	result := os.exec([@VEXE, '-o', output, path])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	body := generated_fn_body(generated, 'inner_block_addresses')
	assert body.contains('Array* inner_values = (Array*)(memdup('), body
	assert body.contains('map* inner_table = (map*)(memdup('), body
	assert body.contains('values = inner_values;'), body
	assert body.contains('table = inner_table;'), body
	assert body.count('memdup(&__heap_value_') == 2, body
	assert !body.contains(' = &__addr_'), body
	modes_body := generated_fn_body(generated, 'store_cloned_modes')
	assert modes_body.contains('Array existing = '), modes_body
	assert modes_body.contains('Array modes = '), modes_body
}
