module parser

// Byte classes of the source skimmers: loops that skip source text up to the
// next byte that their scanner has to see (block braces, the openers of comments
// and literals, directives), and check the bytes that matter only in some
// contexts. A table lookup per byte replaces a chain of comparisons per byte.
const skim_stop = u8(1)
// `r` and `c` open a raw or C string literal when a quote follows.
const skim_quote_prefix = u8(2)
// A byte of an identifier.
const skim_word = u8(4)
// `s` and `u` can start `struct` and `union`.
const skim_struct_start = u8(8)
const skim_union_start = u8(16)

// local_type_skim_classes classes the bytes that index_local_type_declarations skips.
const local_type_skim_classes = new_local_type_skim_classes()
// comptime_block_skim_classes classes the bytes that skip_parallel_comptime_block skips.
const comptime_block_skim_classes = new_block_skim_classes()
// const_name_skim_classes classes the bytes that scan_parallel_const_name_token skips.
const const_name_skim_classes = new_const_name_skim_classes()

fn new_block_skim_classes() [256]u8 {
	mut classes := [256]u8{}
	for c in [u8(`{`), `}`, `/`, `'`, `"`, `\``, `#`] {
		classes[c] = skim_stop
	}
	classes[u8(`r`)] = skim_quote_prefix
	classes[u8(`c`)] = skim_quote_prefix
	return classes
}

fn new_local_type_skim_classes() [256]u8 {
	mut classes := new_block_skim_classes()
	classes[u8(`s`)] = skim_struct_start
	classes[u8(`u`)] = skim_union_start
	return classes
}

fn new_const_name_skim_classes() [256]u8 {
	mut classes := [256]u8{}
	for i in 0 .. 256 {
		c := u8(i)
		if c >= 0x80 {
			classes[i] = skim_stop
		} else if c.is_alnum() || c == `_` {
			classes[i] = skim_word
		}
	}
	for c in [u8(`c`), `m`] {
		classes[c] = skim_stop | skim_word
	}
	for c in [u8(`/`), `'`, `"`, `\``, `#`, `@`] {
		classes[c] = skim_stop
	}
	classes[u8(`r`)] = skim_quote_prefix | skim_word
	return classes
}
