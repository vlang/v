module brotli

import compress.huffman

fn write_window_bits(mut w BitWriter, window_size int, large_window bool) {
	if window_size == 16 && !large_window {
		w.write_bit(0)
		return
	}
	w.write_bit(1)
	if window_size == 17 && !large_window {
		w.write_bits(0, 3)
		w.write_bits(0, 3)
		return
	}
	if large_window {
		w.write_bits(0, 3)
		w.write_bits(1, 3)
		w.write_bit(0)
		w.write_bits(u32(window_size), 6)
		return
	}
	w.write_bits(u32(window_size - 17), 3)
}

fn write_metadata_block(mut w BitWriter, data []u8) {
	w.write_bit(0)
	w.write_bits(3, 2)
	w.write_bit(0)
	mskip_bytes := if data.len == 0 {
		0
	} else if data.len - 1 <= 0xff {
		1
	} else if data.len - 1 <= 0xffff {
		2
	} else {
		3
	}
	w.write_bits(u32(mskip_bytes), 2)
	if mskip_bytes > 0 {
		w.write_bits(u32(data.len - 1), mskip_bytes * 8)
	}
	w.align_byte()
	w.write_bytes(data)
}

fn write_uncompressed_meta_block(mut w BitWriter, data []u8, is_last bool) {
	mnibbles := meta_block_nibbles(data.len)
	w.write_bit(0) // ISLAST (uncompressed blocks are never final)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		6 { 2 }
		else { 3 }
	}), 2)
	write_meta_block_length(mut w, data.len, mnibbles)
	w.write_bit(1) // ISUNCOMPRESSED
	w.write_bytes(data)
	if is_last {
		w.write_bit(1) // ISLAST
		w.write_bit(1) // ISEMPTY
		w.align_byte()
	}
}

fn write_simple_prefix_tree(mut w BitWriter, alphabet_size int, symbol int) {
	w.write_bits(1, 2) // simple prefix code
	w.write_bits(0, 2) // one symbol
	w.write_bits(u32(symbol), bit_width(alphabet_size - 1))
}

fn write_simple_prefix_tree_symbols(mut w BitWriter, alphabet_size int, symbols []int) ! {
	if symbols.len == 0 || symbols.len > 4 {
		return error('brotli: invalid simple prefix symbol count')
	}
	for sym in symbols {
		if sym < 0 || sym >= alphabet_size {
			return error('brotli: simple prefix symbol out of range')
		}
	}
	w.write_bits(1, 2) // simple prefix code
	w.write_bits(u32(symbols.len - 1), 2)
	alphabet_bits := bit_width(alphabet_size - 1)
	for sym in symbols {
		w.write_bits(u32(sym), alphabet_bits)
	}
	if symbols.len == 4 {
		w.write_bit(0) // tree_select = 0, all symbols have length 2
	}
}

fn write_code_length_value(mut w BitWriter, value int) ! {
	match value {
		0 { w.write_bits(0, 2) }
		1 { w.write_bits(7, 4) }
		2 { w.write_bits(3, 3) }
		3 { w.write_bits(2, 2) }
		4 { w.write_bits(1, 2) }
		5 { w.write_bits(15, 4) }
		else { return error('brotli: invalid code length code') }
	}
}

struct CodeLengthToken {
	symbol int
	extra  int
}

fn append_code_length_tokens(mut tokens []CodeLengthToken, value int, repetitions int, previous_value int,
	use_rle bool) int {
	if !use_rle {
		for _ in 0 .. repetitions {
			tokens << CodeLengthToken{ symbol: value }
		}
		return if value > 0 { value } else { previous_value }
	}
	if value == 0 {
		mut remaining := repetitions
		if remaining == 11 {
			tokens << CodeLengthToken{ symbol: 0 }
			remaining--
		}
		if remaining < 3 {
			for _ in 0 .. remaining {
				tokens << CodeLengthToken{ symbol: 0 }
			}
			return previous_value
		}
		remaining -= 3
		mut encoded := []CodeLengthToken{}
		for {
			encoded << CodeLengthToken{ symbol: 17, extra: remaining & 7 }
			remaining >>= 3
			if remaining == 0 {
				break
			}
			remaining--
		}
		mut index := encoded.len
		for index > 0 {
			index--
			tokens << encoded[index]
		}
		return previous_value
	}
	mut remaining := repetitions
	if previous_value != value {
		tokens << CodeLengthToken{ symbol: value }
		remaining--
	}
	if remaining == 7 {
		tokens << CodeLengthToken{ symbol: value }
		remaining--
	}
	if remaining < 3 {
		for _ in 0 .. remaining {
			tokens << CodeLengthToken{ symbol: value }
		}
	} else {
		remaining -= 3
		mut encoded := []CodeLengthToken{}
		for {
			encoded << CodeLengthToken{ symbol: 16, extra: remaining & 3 }
			remaining >>= 2
			if remaining == 0 {
				break
			}
			remaining--
		}
		mut index := encoded.len
		for index > 0 {
			index--
			tokens << encoded[index]
		}
	}
	return value
}

// write_prefix_tree emits the RFC code-length representation used
// when a simple prefix tree is not sufficient.
fn write_prefix_tree(mut w BitWriter, lengths []int) ! {
	if lengths.len == 0 {
		return error('brotli: invalid alphabet size')
	}
	max_bits := max_length(lengths)
	if max_bits == 0 {
		mut sym := -1
		for i, l in lengths {
			if l < 0 {
				sym = i
				break
			}
		}
		if sym < 0 {
			return error('brotli: empty prefix tree')
		}
		write_simple_prefix_tree(mut w, lengths.len, sym)
		return
	}
	for l in lengths {
		if l < 0 || l > 15 {
			return error('brotli: prefix tree length out of range')
		}
	}
	mut has_symbol := false
	for l in lengths {
		if l > 0 {
			has_symbol = true
			break
		}
	}
	if !has_symbol {
		return error('brotli: empty prefix tree')
	}
	mut tree_length := lengths.len
	for tree_length > 0 && lengths[tree_length - 1] == 0 {
		tree_length--
	}
	mut zero_total := 0
	mut zero_runs := 1
	mut nonzero_total := 0
	mut nonzero_runs := 1
	mut run_start := 0
	for run_start < tree_length {
		value := lengths[run_start]
		mut run_end := run_start + 1
		for run_end < tree_length && lengths[run_end] == value {
			run_end++
		}
		repetitions := run_end - run_start
		if value == 0 && repetitions >= 3 {
			zero_total += repetitions
			zero_runs++
		}
		if value > 0 && repetitions >= 4 {
			nonzero_total += repetitions
			nonzero_runs++
		}
		run_start = run_end
	}
	use_nonzero_rle := tree_length > 50 && nonzero_total > nonzero_runs * 2
	use_zero_rle := tree_length > 50 && zero_total > zero_runs * 2
	mut tokens := []CodeLengthToken{}
	mut previous_value := 8
	run_start = 0
	for run_start < tree_length {
		value := lengths[run_start]
		mut run_end := run_start + 1
		for run_end < tree_length && lengths[run_end] == value {
			run_end++
		}
		previous_value = append_code_length_tokens(mut tokens, value, run_end - run_start, previous_value,
			if value == 0 { use_zero_rle } else { use_nonzero_rle })
		run_start = run_end
	}
	mut token_freqs := []int{len: 18}
	for token in tokens {
		token_freqs[token.symbol]++
	}
	mut used_tokens := 0
	for frequency in token_freqs {
		if frequency > 0 {
			used_tokens++
		}
	}
	if used_tokens == 1 {
		token_freqs[if token_freqs[0] == 0 { 0 } else { 1 }] = 1
	}
	token_lengths := compute_huffman_lengths(token_freqs, 5)!
	mut code_length_space := 32
	mut codes_to_store := code_length_code_order.len
	for codes_to_store > 0 && token_lengths[code_length_code_order[codes_to_store - 1]] == 0 {
		codes_to_store--
	}
	mut skip_some := 0
	if token_lengths[code_length_code_order[0]] == 0 && token_lengths[code_length_code_order[1]] == 0 {
		skip_some = 2
		if token_lengths[code_length_code_order[2]] == 0 {
			skip_some = 3
		}
	}
	w.write_bits(u32(skip_some), 2)
	for index in skip_some .. codes_to_store {
		value := token_lengths[code_length_code_order[index]]
		write_code_length_value(mut w, value)!
		if value > 0 {
			code_length_space -= 32 >> value
			if code_length_space == 0 {
				break
			}
		}
	}
	if code_length_space != 0 {
		return error('brotli: incomplete code-length tree')
	}
	token_tree := huffman.build(
		lengths:   token_lengths
		max_bits:  max_length(token_lengths)
		bit_order: .lsb_first
	) or {
		return error('brotli: failed to build code-length tree')
	}
	mut target_space := 32768
	for length in lengths[..tree_length] {
		if length > 0 {
			target_space -= 32768 >> length
		}
	}
	for token in tokens {
		write_symbol_using_codes(mut w, token_tree.codes, token_tree.lengths, token.symbol)
		if token.symbol == 16 {
			w.write_bits(u32(token.extra), 2)
		} else if token.symbol == 17 {
			w.write_bits(u32(token.extra), 3)
		}
	}
	if target_space != 0 {
		return error('brotli: incomplete prefix tree')
	}
}

fn simple_prefix_lengths(alphabet_size int, symbols []int) ![]int {
	if symbols.len == 0 || symbols.len > 4 {
		return error('brotli: invalid simple prefix symbol count')
	}
	mut lengths := []int{len: alphabet_size}
	match symbols.len {
		1 {
			lengths[symbols[0]] = -1
		}
		2 {
			lengths[symbols[0]] = 1
			lengths[symbols[1]] = 1
		}
		3 {
			lengths[symbols[0]] = 1
			lengths[symbols[1]] = 2
			lengths[symbols[2]] = 2
		}
		4 {
			for sym in symbols {
				lengths[sym] = 2
			}
		}
		else {
			return error('brotli: invalid simple prefix symbol count')
		}
	}

	return lengths
}

fn huffman_node_less(left int, right int, freqs []u64) bool {
	return freqs[left] < freqs[right] || (freqs[left] == freqs[right] && left < right)
}

fn huffman_heap_sift_down(mut heap []int, start int, freqs []u64) {
	mut root := start
	for {
		mut child := root * 2 + 1
		if child >= heap.len {
			return
		}
		if child + 1 < heap.len && huffman_node_less(heap[child + 1], heap[child], freqs) {
			child++
		}
		if huffman_node_less(heap[root], heap[child], freqs) {
			return
		}
		heap[root], heap[child] = heap[child], heap[root]
		root = child
	}
}

fn huffman_heap_pop(mut heap []int, freqs []u64) int {
	value := heap[0]
	last := heap.pop()
	if heap.len > 0 {
		heap[0] = last
		huffman_heap_sift_down(mut heap, 0, freqs)
	}
	return value
}

fn huffman_heap_push(mut heap []int, value int, freqs []u64) {
	heap << value
	mut child := heap.len - 1
	for child > 0 {
		parent := (child - 1) >> 1
		if !huffman_node_less(heap[child], heap[parent], freqs) {
			break
		}
		heap[parent], heap[child] = heap[child], heap[parent]
		child = parent
	}
}

fn limit_huffman_lengths(freqs []int, lengths []int, max_allowed int) ![]int {
	mut counts := []int{len: max_allowed + 1}
	mut overflow := 0
	mut symbols := []int{}
	for symbol, frequency in freqs {
		if frequency <= 0 {
			continue
		}
		symbols << symbol
		length := lengths[symbol]
		if length > max_allowed {
			counts[max_allowed]++
			overflow++
		} else {
			counts[length]++
		}
	}
	for bits := max_allowed - 1; bits > 0 && overflow > 0; bits-- {
		for counts[bits] > 0 && overflow > 0 {
			counts[bits]--
			counts[bits + 1] += 2
			counts[max_allowed]--
			overflow -= 2
		}
	}
	if overflow > 0 {
		return error('brotli: unable to limit huffman lengths')
	}
	for i in 1 .. symbols.len {
		symbol := symbols[i]
		mut insert_at := i
		for insert_at > 0 {
			previous := symbols[insert_at - 1]
			if freqs[previous] > freqs[symbol]
				|| freqs[previous] == freqs[symbol] && previous < symbol {
				break
			}
			symbols[insert_at] = previous
			insert_at--
		}
		symbols[insert_at] = symbol
	}
	mut limited := []int{len: freqs.len}
	mut symbol_index := 0
	for bits in 1 .. counts.len {
		for _ in 0 .. counts[bits] {
			if symbol_index >= symbols.len {
				return error('brotli: invalid limited huffman tree')
			}
			limited[symbols[symbol_index]] = bits
			symbol_index++
		}
	}
	if symbol_index != symbols.len {
		return error('brotli: incomplete limited huffman tree')
	}
	return limited
}

fn compute_huffman_lengths(freqs []int, max_allowed int) ![]int {
	// Build canonical Huffman code lengths using a basic two-way merge tree.
	// Returns per-symbol lengths (0 for absent symbols). For single-symbol cases
	// returns an array with a single negative entry (-1) at that symbol index to
	// signal the single-symbol simple prefix special case used by write_prefix_tree.
	if freqs.len == 0 {
		return error('brotli: empty freqs')
	}
	mut used := []int{cap: freqs.len}
	for i, f in freqs {
		if f > 0 {
			used << i
		}
	}
	if used.len == 0 {
		return []int{len: freqs.len}
	}
	if used.len == 1 {
		mut lengths := []int{len: freqs.len}
		lengths[used[0]] = -1
		return lengths
	}
	leaf_count := used.len
	tree_size := 2 * leaf_count - 1
	mut freq_arr := []u64{len: tree_size}
	mut parent := []int{len: tree_size, init: -1}
	mut sym_of := []int{len: tree_size, init: -1}
	for i in 0 .. leaf_count {
		s := used[i]
		freq_arr[i] = u64(freqs[s])
		sym_of[i] = s
	}
	mut next := leaf_count
	mut active := []int{cap: leaf_count}
	for i in 0 .. leaf_count {
		huffman_heap_push(mut active, i, freq_arr)
	}
	for active.len > 1 {
		min1 := huffman_heap_pop(mut active, freq_arr)
		min2 := huffman_heap_pop(mut active, freq_arr)
		freq_arr[next] = freq_arr[min1] + freq_arr[min2]
		parent[min1] = next
		parent[min2] = next
		active << next
		next++
	}
	root := active[0]
	mut lengths := []int{len: freqs.len}
	for leaf_idx := 0; leaf_idx < leaf_count; leaf_idx++ {
		mut depth := 0
		mut cur := leaf_idx
		for cur != root {
			cur = parent[cur]
			depth++
			if cur == -1 {
				return error('brotli: invalid huffman tree')
			}
		}
		lengths[sym_of[leaf_idx]] = depth
	}
	// Validate max length
	if max_length(lengths) > max_allowed {
		return limit_huffman_lengths(freqs, lengths, max_allowed)
	}
	return lengths
}

fn write_context_map(mut w BitWriter, vals []int, ntrees int) ! {
	if ntrees <= 0 {
		return error('brotli: invalid ntrees for context map')
	}
	// Single-tree fast path
	if ntrees == 1 {
		w.write_bit(0)
		write_simple_prefix_tree(mut w, 1, 0)
		w.write_bit(0)
		return
	}

	// Apply inverse MTF (encoder-side) to produce positions
	mut mtf := []int{len: ntrees, init: index}
	mut pos_seq := []int{}
	for v in vals {
		mut pos := -1
		for i in 0 .. mtf.len {
			if mtf[i] == v {
				pos = i
				break
			}
		}
		if pos == -1 {
			return error('brotli: invalid context map value')
		}
		pos_seq << pos
		for j := pos; j > 0; j-- {
			mtf[j] = mtf[j - 1]
		}
		mtf[0] = v
	}

	// Estimate RLEMAX from runs of zeros in pos_seq
	mut max_run := 0
	mut i := 0
	for i < pos_seq.len {
		if pos_seq[i] == 0 {
			mut run := 1
			mut j := i + 1
			for j < pos_seq.len && pos_seq[j] == 0 {
				run++
				j++
			}
			if run > max_run {
				max_run = run
			}
			i = j
		} else {
			i++
		}
	}
	mut rlemax := 0
	if max_run > 1 {
		rlemax = bit_width(max_run) - 1
		if rlemax > 16 {
			rlemax = 16
		}
		if rlemax < 1 {
			rlemax = 1
		}
	}

	// Emit RLEMAX header
	if rlemax == 0 {
		w.write_bit(0)
	} else {
		w.write_bit(1)
		w.write_bits(u32(rlemax - 1), 4)
	}

	alphabet_size := ntrees + rlemax

	// Build symbol sequence (with extra bits for run symbols)
	mut sym_seq := []int{}
	mut extra_seq := []int{}
	mut extra_bits_seq := []int{}
	i = 0
	for i < pos_seq.len {
		if pos_seq[i] == 0 {
			mut run := 1
			mut j := i + 1
			for j < pos_seq.len && pos_seq[j] == 0 {
				run++
				j++
			}
			if run == 1 {
				sym_seq << 0
				extra_seq << 0
				extra_bits_seq << 0
			} else {
				mut remaining := run
				for remaining > 0 {
					mut k := bit_width(remaining) - 1
					if k > rlemax {
						k = rlemax
					}
					base := 1 << k
					mut extra := remaining - base
					if extra >= base {
						extra = base - 1
					}
					sym_seq << k
					extra_seq << extra
					extra_bits_seq << k
					remaining -= base + extra
				}
			}
			i = j
		} else {
			sym_seq << pos_seq[i] + rlemax
			extra_seq << 0
			extra_bits_seq << 0
			i++
		}
	}

	// Compute symbol frequencies and try building tuned Huffman lengths
	mut freqs := []int{len: alphabet_size}
	for s in sym_seq {
		freqs[s]++
	}
	mut lengths := []int{}
	// try tuned Huffman lengths; fall back to balanced if tree too deep
	lengths = compute_huffman_lengths(freqs, 15) or {
		max_b := bit_width(alphabet_size - 1)
		mut fb := []int{len: alphabet_size}
		for j := 0; j < alphabet_size; j++ {
			fb[j] = max_b
		}
		fb
	}

	max_bits := max_length(lengths)
	write_prefix_tree(mut w, lengths)!
	enc := huffman.build(lengths: lengths, max_bits: max_bits, bit_order: .lsb_first)!

	// Emit symbols and their extra bits
	for k := 0; k < sym_seq.len; k++ {
		sym := sym_seq[k]
		if sym < 0 || sym >= alphabet_size {
			return error('brotli: invalid context map symbol')
		}
		write_symbol_using_codes(mut w, enc.codes, enc.lengths, sym)
		if extra_bits_seq[k] > 0 {
			w.write_bits(u32(extra_seq[k]), extra_bits_seq[k])
		}
	}

	// IMTF = 1 (encoder applied MTF)
	w.write_bit(1)
	return
}

fn collect_small_alphabet_symbols(data []u8, max_symbols int) []int {
	mut seen := [256]bool{}
	mut symbols := []int{}
	for b in data {
		if seen[int(b)] {
			continue
		}
		seen[int(b)] = true
		symbols << int(b)
		if symbols.len > max_symbols {
			return []int{}
		}
	}
	sort_small(mut symbols)
	return symbols
}

fn collect_small_alphabet_symbols_pair(first []u8, second []u8, max_symbols int) []int {
	mut seen := [256]bool{}
	mut symbols := []int{}
	for data in first {
		if !seen[int(data)] {
			seen[int(data)] = true
			symbols << int(data)
			if symbols.len > max_symbols {
				return []int{}
			}
		}
	}
	for data in second {
		if !seen[int(data)] {
			seen[int(data)] = true
			symbols << int(data)
			if symbols.len > max_symbols {
				return []int{}
			}
		}
	}
	sort_small(mut symbols)
	return symbols
}

fn find_literal_only_cmd_symbol_for_insert_len(target int) !int {
	for sym in 0 .. 704 {
		e := cmd_lut(sym)
		if e.copy_len_offset != 2 {
			continue
		}
		if target >= e.insert_len_offset
			&& target < e.insert_len_offset + (1 << e.insert_len_extra_bits) {
			return sym
		}
	}
	return error('brotli: no literal-only command symbol for insert length')
}

fn find_copy_cmd_symbol(insert_target int, copy_target int) !int {
	for sym in 128 .. 704 {
		e := cmd_lut(sym)
		if insert_target < e.insert_len_offset
			|| insert_target >= e.insert_len_offset + (1 << e.insert_len_extra_bits) {
			continue
		}
		if copy_target < e.copy_len_offset
			|| copy_target >= e.copy_len_offset + (1 << e.copy_len_extra_bits) {
			continue
		}
		return sym
	}
	return error('brotli: no command symbol for insert/copy lengths')
}

fn find_smallest_copy_cmd_symbol(insert_target int, copy_target int) !int {
	mut best_sym := -1
	mut best_bits := 1 << 30
	for sym in 128 .. 704 {
		e := cmd_lut(sym)
		if insert_target < e.insert_len_offset
			|| insert_target >= e.insert_len_offset + (1 << e.insert_len_extra_bits)
			|| copy_target < e.copy_len_offset
			|| copy_target >= e.copy_len_offset + (1 << e.copy_len_extra_bits) {
			continue
		}
		bits := e.insert_len_extra_bits + e.copy_len_extra_bits
		if bits < best_bits {
			best_sym = sym
			best_bits = bits
		}
	}
	if best_sym < 0 {
		return error('brotli: no command symbol for insert/copy lengths')
	}
	return best_sym
}

fn write_periodic_pattern_compressed_meta_block_with_symbols_and_cmd(mut w BitWriter, pattern []u8, mlen int, is_last bool, lit_symbols []int, cmd_sym int, mut recent_distances []int) ! {
	if pattern.len <= 0 || pattern.len > 1024 {
		return error('brotli: invalid periodic pattern length')
	}
	if mlen < pattern.len {
		return error('brotli: periodic pattern longer than metablock')
	}
	if lit_symbols.len == 0 {
		return error('brotli: periodic pattern has too many symbols')
	}
	literal_tree_lengths := if lit_symbols.len <= 4 {
		simple_prefix_lengths(256, lit_symbols)!
	} else {
		mut lit_freqs := []int{len: 256}
		for b in pattern {
			lit_freqs[int(b)]++
		}
		lit_lengths := compute_huffman_lengths(lit_freqs, 15) or {
			return error('brotli: failed to build periodic literal tree')
		}
		lit_lengths
	}
	w.write_bit(u8(if is_last { 1 } else { 0 })) // ISLAST
	if is_last {
		w.write_bit(0) // ISLASTEMPTY
	}
	mnibbles := meta_block_nibbles(mlen)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		6 { 2 }
		else { 3 }
	}), 2)
	write_meta_block_length(mut w, mlen, mnibbles)
	if !is_last {
		w.write_bit(0) // ISUNCOMPRESSED = 0
	}

	// Block-type counts for literal, insert-and-copy, and distance categories
	nbltypes_l := 1
	nbltypes_i := 1
	nbltypes_d := 1

	write_varlen_uint8(mut w, nbltypes_l - 1)
	write_varlen_uint8(mut w, nbltypes_i - 1)
	write_varlen_uint8(mut w, nbltypes_d - 1)

	// Optional block type and block count prefix codes (one pair per category) when NBLTYPES >= 2
	// Use simple prefix trees for small test-friendly encodings.
	mut literal_block_count_codes := []u32{}
	mut literal_block_count_lengths := []int{}
	mut literal_block_type_codes := []u32{}
	mut literal_block_type_lengths := []int{}
	if nbltypes_l >= 2 {
		// write block-type prefix tree (alphabet = NBLTYPES + 2) and block-count prefix (alphabet = 26)
		write_simple_prefix_tree_symbols(mut w, nbltypes_l + 2, [0, 1, 2, 3])!
		write_simple_prefix_tree_symbols(mut w, 26, [0])!
		literal_block_type_lengths = simple_prefix_lengths(nbltypes_l + 2, [0, 1, 2, 3])!
		literal_block_count_lengths = simple_prefix_lengths(26, [0])!
		if max_length(literal_block_type_lengths) > 0 {
			enc_lt := huffman.build(
				lengths:   literal_block_type_lengths
				max_bits:  max_length(literal_block_type_lengths)
				bit_order: .lsb_first
			)!
			literal_block_type_codes = enc_lt.codes.clone()
		}
		if max_length(literal_block_count_lengths) > 0 {
			enc_l := huffman.build(
				lengths:   literal_block_count_lengths
				max_bits:  max_length(literal_block_count_lengths)
				bit_order: .lsb_first
			)!
			literal_block_count_codes = enc_l.codes.clone()
		}
	}

	mut insert_block_count_codes := []u32{}
	mut insert_block_count_lengths := []int{}
	mut insert_block_type_codes := []u32{}
	mut insert_block_type_lengths := []int{}
	if nbltypes_i >= 2 {
		write_simple_prefix_tree_symbols(mut w, nbltypes_i + 2, [0, 1, 2, 3])!
		write_simple_prefix_tree_symbols(mut w, 26, [0])!
		insert_block_type_lengths = simple_prefix_lengths(nbltypes_i + 2, [0, 1, 2, 3])!
		insert_block_count_lengths = simple_prefix_lengths(26, [0])!
		if max_length(insert_block_type_lengths) > 0 {
			enc_it := huffman.build(
				lengths:   insert_block_type_lengths
				max_bits:  max_length(insert_block_type_lengths)
				bit_order: .lsb_first
			)!
			insert_block_type_codes = enc_it.codes.clone()
		}
		if max_length(insert_block_count_lengths) > 0 {
			enc_i := huffman.build(
				lengths:   insert_block_count_lengths
				max_bits:  max_length(insert_block_count_lengths)
				bit_order: .lsb_first
			)!
			insert_block_count_codes = enc_i.codes.clone()
		}
	}

	mut distance_block_count_codes := []u32{}
	mut distance_block_count_lengths := []int{}
	mut distance_block_type_codes := []u32{}
	mut distance_block_type_lengths := []int{}
	if nbltypes_d >= 2 {
		write_simple_prefix_tree_symbols(mut w, nbltypes_d + 2, [0, 1, 2, 3])!
		write_simple_prefix_tree_symbols(mut w, 26, [0])!
		distance_block_type_lengths = simple_prefix_lengths(nbltypes_d + 2, [0, 1, 2, 3])!
		distance_block_count_lengths = simple_prefix_lengths(26, [0])!
		if max_length(distance_block_type_lengths) > 0 {
			enc_dt := huffman.build(
				lengths:   distance_block_type_lengths
				max_bits:  max_length(distance_block_type_lengths)
				bit_order: .lsb_first
			)!
			distance_block_type_codes = enc_dt.codes.clone()
		}
		if max_length(distance_block_count_lengths) > 0 {
			enc_d := huffman.build(
				lengths:   distance_block_count_lengths
				max_bits:  max_length(distance_block_count_lengths)
				bit_order: .lsb_first
			)!
			distance_block_count_codes = enc_d.codes.clone()
		}
	}

	// Initial block counts for categories with NBLTYPES >= 2
	if nbltypes_l >= 2 {
		write_block_count_using_enc(mut w, literal_block_count_codes, literal_block_count_lengths, 1)!
	}
	if nbltypes_i >= 2 {
		// periodic pattern writer emits a single insert-and-copy command, so initial count = 1
		write_block_count_using_enc(mut w, insert_block_count_codes, insert_block_count_lengths, 1)!
	}
	if nbltypes_d >= 2 {
		write_block_count_using_enc(mut w, distance_block_count_codes, distance_block_count_lengths, 1)!
	}

	// Distance parameters
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0

	// literal context modes (2 bits per literal block type)
	for _ in 0 .. nbltypes_l {
		w.write_bits(0, 2)
	}

	// One literal tree is sufficient because the periodic writer emits the same
	// alphabet for every literal context.
	ntrees_l := 1
	mut ntrees_d := 1

	write_varlen_uint8(mut w, ntrees_l - 1)
	write_varlen_uint8(mut w, ntrees_d - 1)

	// Write literal context map when multiple literal trees
	if ntrees_l >= 2 {
		mut literal_context_map := []int{len: 64 * nbltypes_l}
		for i in 0 .. 64 * nbltypes_l {
			literal_context_map[i] = i % ntrees_l
		}
		write_context_map(mut w, literal_context_map, ntrees_l)!
	}
	// Write distance context map when multiple distance trees
	if ntrees_d >= 2 {
		mut distance_context_map := []int{len: 4 * nbltypes_d}
		for i in 0 .. 4 * nbltypes_d {
			distance_context_map[i] = i % ntrees_d
		}
		write_context_map(mut w, distance_context_map, ntrees_d)!
	}

	mut dist_code := DistanceCode{}
	if mlen > pattern.len {
		dist_code = encode_distance_code_with_recent(pattern.len, recent_distances, 0, 0)!
	}

	// Prefix codes: NTREESL literals, insert-and-copy (one per insert block type), NTREESD distances
	for _ in 0 .. ntrees_l {
		if lit_symbols.len <= 4 {
			write_simple_prefix_tree_symbols(mut w, 256, lit_symbols)!
		} else {
			write_prefix_tree(mut w, literal_tree_lengths)!
		}
	}
	for _ in 0 .. nbltypes_i {
		write_simple_prefix_tree(mut w, 704, cmd_sym)
	}
	for _ in 0 .. ntrees_d {
		if mlen == pattern.len {
			write_simple_prefix_tree(mut w, 64, 0)
		} else {
			write_simple_prefix_tree(mut w, 64, dist_code.symbol)
		}
	}

	// Block switching state: prev=1, curr=0 per RFC
	mut prev_block := [1, 1, 1]
	mut curr_block := [0, 0, 0]
	mut block_remaining := [0, 0, 0]
	if nbltypes_l >= 2 {
		block_remaining[0] = 1
	} else {
		block_remaining[0] = mlen
	}
	if nbltypes_i >= 2 {
		block_remaining[1] = 1
	} else {
		block_remaining[1] = mlen
	}
	if nbltypes_d >= 2 {
		block_remaining[2] = 1
	} else {
		block_remaining[2] = mlen
	}
	mut produced := 0

	// Ensure an insert-and-copy block is active (write block-type/count if needed)
	if nbltypes_i >= 2 && block_remaining[1] == 0 {
		new_type := curr_block[1]
		sym_bt := encode_block_type_symbol(prev_block[1], curr_block[1], new_type, nbltypes_i)
		write_symbol_using_codes(mut w, insert_block_type_codes, insert_block_type_lengths, sym_bt)
		// default to a single-block count
		write_block_count_using_enc(mut w, insert_block_count_codes, insert_block_count_lengths, 1)!
		prev_block[1] = curr_block[1]
		curr_block[1] = new_type
		block_remaining[1] = 1
	}

	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(pattern.len - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	if mlen == pattern.len {
		w.write_bits(0, cmd.copy_len_extra_bits)
	} else {
		w.write_bits(u32((mlen - pattern.len) - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	}
	// one command consumed
	if nbltypes_i >= 2 {
		block_remaining[1]--
	}

	// Literals
	if lit_symbols.len == 1 {
		// single-symbol literal tree: write nothing for symbols
		if mlen > pattern.len {
			// Ensure distance block is active if needed
			if cmd.distance_code == -1 && nbltypes_d >= 2 && block_remaining[2] == 0 {
				new_type := curr_block[2]
				sym_bt := encode_block_type_symbol(prev_block[2], curr_block[2], new_type, nbltypes_d)
				write_symbol_using_codes(mut w, distance_block_type_codes, distance_block_type_lengths, sym_bt)
				write_block_count_using_enc(mut w, distance_block_count_codes, distance_block_count_lengths, 1)!
				prev_block[2] = curr_block[2]
				curr_block[2] = new_type
				block_remaining[2] = 1
			}
			w.write_bits(u32(dist_code.extra), dist_code.extra_bits)
			if dist_code.should_push {
				push_recent_distance(mut recent_distances, pattern.len)
			}
		}
		return
	}

	max_bits := max_length(literal_tree_lengths)
	enc := huffman.build(lengths: literal_tree_lengths, max_bits: max_bits, bit_order: .lsb_first)!
	for b in pattern {
		// Ensure literal block active
		if nbltypes_l >= 2 && block_remaining[0] == 0 {
			new_type := curr_block[0]
			sym_bt := encode_block_type_symbol(prev_block[0], curr_block[0], new_type, nbltypes_l)
			write_symbol_using_codes(mut w, literal_block_type_codes, literal_block_type_lengths, sym_bt)
			write_block_count_using_enc(mut w, literal_block_count_codes, literal_block_count_lengths, 1)!
			prev_block[0] = curr_block[0]
			curr_block[0] = new_type
			block_remaining[0] = 1
		}
		w.write_bits(enc.codes[int(b)], literal_tree_lengths[int(b)])
		produced++
		if nbltypes_l >= 2 {
			block_remaining[0]--
		}
	}
	if mlen > pattern.len {
		// Ensure distance block is active if needed
		if cmd.distance_code == -1 && nbltypes_d >= 2 && block_remaining[2] == 0 {
			new_type := curr_block[2]
			sym_bt := encode_block_type_symbol(prev_block[2], curr_block[2], new_type, nbltypes_d)
			write_symbol_using_codes(mut w, distance_block_type_codes, distance_block_type_lengths, sym_bt)
			write_block_count_using_enc(mut w, distance_block_count_codes, distance_block_count_lengths, 1)!
			prev_block[2] = curr_block[2]
			curr_block[2] = new_type
			block_remaining[2] = 1
		}
		w.write_bits(u32(dist_code.extra), dist_code.extra_bits)
		if dist_code.should_push {
			push_recent_distance(mut recent_distances, pattern.len)
		}
	}
}

fn write_small_alphabet_compressed_meta_block(mut w BitWriter, data []u8, symbols []int, is_last bool) ! {
	w.write_bit(u8(if is_last { 1 } else { 0 })) // ISLAST
	if is_last {
		w.write_bit(0) // ISLASTEMPTY
	}
	mnibbles := meta_block_nibbles(data.len)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		6 { 2 }
		else { 3 }
	}), 2)
	write_meta_block_length(mut w, data.len, mnibbles)
	if !is_last {
		w.write_bit(0) // ISUNCOMPRESSED = 0
	}
	write_varlen_uint8(mut w, 0) // 1 literal block type
	write_varlen_uint8(mut w, 0) // 1 insert-copy block type
	write_varlen_uint8(mut w, 0) // 1 distance block type
	w.write_bits(0, 2) // NPOSTFIX = 0
	w.write_bits(0, 4) // NDIRECT = 0
	w.write_bits(0, 2) // literal context mode
	write_varlen_uint8(mut w, 0) // 1 literal tree
	write_varlen_uint8(mut w, 0) // 1 distance tree
	write_simple_prefix_tree_symbols(mut w, 256, symbols)!
	cmd_sym := find_literal_only_cmd_symbol_for_insert_len(data.len)!
	write_simple_prefix_tree(mut w, 704, cmd_sym)
	write_simple_prefix_tree(mut w, 64, 0)
	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(data.len - cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(0, cmd.copy_len_extra_bits)
	lengths := simple_prefix_lengths(256, symbols)!
	max_bits := max_length(lengths)
	enc := huffman.build(lengths: lengths, max_bits: max_bits, bit_order: .lsb_first)!
	for b in data {
		w.write_bits(enc.codes[int(b)], lengths[int(b)])
	}
}

struct StaticDictionaryMatch {
	word_len int
	address  int
}

fn dictionary_transform_output_len(word_len int, transform_idx int) !int {
	if transform_idx < 0 || transform_idx >= brotli_transforms.len / 3 {
		return error('brotli: invalid dictionary transform')
	}
	prefix_id := brotli_transforms[transform_idx * 3]
	transform_type := brotli_transforms[transform_idx * 3 + 1]
	suffix_id := brotli_transforms[transform_idx * 3 + 2]
	prefix_len := brotli_prefix_suffix[brotli_prefix_suffix_map[prefix_id]]
	suffix_len := brotli_prefix_suffix[brotli_prefix_suffix_map[suffix_id]]
	mut body_len := word_len
	if transform_type <= 9 {
		body_len -= int(transform_type)
	} else if transform_type >= 12 && transform_type <= 20 {
		body_len -= int(transform_type) - 11
	}
	if body_len < 0 {
		body_len = 0
	}
	return int(prefix_len) + body_len + int(suffix_len)
}

fn find_static_dictionary_match(data []u8) ?StaticDictionaryMatch {
	if data.len < 4 || data.len > 32 {
		return none
	}
	mut found := false
	mut best := StaticDictionaryMatch{}
	mut best_cost := 1 << 30
	transform_count := brotli_transforms.len / 3
	for word_len in 4 .. 25 {
		bits := brotli_dictionary_bits[word_len - 4]
		word_count := 1 << bits
		base_offset := brotli_dictionary_offsets[word_len - 4]
		for word_idx in 0 .. word_count {
			offset := base_offset + word_idx * word_len
			word := brotli_dictionary_data[offset..offset + word_len]
			for transform_idx in 0 .. transform_count {
				transformed_len := dictionary_transform_output_len(word_len, transform_idx) or {
					continue
				}
				if transformed_len != data.len {
					continue
				}
				transformed := transform_dictionary_word(word, transform_idx) or { continue }
				if transformed == data {
					address := (transform_idx << bits) | word_idx
					distance := encode_distance_code(address + 1, 0, 0) or { continue }
					if !found || distance.extra_bits < best_cost
						|| distance.extra_bits == best_cost && address < best.address {
						found = true
						best = StaticDictionaryMatch{ word_len: word_len, address: address }
						best_cost = distance.extra_bits
					}
				}
			}
		}
	}
	return if found { best } else { none }
}

fn write_dictionary_copy_meta_block_with_length(mut w BitWriter, data []u8, copy_len int, distance int) ! {
	w.write_bit(1)
	w.write_bit(0)
	mnibbles := meta_block_nibbles(data.len)
	w.write_bits(u32(match mnibbles {
		4 { 0 }
		5 { 1 }
		else { 2 }
	}), 2)
	write_meta_block_length(mut w, data.len, mnibbles)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	w.write_bits(0, 2)
	w.write_bits(0, 4)
	w.write_bits(0, 2)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	write_simple_prefix_tree(mut w, 256, int(data[0]))
	cmd_sym := find_smallest_copy_cmd_symbol(0, copy_len)!
	write_simple_prefix_tree(mut w, 704, cmd_sym)
	dist_code := encode_distance_code(distance, 0, 0)!
	write_simple_prefix_tree(mut w, 64, dist_code.symbol)
	cmd := cmd_lut(cmd_sym)
	w.write_bits(u32(-cmd.insert_len_offset), cmd.insert_len_extra_bits)
	w.write_bits(u32(copy_len - cmd.copy_len_offset), cmd.copy_len_extra_bits)
	w.write_bits(u32(dist_code.extra), dist_code.extra_bits)
	return
}
