module brotli

import hash.huffman

fn meta_block_nibbles(mlen int) int {
	if mlen <= 0x10000 {
		return 4
	}
	if mlen <= 0x10_0000 {
		return 5
	}
	return 6
}

fn max_meta_block_len_for_nibbles(mnibbles int) int {
	return match mnibbles {
		4 { 0x10000 }
		5 { 0x100000 }
		else { 0x1000000 }
	}
}

fn find_periodic_pattern(data []u8) []u8 {
	max_period := if data.len < 256 { data.len } else { 256 }
	for period in 1 .. max_period + 1 {
		if data.len % period != 0 {
			continue
		}
		mut matches := true
		mut offset := 0
		for i in period .. data.len {
			if data[i] != data[offset] {
				matches = false
				break
			}
			offset++
			if offset == period {
				offset = 0
			}
		}
		if matches {
			return data[..period]
		}
	}
	return []u8{}
}

struct PeriodicRun {
	period int
	length int
}

fn find_periodic_run(data []u8, start int, max_period int) PeriodicRun {
	if start < 0 || start >= data.len {
		return PeriodicRun{}
	}
	period_limit := if data.len - start < max_period { data.len - start } else { max_period }
	for period in 1 .. period_limit + 1 {
		if start + period * 2 > data.len {
			break
		}
		mut matches := true
		for offset in 0 .. period {
			if data[start + offset] != data[start + period + offset] {
				matches = false
				break
			}
		}
		if !matches {
			continue
		}
		mut run_len := period * 2
		mut offset := 0
		for start + run_len < data.len && data[start + run_len] == data[start + offset] {
			run_len++
			offset++
			if offset == period {
				offset = 0
			}
		}
		return PeriodicRun{ period: period, length: run_len }
	}
	return PeriodicRun{}
}

fn periodic_search_limit(quality int, window_bits int) int {
	quality_limit := if quality <= 3 {
		96
	} else if quality <= 7 {
		160
	} else {
		256
	}
	window_limit := if window_bits <= 16 {
		96
	} else if window_bits <= 18 {
		160
	} else {
		256
	}
	return if quality_limit < window_limit { quality_limit } else { window_limit }
}

struct LzCommand {
	insert_len int
	copy_len   int
	distance   int
}

struct LzMatch {
	length   int
	distance int
}

fn lz_dictionary_key(data []u8) u64 {
	mut key := u64(14695981039346656037)
	for value in data {
		key ^= u64(value)
		key *= 1099511628211
	}
	return key ^ u64(data.len)
}

fn lz_dictionary_bytes_equal(left []u8, right []u8) bool {
	if left.len != right.len {
		return false
	}
	for index in 0 .. left.len {
		if left[index] != right[index] {
			return false
		}
	}
	return true
}

fn lz_static_dictionary_matches(data []u8, history_len int, search_limit int) []LzMatch {
	mut matches := []LzMatch{len: data.len}
	limit := if data.len < search_limit { data.len } else { search_limit }
	mut targets := map[u64][]int{}
	for pos in 0 .. limit {
		max_len := if data.len - pos > 24 { 24 } else { data.len - pos }
		for length in 4 .. max_len + 1 {
			key := lz_dictionary_key(data[pos..pos + length])
			mut positions := targets[key] or { []int{} }
			positions << pos
			targets[key] = positions
		}
	}
	mut best_distance_bits := []int{len: data.len, init: 1 << 30}
	for word_len in 4 .. 25 {
		bits := brotli_dictionary_bits[word_len - 4]
		word_count := 1 << bits
		base_offset := brotli_dictionary_offsets[word_len - 4]
		for word_idx in 0 .. word_count {
			offset := base_offset + word_idx * word_len
			word := brotli_dictionary_data[offset..offset + word_len]
			for transform_idx in 0 .. brotli_transforms.len / 3 {
				transformed_len := dictionary_transform_output_len(word_len, transform_idx) or { continue }
				if transformed_len != word_len {
					continue
				}
				transformed := transform_dictionary_word(word, transform_idx) or { continue }
				positions := targets[lz_dictionary_key(transformed)] or { continue }
				mut pos := -1
				for candidate_pos in positions {
					if candidate_pos + transformed_len <= data.len
						&& lz_dictionary_bytes_equal(data[candidate_pos..candidate_pos + transformed_len], transformed) {
						pos = candidate_pos
						break
					}
				}
				if pos < 0 {
					continue
				}
				address := (transform_idx << bits) | word_idx
				distance := history_len + pos + address + 1
				distance_code := encode_distance_code(distance, 0, 0) or { continue }
				if transformed_len > matches[pos].length || transformed_len == matches[pos].length
					&& distance_code.extra_bits < best_distance_bits[pos] {
					matches[pos] = LzMatch{ length: transformed_len, distance: distance }
					best_distance_bits[pos] = distance_code.extra_bits
				}
			}
		}
	}
	return matches
}

fn lz_hash(data []u8, pos int) int {
	return (int(data[pos]) * 251 + int(data[pos + 1]) * 17 + int(data[pos + 2]) * 31 +
		int(data[pos + 3])) & 0xffff
}

fn has_short_lz_match(data []u8) bool {
	if data.len < 8 {
		return false
	}
	for pos in 1 .. data.len - 3 {
		for previous in 0 .. pos {
			if data[previous] == data[pos] && data[previous + 1] == data[pos + 1]
				&& data[previous + 2] == data[pos + 2] && data[previous + 3] == data[pos + 3] {
				return true
			}
		}
	}
	return false
}

fn lz_chain_limit(quality int) int {
	return if quality <= 2 {
		8
	} else if quality <= 5 {
		32
	} else if quality <= 8 {
		96
	} else {
		256
	}
}

fn lz_max_distance(window_bits int) int {
	return (1 << window_bits) - 16
}

fn lz_copy_extra_bits(copy_len int, mut cache []int) int {
	if copy_len <= 0 || copy_len >= cache.len {
		return 24
	}
	if cache[copy_len] >= 0 {
		return cache[copy_len]
	}
	mut best := 24
	for copy_code in 0 .. cmd_copy_offsets.len {
		offset := int(cmd_copy_offsets[copy_code])
		extra_bits := int(cmd_copy_length_extra_bits[copy_code])
		if copy_len >= offset && copy_len < offset + (1 << extra_bits) && extra_bits < best {
			best = extra_bits
		}
	}
	cache[copy_len] = best
	return best
}

fn lz_match_cost_with_recent(distance int, copy_len int, mut copy_costs []int, recent_distances []int) int {
	copy_extra_bits := lz_copy_extra_bits(copy_len, mut copy_costs)
	distance_bits := if distance_code := encode_distance_code_with_recent(distance, recent_distances, 0, 0) {
		distance_code.extra_bits
	} else {
		16
	}
	return 7 + copy_extra_bits + distance_bits
}

fn lz_huffman_lengths_are_serializable(lengths []int) bool {
	mut w := BitWriter{}
	if _ := write_prefix_tree(mut w, lengths) {
		return true
	}
	return false
}

fn is_lz_cost_candidate(copy_len int, best_len int) bool {
	if copy_len == best_len {
		return true
	}
	for offset in cmd_copy_offsets {
		if copy_len == int(offset) || copy_len == int(offset) - 1 {
			return true
		}
	}
	return copy_len == 4
}

fn lz_build_dp_commands(data []u8, best_lengths []int, best_distances []int,
	initial_recent_distances []int, recent_at [][]int, static_distances []int) []LzCommand {
	mut copy_costs := []int{len: 257, init: -1}
	mut costs := []int{len: data.len + 1, init: 0x3fffffff}
	mut next_lengths := []int{len: data.len, init: 0}
	mut next_distances := []int{len: data.len, init: 0}
	costs[data.len] = 0
	for cost_pos := data.len - 1; cost_pos >= 0; cost_pos-- {
		costs[cost_pos] = 7 + costs[cost_pos + 1]
		mut best_len := best_lengths[cost_pos]
		if best_len < 4 {
			continue
		}
		if best_len > data.len - cost_pos {
			best_len = data.len - cost_pos
		}
		is_static_match := static_distances.len > cost_pos
			&& static_distances[cost_pos] > 0 && static_distances[cost_pos] == best_distances[cost_pos]
		max_costed_len := if best_len > 256 { 256 } else { best_len }
		recent_distances := if recent_at.len > cost_pos {
			recent_at[cost_pos]
		} else {
			initial_recent_distances
		}
		for copy_len := 4; copy_len <= max_costed_len; copy_len++ {
			if is_static_match && copy_len != best_len {
				continue
			}
			if !is_lz_cost_candidate(copy_len, best_len) {
				continue
			}
			match_cost := lz_match_cost_with_recent(best_distances[cost_pos], copy_len, mut copy_costs,
				recent_distances) - 3
			candidate_cost := match_cost + costs[cost_pos + copy_len]
			if candidate_cost < costs[cost_pos] {
				costs[cost_pos] = candidate_cost
				next_lengths[cost_pos] = copy_len
				next_distances[cost_pos] = best_distances[cost_pos]
			}
		}
	}
	mut commands := []LzCommand{}
	mut parse_pos := 0
	mut literal_start := 0
	for parse_pos < data.len {
		copy_len := next_lengths[parse_pos]
		if copy_len == 0 {
			parse_pos++
			continue
		}
		commands << LzCommand{
			insert_len: parse_pos - literal_start
			copy_len:   copy_len
			distance:   next_distances[parse_pos]
		}
		parse_pos += copy_len
		literal_start = parse_pos
	}
	if literal_start < data.len {
		commands << LzCommand{ insert_len: data.len - literal_start }
	}
	return commands
}

fn lz_recent_distance_snapshots(data_len int, commands []LzCommand,
	initial_recent_distances []int) [][]int {
	mut snapshots := [][]int{len: data_len}
	mut recent_distances := initial_recent_distances.clone()
	mut data_pos := 0
	for command in commands {
		command_pos := data_pos
		for _ in 0 .. command.insert_len {
			if data_pos < snapshots.len {
				snapshots[data_pos] = recent_distances.clone()
			}
			data_pos++
		}
		for _ in 0 .. command.copy_len {
			if data_pos < snapshots.len {
				snapshots[data_pos] = recent_distances.clone()
			}
			data_pos++
		}
		if command.copy_len > 0 {
			if distance_code := encode_distance_code_with_recent(command.distance, recent_distances, 0, 0) {
				if distance_code.should_push && command.distance <= command_pos {
					push_recent_distance(mut recent_distances, command.distance)
				}
			}
		}
	}
	return snapshots
}

fn lz_find_best_match(data []u8, pos int, heads []int, previous []int, max_distance int, chain_limit int, max_length int, recent_distances []int, use_savings_cost bool) LzMatch {
	if pos + 4 > data.len {
		return LzMatch{}
	}
	hash := lz_hash(data, pos)
	mut candidate := heads[hash]
	mut best_len := 0
	mut best_distance := 0
	mut steps := 0
	mut copy_costs := []int{len: 257, init: -1}
	for candidate >= 0 && pos - candidate <= max_distance && steps < chain_limit {
		if max_length > 0 && best_len >= max_length {
			break
		}
		if !use_savings_cost && best_len >= 4 && pos + best_len < data.len
			&& data[pos + best_len] != data[candidate + best_len] {
			candidate = previous[candidate]
			steps++
			continue
		}
		if pos + 3 < data.len && data[pos + 3] != data[candidate + 3] {
			candidate = previous[candidate]
			steps++
			continue
		}
		mut length := 0
		for pos + length < data.len && data[pos + length] == data[candidate + length] {
			length++
			if max_length > 0 && length == max_length {
				break
			}
		}
		mut better := false
		if use_savings_cost {
			candidate_cost := if length > 0 {
				lz_match_cost_with_recent(pos - candidate, length, mut copy_costs, recent_distances) - length * 8
			} else {
				1 << 30
			}
			best_cost := if best_len > 0 {
				lz_match_cost_with_recent(best_distance, best_len, mut copy_costs, recent_distances) - best_len * 8
			} else {
				1 << 30
			}
			better = candidate_cost < best_cost || candidate_cost == best_cost && length > best_len
		} else if length > best_len {
			better = true
		} else if length == best_len && length > 0 {
			candidate_distance := encode_distance_code_with_recent(pos - candidate, recent_distances, 0, 0) or {
				DistanceCode{ extra_bits: 16 }
			}
			best_distance_code := encode_distance_code_with_recent(best_distance, recent_distances, 0, 0) or {
				DistanceCode{ extra_bits: 16 }
			}
			better = candidate_distance.extra_bits < best_distance_code.extra_bits
		}
		if better {
			best_len = length
			best_distance = pos - candidate
		}
		candidate = previous[candidate]
		steps++
	}
	return LzMatch{ length: best_len, distance: best_distance }
}

fn parse_lz_commands_greedy(data []u8, window_bits int, quality int, initial_recent_distances []int,
	history []u8) []LzCommand {
	mut parser_data := history.clone()
	parser_data << data
	history_len := history.len
	mut heads := []int{len: 65536, init: -1}
	mut previous := []int{len: parser_data.len, init: -1}
	mut commands := []LzCommand{}
	mut pos := history_len
	mut literal_start := history_len
	max_distance := lz_max_distance(window_bits)
	chain_limit := lz_chain_limit(quality)
	mut copy_costs := []int{len: 257, init: -1}
	mut recent_distances := initial_recent_distances.clone()
	static_matches := if quality == 5 && data.len >= 20 && data.len <= 65536 {
		lz_static_dictionary_matches(data, history_len, 4096)
	} else {
		[]LzMatch{len: data.len}
	}
	for indexed in 0 .. history_len {
		if indexed + 4 > parser_data.len {
			break
		}
		hash := lz_hash(parser_data, indexed)
		previous[indexed] = heads[hash]
		heads[hash] = indexed
	}
	for pos < parser_data.len {
		best_match := lz_find_best_match(parser_data, pos, heads, previous, max_distance, chain_limit, 0,
			recent_distances, quality <= 5)
		mut best_len := best_match.length
		mut best_distance := best_match.distance
		if pos - history_len < static_matches.len {
			static_match := static_matches[pos - history_len]
			if static_match.length > best_len {
				best_len = static_match.length
				best_distance = static_match.distance
			}
		}
		if best_len >= 4 && pos + 4 < parser_data.len {
			next_match := lz_find_best_match(parser_data, pos + 1, heads, previous, max_distance, chain_limit, 0,
				recent_distances, quality <= 5)
			mut next_best_len := next_match.length
			mut next_best_distance := next_match.distance
			if pos + 1 - history_len < static_matches.len {
				static_match := static_matches[pos + 1 - history_len]
				if static_match.length > next_best_len {
					next_best_len = static_match.length
					next_best_distance = static_match.distance
				}
			}
			current_cost := lz_match_cost_with_recent(best_distance, best_len, mut copy_costs, recent_distances)
			next_cost := lz_match_cost_with_recent(next_best_distance, next_best_len, mut copy_costs, recent_distances)
			if next_best_len >= 4 && next_cost + 8 < current_cost {
				hash := lz_hash(parser_data, pos)
				previous[pos] = heads[hash]
				heads[hash] = pos
				pos++
				continue
			}
		}
		if best_len < 4 {
			if pos + 4 <= parser_data.len {
				hash := lz_hash(parser_data, pos)
				previous[pos] = heads[hash]
				heads[hash] = pos
			}
			pos++
			continue
		}
		commands << LzCommand{ insert_len: pos - literal_start, copy_len: best_len, distance: best_distance }
		if distance_code := encode_distance_code_with_recent(best_distance, recent_distances, 0, 0) {
			if distance_code.should_push && best_distance <= pos {
				push_recent_distance(mut recent_distances, best_distance)
			}
		}
		for indexed in pos .. pos + best_len {
			if indexed + 4 > parser_data.len {
				break
			}
			hash := lz_hash(parser_data, indexed)
			previous[indexed] = heads[hash]
			heads[hash] = indexed
		}
		pos += best_len
		literal_start = pos
	}
	if literal_start < parser_data.len {
		commands << LzCommand{ insert_len: parser_data.len - literal_start }
	}
	return commands
}

fn parse_lz_commands(data []u8, window_bits int, quality int, initial_recent_distances []int) []LzCommand {
	return parse_lz_commands_with_history(data, window_bits, quality, initial_recent_distances, []u8{})
}

fn is_json_like_lz_input(data []u8) bool {
	mut braces := 0
	mut quotes := 0
	mut colons := 0
	mut commas := 0
	mut newlines := 0
	limit := if data.len < 4096 { data.len } else { 4096 }
	for value in data[..limit] {
		match value {
			`{`, `}` { braces++ }
			`"` { quotes++ }
			`:` { colons++ }
			`,` { commas++ }
			`\n` { newlines++ }
			else {}
		}
	}
	return braces >= 4 && quotes >= 16 && colons >= 4 && commas >= 4 && newlines >= 4
}

fn lz_commands_are_encodable(data []u8, commands []LzCommand, recent_distances []int) bool {
	mut command_freqs := []int{len: 704}
	mut distance_freqs := []int{len: 64}
	mut literal_freqs := []int{len: 256}
	mut literal_context_freqs := [][]int{len: 64}
	for index in 0 .. literal_context_freqs.len {
		literal_context_freqs[index] = []int{len: 256}
	}
	mut recent := recent_distances.clone()
	mut data_pos := 0
	mut p1 := u8(0)
	mut p2 := u8(0)
	for command in commands {
		command_pos := data_pos
		cmd_sym := if command.copy_len > 0 {
			find_smallest_copy_cmd_symbol(command.insert_len, command.copy_len) or { return false }
		} else {
			find_literal_only_cmd_symbol_for_insert_len(command.insert_len) or { return false }
		}
		command_freqs[cmd_sym]++
		for _ in 0 .. command.insert_len {
			literal := data[data_pos]
			literal_freqs[int(literal)]++
			context_id := compute_literal_context_id(p1, p2, 0)
			literal_context_freqs[context_id][int(literal)]++
			p2 = p1
			p1 = literal
			data_pos++
		}
		for _ in 0 .. command.copy_len {
			if data_pos >= data.len {
				return false
			}
			p2 = p1
			p1 = data[data_pos]
			data_pos++
		}
		if command.copy_len > 0 {
			distance := encode_distance_code_with_recent(command.distance, recent, 0, 0) or { return false }
			distance_freqs[distance.symbol]++
			if distance.should_push && command.distance <= command_pos {
				push_recent_distance(mut recent, command.distance)
			}
		}
	}
	mut command_unique := 0
	for frequency in command_freqs {
		if frequency > 0 {
			command_unique++
		}
	}
	if command_unique > 4 {
		if command_lengths := compute_huffman_lengths(command_freqs, 15) {
			if !lz_huffman_lengths_are_serializable(command_lengths) {
				return false
			}
		} else {
			return false
		}
	}
	if literal_lengths := compute_huffman_lengths(literal_freqs, 15) {
		if !lz_huffman_lengths_are_serializable(literal_lengths) {
			return false
		}
	} else {
		return false
	}
	mut distance_unique := 0
	for frequency in distance_freqs {
		if frequency > 0 {
			distance_unique++
		}
	}
	if distance_unique > 4 {
		if distance_lengths := compute_huffman_lengths(distance_freqs, 15) {
			if !lz_huffman_lengths_are_serializable(distance_lengths) {
				return false
			}
		} else {
			return false
		}
	}
	if data.len > 80000 && data.len <= 0x100000 {
		for tree_freqs in literal_context_freqs {
			if _ := compute_huffman_lengths(tree_freqs, 15) {
			} else {
				return false
			}
		}
	}
	return data_pos == data.len
}

fn parse_lz_commands_with_history(data []u8, window_bits int, quality int,
	initial_recent_distances []int, history []u8) []LzCommand {
	if data.len < 3 {
		return [LzCommand{ insert_len: data.len }]
	}
	use_json_dp := quality == 5 && data.len > 80000 && data.len <= 0x100000
		&& is_json_like_lz_input(data)
	if history.len > 0 || quality <= 1 && data.len > 8192 || quality >= 8
		|| !use_json_dp && data.len >= 60000 || quality >= 4 && data.len < 60000 {
		return parse_lz_commands_greedy(data, window_bits, quality, initial_recent_distances, history)
	}
	mut heads := []int{len: 65536, init: -1}
	mut previous := []int{len: data.len, init: -1}
	mut best_lengths := []int{len: data.len, init: 0}
	mut best_distances := []int{len: data.len, init: 0}
	mut static_distances := []int{len: data.len, init: 0}
	max_distance := lz_max_distance(window_bits)
	chain_limit := lz_chain_limit(quality)
	static_matches := if use_json_dp {
		lz_static_dictionary_matches(data, 0, 4096)
	} else {
		[]LzMatch{len: data.len}
	}
	mut pos := 0
	for pos < data.len {
		best_match := lz_find_best_match(data, pos, heads, previous, max_distance, chain_limit, 256,
			initial_recent_distances, true)
		mut best_len := best_match.length
		mut best_distance := best_match.distance
		if pos < static_matches.len {
			static_match := static_matches[pos]
			if static_match.length > best_len {
				best_len = static_match.length
				best_distance = static_match.distance
				static_distances[pos] = static_match.distance
			}
		}
		best_lengths[pos] = best_len
		best_distances[pos] = best_distance
		if pos + 4 <= data.len {
			hash := lz_hash(data, pos)
			previous[pos] = heads[hash]
			heads[hash] = pos
		}
		pos++
	}
	mut commands := lz_build_dp_commands(data, best_lengths, best_distances, initial_recent_distances, [][]int{}, static_distances)
	mut recent_snapshots := lz_recent_distance_snapshots(data.len, commands, initial_recent_distances)
	commands = lz_build_dp_commands(data, best_lengths, best_distances, initial_recent_distances, recent_snapshots, static_distances)
	recent_snapshots = lz_recent_distance_snapshots(data.len, commands, initial_recent_distances)
	commands = lz_build_dp_commands(data, best_lengths, best_distances, initial_recent_distances, recent_snapshots, static_distances)
	if !lz_commands_are_encodable(data, commands, initial_recent_distances) {
		return parse_lz_commands_greedy(data, window_bits, quality, initial_recent_distances, history)
	}
	return commands
}

// write_lz_compressed_meta_block emits a greedy LZ77 block with shared RFC trees.
fn write_lz_compressed_meta_block(mut w BitWriter, data []u8, is_last bool, window_bits int, quality int, mut recent_distances []int) ! {
	return write_lz_compressed_meta_block_with_history(mut w, data, is_last, window_bits, quality,
		mut recent_distances, []u8{})
}

fn write_lz_compressed_meta_block_with_history(mut w BitWriter, data []u8, is_last bool, window_bits int,
	quality int, mut recent_distances []int, history []u8) ! {
	if data.len == 0 {
		return error('brotli: invalid compressed meta-block length')
	}
	commands := parse_lz_commands_with_history(data, window_bits, quality, recent_distances, history)
	mut command_symbols := []int{}
	mut distance_codes := []DistanceCode{}
	mut literal_freqs := []int{len: 256}
	mut literal_context_freqs := [][]int{len: 64}
	for index in 0 .. literal_context_freqs.len {
		literal_context_freqs[index] = []int{len: 256}
	}
	mut command_freqs := []int{len: 704}
	mut distance_freqs := []int{len: 64}
	mut distance_context_freqs := [][]int{len: 4}
	for index in 0 .. distance_context_freqs.len {
		distance_context_freqs[index] = []int{len: 64}
	}
	mut data_pos := 0
	mut p1 := u8(0)
	mut p2 := u8(0)
	context_mode := 0
	for command in commands {
		cmd_sym := if command.copy_len > 0 {
			find_smallest_copy_cmd_symbol(command.insert_len, command.copy_len)!
		} else {
			find_literal_only_cmd_symbol_for_insert_len(command.insert_len)!
		}
		command_symbols << cmd_sym
		command_freqs[cmd_sym]++
		for i in 0 .. command.insert_len {
			literal := int(data[data_pos + i])
			literal_freqs[literal]++
			context_id := compute_literal_context_id(p1, p2, context_mode)
			literal_context_freqs[context_id][literal]++
			p2 = p1
			p1 = u8(literal)
		}
		data_pos += command.insert_len
		if command.copy_len > 0 {
			distance_code := encode_distance_code_with_recent(command.distance, recent_distances, 0, 0)!
			distance_codes << distance_code
			distance_freqs[distance_code.symbol]++
			distance_context_freqs[cmd_lut(cmd_sym).context][distance_code.symbol]++
			if distance_code.should_push && command.distance <= history.len + data_pos {
				push_recent_distance(mut recent_distances, command.distance)
			}
			for i in 0 .. command.copy_len {
				literal := data[data_pos + i]
				p2 = p1
				p1 = literal
			}
			data_pos += command.copy_len
		} else {
			distance_codes << DistanceCode{}
		}
	}
	if data_pos != data.len {
		return error('brotli: LZ parser length mismatch')
	}
	mut command_unique := []int{}
	for symbol, frequency in command_freqs {
		if frequency > 0 {
			command_unique << symbol
		}
	}
	mut distance_unique := []int{}
	for symbol, frequency in distance_freqs {
		if frequency > 0 {
			distance_unique << symbol
		}
	}
	mut literal_unique := []int{}
	for sym, frequency in literal_freqs {
		if frequency > 0 {
			literal_unique << sym
		}
	}
	if literal_unique.len == 0 {
		literal_unique << 0
	}
	mut literal_lengths := if literal_unique.len <= 4 {
		simple_prefix_lengths(256, literal_unique)!
	} else {
		compute_huffman_lengths(literal_freqs, 15)!
	}
	mut literal_tree_count := 1
	mut literal_tree_lengths := [literal_lengths.clone()]
	mut literal_context_map := []int{len: 64}
	for context_id in 0 .. 64 {
		literal_context_map[context_id] = context_id % 2
	}
	if literal_unique.len > 4 && data.len > 80000 && data.len <= 0x100000 {
		mut context_tree_lengths := [][]int{len: 2}
		mut context_stable := false
		for iteration in 0 .. 17 {
			mut aggregate_freqs := [][]int{len: 2}
			for tree_index in 0 .. aggregate_freqs.len {
				aggregate_freqs[tree_index] = []int{len: 256}
			}
			for context_id in 0 .. literal_context_map.len {
				tree_index := literal_context_map[context_id]
				if tree_index < 0 || tree_index >= aggregate_freqs.len {
					return error('brotli: adaptive aggregate tree index out of range')
				}
				for symbol, frequency in literal_context_freqs[context_id] {
					aggregate_freqs[tree_index][symbol] += frequency
				}
			}
			mut context_valid := true
			for tree_index in 0 .. context_tree_lengths.len {
				mut tree_freqs := aggregate_freqs[tree_index].clone()
				mut tree_symbols := 0
				for frequency in tree_freqs {
					if frequency > 0 {
						tree_symbols++
					}
				}
				if tree_symbols == 0 {
					tree_freqs[literal_unique[0]] = 1
					tree_symbols = 1
				}
				if tree_symbols == 1 {
					for symbol, frequency in tree_freqs {
						if frequency == 0 {
							tree_freqs[symbol] = 1
							break
						}
					}
				}
				context_tree_lengths[tree_index] = compute_huffman_lengths(tree_freqs, 15) or {
					context_valid = false
					break
				}
			}
			if !context_valid {
				break
			}
			if iteration == 16 {
				context_stable = true
				break
			}
			mut changed := false
			for context_id in 0 .. literal_context_map.len {
				mut cost_zero := 0
				mut cost_one := 0
				for sym, frequency in literal_context_freqs[context_id] {
					if frequency > 0 {
						cost_zero += frequency * context_tree_lengths[0][sym]
						cost_one += frequency * context_tree_lengths[1][sym]
					}
				}
				new_tree := if cost_one < cost_zero { 1 } else { 0 }
				if literal_context_map[context_id] != new_tree {
					literal_context_map[context_id] = new_tree
					changed = true
				}
			}
			if !changed {
				context_stable = true
				break
			}
		}
		for tree_index in literal_context_map {
			if tree_index < 0 || tree_index >= 2 {
				return error('brotli: adaptive literal context map value out of range')
			}
		}
		mut global_bits := 0
		for sym, frequency in literal_freqs {
			if frequency > 0 && literal_lengths[sym] > 0 {
				global_bits += frequency * literal_lengths[sym]
			}
		}
		mut context_bits := 0
		for context_id in 0 .. literal_context_map.len {
			tree_index := literal_context_map[context_id]
			if tree_index < 0 || tree_index >= context_tree_lengths.len {
				return error('brotli: adaptive cost tree index out of range')
			}
			for sym, frequency in literal_context_freqs[context_id] {
				if frequency > 0 && context_tree_lengths[tree_index][sym] > 0 {
					context_bits += frequency * context_tree_lengths[tree_index][sym]
				}
			}
		}
		if context_stable && context_bits + 128 < global_bits {
			literal_tree_count = 2
			literal_tree_lengths = context_tree_lengths.clone()
		}
	}
	mut command_lengths := []int{}
	mut distance_lengths := []int{}
	if command_unique.len <= 4 {
		command_lengths = simple_prefix_lengths(704, command_unique)!
	} else {
		command_lengths = compute_huffman_lengths(command_freqs, 15)!
	}
	if distance_unique.len == 0 {
		distance_lengths = simple_prefix_lengths(64, [0])!
	} else if distance_unique.len <= 4 {
		distance_lengths = simple_prefix_lengths(64, distance_unique)!
	} else {
		distance_lengths = compute_huffman_lengths(distance_freqs, 15)!
	}
	mut distance_tree_count := 1
	mut distance_tree_lengths := [distance_lengths.clone()]
	mut distance_context_map := []int{len: 4, init: 0}
	if false && distance_unique.len > 4 && data.len > 4096 {
		mut context_lengths := [][]int{len: 4}
		mut context_bits := 0
		mut context_valid := true
		for tree_index in 0 .. context_lengths.len {
			mut tree_freqs := distance_context_freqs[tree_index].clone()
			mut tree_unique := 0
			for frequency in tree_freqs {
				if frequency > 0 {
					tree_unique++
				}
			}
			if tree_unique == 0 {
				tree_freqs[distance_unique[0]] = 1
			}
			context_lengths[tree_index] = compute_huffman_lengths(tree_freqs, 15) or {
				context_valid = false
				break
			}
			for symbol, frequency in distance_context_freqs[tree_index] {
				if frequency > 0 && context_lengths[tree_index][symbol] > 0 {
					context_bits += frequency * context_lengths[tree_index][symbol]
				}
			}
		}
		mut global_bits := 0
		for symbol, frequency in distance_freqs {
			if frequency > 0 && distance_lengths[symbol] > 0 {
				global_bits += frequency * distance_lengths[symbol]
			}
		}
		if context_valid && context_bits + 128 < global_bits {
			distance_tree_count = 4
			distance_tree_lengths = context_lengths.clone()
			for index in 0 .. distance_context_map.len {
				distance_context_map[index] = index
			}
		}
	}
	mnibbles := meta_block_nibbles(data.len)
	w.write_bit(u8(if is_last { 1 } else { 0 }))
	if is_last {
		w.write_bit(0) // ISLASTEMPTY = 0 (not empty)
	}
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
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	w.write_bits(0, 2)
	w.write_bits(0, 4)
	w.write_bits(u32(context_mode), 2)
	write_varlen_uint8(mut w, literal_tree_count - 1)
	if literal_tree_count >= 2 {
		write_context_map(mut w, literal_context_map, literal_tree_count)!
	}
	write_varlen_uint8(mut w, distance_tree_count - 1)
	if distance_tree_count >= 2 {
		write_context_map(mut w, distance_context_map, distance_tree_count)!
	}
	if literal_unique.len <= 4 {
		if literal_tree_count == 1 {
			write_simple_prefix_tree_symbols(mut w, 256, literal_unique)!
		} else {
			for tree_index in 0 .. literal_tree_count {
				write_prefix_tree(mut w, literal_tree_lengths[tree_index])!
			}
		}
	} else {
		for tree_index in 0 .. literal_tree_count {
			write_prefix_tree(mut w, literal_tree_lengths[tree_index])!
		}
	}
	if command_unique.len <= 4 {
		write_simple_prefix_tree_symbols(mut w, 704, command_unique)!
	} else {
		write_prefix_tree(mut w, command_lengths)!
	}
	for tree_index in 0 .. distance_tree_count {
		if distance_unique.len == 0 {
			write_simple_prefix_tree(mut w, 64, 0)
		} else if distance_unique.len <= 4 {
			write_simple_prefix_tree_symbols(mut w, 64, distance_unique)!
		} else {
			write_prefix_tree(mut w, distance_tree_lengths[tree_index])!
		}
	}
	mut literal_encoder_lengths := [][]int{len: literal_tree_count}
	mut command_encoder_lengths := command_lengths.clone()
	for tree_index in 0 .. literal_tree_count {
		literal_encoder_lengths[tree_index] = literal_tree_lengths[tree_index].clone()
		for idx, length in literal_encoder_lengths[tree_index] {
			if length < 0 {
				literal_encoder_lengths[tree_index][idx] = 1
			}
		}
	}
	for idx, length in command_encoder_lengths {
		if length < 0 {
			command_encoder_lengths[idx] = 1
		}
	}
	mut literal_enc_codes := [][]u32{len: literal_tree_count}
	mut literal_enc_lengths := [][]int{len: literal_tree_count}
	for tree_index in 0 .. literal_tree_count {
		literal_enc := huffman.build(
			lengths:   literal_encoder_lengths[tree_index]
			max_bits:  max_length(literal_encoder_lengths[tree_index])
			bit_order: .lsb_first
		)!
		literal_enc_codes[tree_index] = literal_enc.codes.clone()
		literal_enc_lengths[tree_index] = literal_enc.lengths.clone()
	}
	command_enc := huffman.build(
		lengths:   command_encoder_lengths
		max_bits:  max_length(command_encoder_lengths)
		bit_order: .lsb_first
	)!
	mut distance_enc_codes := [][]u32{len: distance_tree_count}
	mut distance_enc_lengths := [][]int{len: distance_tree_count}
	for tree_index in 0 .. distance_tree_count {
		mut distance_encoder_lengths := distance_tree_lengths[tree_index].clone()
		if distance_unique.len == 0 {
			distance_encoder_lengths = simple_prefix_lengths(64, [0])!
			distance_encoder_lengths[0] = 1
		} else {
			for idx, length in distance_encoder_lengths {
				if length < 0 {
					distance_encoder_lengths[idx] = 1
				}
			}
		}
		distance_enc := huffman.build(
			lengths:   distance_encoder_lengths
			max_bits:  max_length(distance_encoder_lengths)
			bit_order: .lsb_first
		)!
		distance_enc_codes[tree_index] = distance_enc.codes.clone()
		distance_enc_lengths[tree_index] = distance_enc.lengths.clone()
	}
	data_pos = 0
	p1 = 0
	p2 = 0
	mut distance_index := 0
	for index, command in commands {
		cmd_sym := command_symbols[index]
		if command_unique.len > 1 {
			write_symbol_using_codes(mut w, command_enc.codes, command_enc.lengths, cmd_sym)
		}
		cmd := cmd_lut(cmd_sym)
		w.write_bits(u32(command.insert_len - cmd.insert_len_offset), cmd.insert_len_extra_bits)
		w.write_bits(u32(command.copy_len - cmd.copy_len_offset), cmd.copy_len_extra_bits)
		for i in 0 .. command.insert_len {
			literal := data[data_pos + i]
			tree_index := if literal_tree_count == 1 {
				0
			} else {
				literal_context_map[compute_literal_context_id(p1, p2, context_mode)]
			}
			if tree_index < 0 || tree_index >= literal_tree_count {
				return error('brotli: literal context tree index out of range')
			}
			if literal_unique.len > 1 {
				write_symbol_using_codes(mut w, literal_enc_codes[tree_index], literal_enc_lengths[tree_index], int(literal))
			}
			p2 = p1
			p1 = literal
		}
		data_pos += command.insert_len
		if command.copy_len > 0 {
			distance_code := distance_codes[distance_index]
			distance_index++
			if distance_unique.len > 1 {
				distance_tree_index := if distance_tree_count == 1 {
					0
				} else {
					distance_context_map[cmd.context]
				}
				write_symbol_using_codes(mut w, distance_enc_codes[distance_tree_index],
					distance_enc_lengths[distance_tree_index], distance_code.symbol)
			}
			w.write_bits(u32(distance_code.extra), distance_code.extra_bits)
			for i in 0 .. command.copy_len {
				literal := data[data_pos + i]
				p2 = p1
				p1 = literal
			}
			data_pos += command.copy_len
		}
	}
}

fn write_two_periodic_runs_with_symbols(mut w BitWriter, first []u8, first_len int, second []u8, second_len int, literal_symbols []int) ! {
	if first.len == 0 || second.len == 0 || first_len < first.len * 2 || second_len < second.len * 2 {
		return error('brotli: invalid periodic run pair')
	}
	mlen := first_len + second_len
	cmd_one := find_smallest_copy_cmd_symbol(first.len, first_len - first.len)!
	cmd_two := find_smallest_copy_cmd_symbol(second.len, second_len - second.len)!
	dist_one := encode_distance_code(first.len, 0, 0)!
	dist_two := encode_distance_code(second.len, 0, 0)!

	w.write_bit(1)
	w.write_bit(0)
	w.write_bits(0, 2)
	write_meta_block_length(mut w, mlen, 4)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)
	w.write_bits(0, 2)
	w.write_bits(0, 4)
	w.write_bits(0, 2)
	write_varlen_uint8(mut w, 0)
	write_varlen_uint8(mut w, 0)

	mut literal_lengths := []int{}
	if literal_symbols.len > 0 && literal_symbols.len <= 4 {
		write_simple_prefix_tree_symbols(mut w, 256, literal_symbols)!
	} else {
		mut literal_freqs := []int{len: 256}
		for b in first {
			if literal_freqs[int(b)] < 3 {
				literal_freqs[int(b)]++
			}
		}
		for b in second {
			if literal_freqs[int(b)] < 3 {
				literal_freqs[int(b)]++
			}
		}
		literal_lengths = compute_huffman_lengths(literal_freqs, 15)!
		write_prefix_tree(mut w, literal_lengths)!
	}

	mut cmd_symbols := [cmd_one, cmd_two]
	if cmd_one == cmd_two {
		cmd_symbols = [cmd_one]
	}
	sort_small(mut cmd_symbols)
	write_simple_prefix_tree_symbols(mut w, 704, cmd_symbols)!
	cmd_lengths := simple_prefix_lengths(704, cmd_symbols)!
	cmd_enc := huffman.build(
		lengths:   cmd_lengths
		max_bits:  max_length(cmd_lengths)
		bit_order: .lsb_first
	)!

	mut dist_symbols := [dist_one.symbol, dist_two.symbol]
	if dist_one.symbol == dist_two.symbol {
		dist_symbols = [dist_one.symbol]
	}
	sort_small(mut dist_symbols)
	write_simple_prefix_tree_symbols(mut w, 64, dist_symbols)!
	dist_lengths := simple_prefix_lengths(64, dist_symbols)!
	dist_enc := huffman.build(
		lengths:   dist_lengths
		max_bits:  max_length(dist_lengths)
		bit_order: .lsb_first
	)!

	literal_enc_lengths := if literal_symbols.len > 0 && literal_symbols.len <= 4 {
		simple_prefix_lengths(256, literal_symbols)!
	} else {
		literal_lengths
	}
	lit_enc := huffman.build(
		lengths:   literal_enc_lengths
		max_bits:  max_length(literal_enc_lengths)
		bit_order: .lsb_first
	)!
	for run_index in 0 .. 2 {
		pattern := if run_index == 0 { first } else { second }
		pattern_len := pattern.len
		run_len := if run_index == 0 { first_len } else { second_len }
		copy_len := run_len - pattern_len
		cmd_sym := if run_index == 0 { cmd_one } else { cmd_two }
		dist := if run_index == 0 { dist_one } else { dist_two }
		write_symbol_using_codes(mut w, cmd_enc.codes, cmd_enc.lengths, cmd_sym)
		cmd := cmd_lut(cmd_sym)
		w.write_bits(u32(pattern_len - cmd.insert_len_offset), cmd.insert_len_extra_bits)
		w.write_bits(u32(copy_len - cmd.copy_len_offset), cmd.copy_len_extra_bits)
		for b in pattern {
			write_symbol_using_codes(mut w, lit_enc.codes, lit_enc.lengths, int(b))
		}
		write_symbol_using_codes(mut w, dist_enc.codes, dist_enc.lengths, dist.symbol)
		w.write_bits(u32(dist.extra), dist.extra_bits)
	}
}
