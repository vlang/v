// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module hashmap

import hash.wyhash

const (
	log_size = 5
	n_hashbits = 24
	window_size = 16
	initial_size = 1 << log_size
	initial_cap = initial_size - 1
	default_load_factor = 0.8
	hashbit_mask = u32(0xFFFFFF)
	probe_offset = u32(0x1000000)
	max_probe = u32(0xFF000000)
)

pub struct Hashmap {
mut:
	cap         u32
	shift       byte
	window      byte
	info        &u32
	key_values  &KeyValue
pub mut:
	load_factor f32
	size        int
}

struct KeyValue {
	key   string
mut:
	value int
}

pub fn new_hashmap() Hashmap {
	return Hashmap{
		cap: initial_cap
		shift: log_size
		window: window_size
		info: &u32(calloc(sizeof(u32) * initial_size))
		key_values: &KeyValue(calloc(sizeof(KeyValue) * initial_size))
		load_factor: default_load_factor
		size: 0
	}
}

pub fn (h mut Hashmap) set(key string, value int) {
	// load_factor can be adjusted.
	if (f32(h.size) / f32(h.cap)) > h.load_factor {
		h.rehash()
	}
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut info := u32(((hash >> h.shift) & hashbit_mask) | probe_offset)
	mut index := hash & h.cap
	// While probe count is less
	for info < h.info[index] {
		index = (index + 1) & h.cap
		info += probe_offset
	}
	// While we might have a match
	for info == h.info[index] {
		if key == h.key_values[index].key {
			h.key_values[index].value = value
			return
		}
		index = (index + 1) & h.cap
		info += probe_offset
	}
	// Match is not possible anymore.
	// Probe until an empty index is found.
	// Swap when probe count is higher/richer (Robin Hood).
	mut current_kv := KeyValue{key, value}
	for h.info[index] != 0 {
		if info > h.info[index] {
			// Swap info word
			tmp_info := h.info[index]
			h.info[index] = info
			info = tmp_info
			// Swap KeyValue
			tmp_kv := h.key_values[index]
			h.key_values[index] = current_kv
			current_kv = tmp_kv
		}
		index = (index + 1) & h.cap
		info += probe_offset
	}
	// Should almost never happen
	if (info & max_probe) == max_probe {
		h.rehash()
		h.set(current_kv.key, current_kv.value)
		return
	}
	h.info[index] = info
	h.key_values[index] = current_kv
	h.size++
}

fn (h mut Hashmap) rehash() {
	old_cap := h.cap
	h.window--
	// check if any hashbits are left
	if h.window == 0 {
		h.shift += window_size
	}
	// double the size of the hashmap
	h.cap = ((h.cap + 1) << 1) - 1
	mut new_key_values := &KeyValue(calloc(sizeof(KeyValue) * (h.cap + 1)))
	mut new_info := &u32(calloc(sizeof(u32) * (h.cap + 1)))
	for i in 0 .. (old_cap + 1) {
		if h.info[i] != 0 {
			mut kv := h.key_values[i]
			mut hash := u64(0)
			mut info := u32(0)
			if h.window == 0 {
				hash = wyhash.wyhash_c(kv.key.str, u64(kv.key.len), 0)
				info = u32(((hash >> h.shift) & hashbit_mask) | probe_offset)
			}
			else {
				original := u64(i - ((h.info[i] >> n_hashbits) - 1)) & (h.cap >> 1)
				hash = original | (h.info[i] << h.shift)
				info = (h.info[i] & hashbit_mask) | probe_offset
			}
			mut index := hash & h.cap
			// While probe count is less
			for info < new_info[index] {
				index = (index + 1) & h.cap
				info += probe_offset
			}
			// Probe until an empty index is found.
			// Swap when probe count is higher/richer (Robin Hood).
			for new_info[index] != 0 {
				if info > new_info[index] {
					// Swap info word
					tmp_info := new_info[index]
					new_info[index] = info
					info = tmp_info
					// Swap KeyValue
					tmp_kv := new_key_values[index]
					new_key_values[index] = kv
					kv = tmp_kv
				}
				index = (index + 1) & h.cap
				info += probe_offset
			}
			// Should almost never happen
			if (info & max_probe) == max_probe {
				h.rehash()
				h.set(kv.key, kv.value)
				return
			}
			new_info[index] = info
			new_key_values[index] = kv
		}
	}
	if h.window == 0 {
		h.window = window_size
	}
	free(h.key_values)
	free(h.info)
	h.key_values = new_key_values
	h.info = new_info
}

pub fn (h mut Hashmap) delete(key string) {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & h.cap
	mut info := u32(((hash >> h.shift) & hashbit_mask) | probe_offset)
	for info < h.info[index] {
		index = (index + 1) & h.cap
		info += probe_offset
	}
	// Perform backwards shifting
	for info == h.info[index] {
		if key == h.key_values[index].key {
			mut old_index := index
			index = (index + 1) & h.cap
			mut current_info := h.info[index]
			for (current_info >> n_hashbits) > 1 {
				h.info[old_index] = current_info - probe_offset
				h.key_values[old_index] = h.key_values[index]
				old_index = index
				index = (index + 1) & h.cap
				current_info = h.info[index]
			}
			h.info[old_index] = 0
			h.size--
			return
		}
		index = (index + 1) & h.cap
		info += probe_offset
	}
}

pub fn (h Hashmap) get(key string) int {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & h.cap
	mut info := u32(((hash >> h.shift) & hashbit_mask) | probe_offset)
	for info < h.info[index] {
		index = (index + 1) & h.cap
		info += probe_offset
	}
	for info == h.info[index] {
		if key == h.key_values[index].key {
			return h.key_values[index].value
		}
		index = (index + 1) & h.cap
		info += probe_offset
	}
	return 0
}

pub fn (h Hashmap) exists(key string) bool {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & h.cap
	mut info := u32(((hash >> h.shift) & hashbit_mask) | probe_offset)
	for info < h.info[index] {
		index = (index + 1) & h.cap
		info += probe_offset
	}
	for info == h.info[index] {
		if key == h.key_values[index].key {
			return true
		}
		index = (index + 1) & h.cap
		info += probe_offset
	}
	return false
}

pub fn (h Hashmap) keys() []string {
	mut keys := [''].repeat(h.size)
	mut j := 0
	for i in 0 .. (h.cap + 1) {
		if h.info[i] != 0 {
			keys[j] = h.key_values[i].key
			j++
		}
	}
	return keys
}
