module hashmap

import math

[inline]
fn hm_next_power_of_two(_x i64) i64 {
    if _x == 0 {
        return 1
    }

    mut x := _x
    x--
    x |= x >> 1
    x |= x >> 2
    x |= x >> 4
    x |= x >> 8
    x |= x >> 16
    x |= x >> 32

    return x + 1
}

[inline]
fn hm_math_max(num_a, num_b int) int {
    if num_a > num_b {
        return num_a
    }

    return num_b
}

[inline]
fn hm_get_pot_array_size(expected int, factor f32) int {
    return hm_math_max(2, int(hm_next_power_of_two(i64(math.ceil(f32(expected) / factor)))))
}


[inline]
fn hm_phi_mix(x int) int {
    h := x * 0x9e3779b9
    return h ^ (h >> 16)
}

// imagine having to implement a hash map yourself just to store some tiles in an efficient way

struct IntHashMap {
mut:
    fill_factor f32
    threshold int
    _size int
    mask u32

    keys []int
    values []voidptr
    used []bool
}

const (
    HMDefSize = 16
    HMDefFillFactor = 0.75
)

[inline]
pub fn new_int_hashmap() IntHashMap {
    return new_int_hashmap_options(HMDefSize, HMDefFillFactor)
}

pub fn new_int_hashmap_options(size int, fill_factor f32) IntHashMap {
    mut hashmap := IntHashMap{}
    capacity := hm_get_pot_array_size(size, fill_factor)
    hashmap.mask = u32(capacity - 1)
    hashmap.fill_factor = fill_factor

    hashmap.keys = [0].repeat(capacity)
    hashmap.values = [voidptr(0)].repeat(capacity)
    hashmap.used = [false].repeat(capacity)   
    hashmap.threshold = int(f32(capacity) * f32(fill_factor))

    return hashmap
}

pub fn (hashmap &IntHashMap) get(key int) voidptr {
    idx := hashmap._read_index(key)

    if idx != -1 { // unsafe!
        return C.array__get(hashmap.values, idx)
    } else {
        return 0
    }
}

[inline]
pub fn (hashmap &IntHashMap) has(key int) bool {
    return hashmap._read_index(key) != -1
}

pub fn (hashmap mut IntHashMap) put(key int, value voidptr) {
    mut idx := hashmap._put_index(key)
    if idx < 0 {
        //println('no space, rehashing')
        hashmap._rehash(hashmap.keys.len * 2)
        idx = hashmap._put_index(key)
    }

    if !hashmap.used[idx] {
        hashmap.keys[idx] = key
        hashmap.values[idx] = value
        hashmap.used[idx] = true
        hashmap._size++

        if hashmap._size >= hashmap.threshold {
            //println('hit hashmap threshold, rehashing')
            hashmap._rehash(hashmap.keys.len * 2)
        }
    } else {
        hashmap.values[idx] = value
    }
}

[inline]
pub fn (hashmap &IntHashMap) size() int {
    return hashmap._size
}

pub fn (hashmap &IntHashMap) keys() []int {
    mut keys := [0].repeat(hashmap._size)
    mut kidx := 0

    for idx := 0; idx < hashmap.keys.len; idx++ {
        if hashmap.used[idx] {
            keys[kidx] = hashmap.keys[idx]
            kidx++
        }
    }

    return keys
}

// unsafe
pub fn (hashmap mut IntHashMap) remove(key int) voidptr {
    idx := hashmap._read_index(key)

    if idx == -1 {
        return 0
    }

    res := C.array__get(hashmap.values, idx)
    hashmap._size--
    hashmap._shift_keys(idx)
    return res
}

pub fn (hashmap mut IntHashMap) clear() {
    for idx := 0; idx < hashmap.keys.len; idx++ {
        if hashmap.used[idx] {
            hashmap._size--
            hashmap._shift_keys(idx)
        }    
    }
}

[inline]
fn (hashmap &IntHashMap) _start_index(key int) int {
    return int(u32(hm_phi_mix(key)) & hashmap.mask)
}

[inline]
fn (hashmap &IntHashMap) _next_index(key int) int {
    return int(u32(key + 1) & hashmap.mask)
}

[inline]
fn (hashmap &IntHashMap) _read_index(key int) int {
    mut idx := hashmap._start_index(key)

    if !hashmap.used[idx] {
        return -1
    }

    if hashmap.keys[idx] == key && hashmap.used[idx] {
        return idx
    }

    start_idx := idx
    for {
        idx = hashmap._next_index(idx)

        if idx == start_idx || !hashmap.used[idx] {
            return -1
        }

        if hashmap.keys[idx] == key && hashmap.used[idx] {
            return idx
        }
    }
    
    return -1
}

[inline]
fn (hashmap &IntHashMap) _put_index(key int) int {
    read_idx := hashmap._read_index(key)
    if read_idx >= 0 {
        return read_idx
    }

    start_idx := hashmap._start_index(key)
    mut idx := start_idx

    for {
        if !hashmap.used[idx] {
            break
        }

        idx = hashmap._next_index(key)
        if idx == start_idx {
            return -1
        }
    }

    return idx
}

fn (hashmap mut IntHashMap) _rehash(new_capacity int) {
    hashmap.threshold = int(f32(new_capacity) * f32(hashmap.fill_factor))
    hashmap.mask = u32(new_capacity - 1)

    old_capacity := hashmap.keys.len
    old_keys := hashmap.keys
    old_values := hashmap.values
    old_used := hashmap.used
    //println('rehash $old_capacity -> $new_capacity')
    
    hashmap.keys = [0].repeat(new_capacity)
    hashmap.values = [voidptr(0)].repeat(new_capacity)
    hashmap.used = [false].repeat(new_capacity)   
    hashmap._size = 0

    for i := old_capacity; i > 0; i-- {
        if old_used[i] {
            hashmap.put(old_keys[i], old_values[i])
        }
    }
}

fn (hashmap mut IntHashMap) _shift_keys(_pos int) int {
    mut last := 0
    mut k := 0
    mut pos := _pos

    for {
        last = pos
        pos = hashmap._next_index(pos)

        for {
            k = hashmap.keys[pos]
            if !hashmap.used[pos] {
                hashmap.keys[last] = 0
                hashmap.values[last] = voidptr(0)
                hashmap.used[last] = false

                return last
            }

            slot := hashmap._start_index(k)

            if last <= pos {
                if last >= slot || slot > pos {
                    break
                }
            } else {
                if last >= slot && slot > pos {
                    break
                }
            }

            pos = hashmap._next_index(pos)
        }

        hashmap.keys[last] = k
        hashmap.values[last] = hashmap.values[pos]
        hashmap.used[last] = hashmap.used[pos]
    }

    return -1

    //panic('should not happen!')
}