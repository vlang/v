// vtest build: !windows
// 1brc.v - Multi-thread V 1BRC for WSL Linux, single file
// Compile: v -prod -cc gcc -skip-unused -no-bounds-checking -cflags "-std=c17 -march=native -mtune=native" .
// Run: ./v1brc measurements.txt [-n threads]
// 1.50-1.70s under `WSL windows 11 CPU i13700 32GB, GTX1080 8GB, SSD`
module main

import os
import runtime

#include <sys/mman.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

fn C.mmap(addr voidptr, length u64, prot int, flags int, fd int, offset i64) voidptr
fn C.munmap(addr voidptr, length u64) int
fn C.memchr(ptr voidptr, ch int, count u64) voidptr
fn C.memcpy(dst voidptr, src voidptr, n u64) voidptr
fn C.memcmp(a voidptr, b voidptr, n u64) int
fn C.madvise(addr voidptr, length u64, advice int) int
fn C.open(path &u8, flags int, mode int) int
fn C.close(fd int) int
fn C.fstat(fd int, buf voidptr) int
fn C.clock_gettime(clockid int, tp voidptr) int

const max_city_len = 64
const hash_cap = 1 << 12
const prot_read = 1
const map_shared = 1
const map_populate = 0x08000
const clock_monotonic = 1
const madv_sequential = 2
const madv_willneed = 3

struct Stat {
	sec  i64
	nsec i64
}

struct CityHashEntry {
mut:
	hash   u32
	length u8
	name   [max_city_len]u8
	min    i32
	max    i32
	sum    i64
	count  u32
}

struct CityHashMap {
mut:
	entries []CityHashEntry
	count   u32
}

fn new_hash_map() CityHashMap {
	return CityHashMap{
		entries: []CityHashEntry{len: int(hash_cap)}
	}
}

@[inline]
fn hash_bytes(addr &u8, offset u64, len u32) u32 {
	unsafe {
		if len >= 8 {
			val := *(&u64(&addr[offset]))
			mut h := u32(val) * 0x45d9f3b
			h ^= u32(val >> 32) * 0x45d9f3b
			h ^= h >> 16
			return h
		}
	}
	mut h := u32(2166136261)
	for i in u32(0) .. len {
		h = h ^ unsafe { u32(addr[offset + u64(i)]) }
		h = h * 16777619
	}
	return h
}

@[direct_array_access; inline]
fn (mut m CityHashMap) find_or_insert(addr &u8, offset u64, len u8) &CityHashEntry {
	if len == 0 || len > max_city_len {
		panic('invalid city length')
	}
	mut h := hash_bytes(addr, offset, u32(len))
	mut idx := int(h & (hash_cap - 1))
	for {
		mut e := &m.entries[idx]
		if e.count == 0 {
			e.hash = h
			e.length = len
			unsafe { C.memcpy(voidptr(&e.name[0]), voidptr(&addr[offset]), u64(len)) }
			e.min = 0x7fffffff
			e.max = 0x80000000
			e.sum = 0
			m.count++
			return e
		}
		if e.hash == h && e.length == len && unsafe {
			C.memcmp(voidptr(&e.name[0]), voidptr(&addr[offset]), u64(len))
		} == 0 {
			return e
		}
		idx = (idx + 1) & (hash_cap - 1)
	}
	return &m.entries[0]
}

fn format_value(value i32) string {
	if value < 0 {
		abs := -value
		return '-${abs / 10}.${abs % 10}'
	}
	return '${value / 10}.${value % 10}'
}

fn print_results(results CityHashMap, print_nicely bool) {
	mut output := []string{cap: int(results.count)}
	for e in results.entries {
		if e.count == 0 {
			continue
		}
		name := unsafe { tos(&e.name[0], int(e.length)) }
		mean := f64(e.sum) / f64(e.count) / 10.0
		output << '${name}=${format_value(e.min)}/${mean:.1f}/${format_value(e.max)}'
	}
	output.sort()
	if print_nicely {
		println(output.join('\n'))
	} else {
		println('{' + output.join(', ') + '}')
	}
}

@[direct_array_access; inline]
fn parse_temp(addr &u8, start u64, len int) i32 {
	unsafe {
		if len == 3 {
			// X.X
			return i32(addr[start] - 48) * 10 + i32(addr[start + 2] - 48)
		}
		if len == 4 {
			if addr[start] == `-` {
				// -X.X
				return -(i32(addr[start + 1] - 48) * 10 + i32(addr[start + 3] - 48))
			}
			// XX.X
			return i32(addr[start] - 48) * 100 + i32(addr[start + 1] - 48) * 10 + i32(addr[start +
				3] - 48)
		}
		// len == 5, -XX.X
		return -(i32(addr[start + 1] - 48) * 100 + i32(addr[start + 2] - 48) * 10 + i32(addr[start +
			4] - 48))
	}
}

@[direct_array_access]
fn process_chunk(addr &u8, from u64, to u64) CityHashMap {
	mut results := new_hash_map()
	mut pos := from

	for pos < to {
		remaining := to - pos
		p := unsafe { C.memchr(voidptr(&addr[int(pos)]), int(`;`), remaining) }
		if p == C.NULL {
			break
		}
		semi_off := unsafe { u64(p) - u64(voidptr(addr)) }

		city_start := pos
		city_len := u8(semi_off - pos)

		if semi_off + 1 >= to {
			break
		}
		// Temperature format: [-\]?X.X (3) | [-\]?XX.X (4-5)
		b0 := unsafe { addr[semi_off + 1] }
		temp_start := semi_off + 1
		mut temp_len := 0
		if b0 == `-` {
			if unsafe { addr[semi_off + 3] } == `.` {
				temp_len = 4 // -X.X
			} else {
				temp_len = 5 // -XX.X
			}
		} else {
			if unsafe { addr[semi_off + 2] } == `.` {
				temp_len = 3 // X.X
			} else {
				temp_len = 4 // XX.X
			}
		}
		mut nl_off := temp_start + u64(temp_len)
		if nl_off >= to {
			break
		}
		// Handle both \n and \r\n line endings
		mut c := unsafe { addr[nl_off] }
		if c == `\r` {
			nl_off++
			if nl_off >= to {
				break
			}
			c = unsafe { addr[nl_off] }
		}
		if c != `\n` {
			break
		}

		temp := parse_temp(addr, temp_start, temp_len)

		mut e := results.find_or_insert(addr, city_start, city_len)
		if temp > e.max {
			e.max = temp
		}
		if temp < e.min {
			e.min = temp
		}
		e.sum += i64(temp)
		e.count++

		pos = nl_off + 1
	}
	return results
}

fn combine_results(chunks []CityHashMap) CityHashMap {
	mut result := new_hash_map()
	for chunk in chunks {
		for e in chunk.entries {
			if e.count == 0 {
				continue
			}
			mut target := result.find_or_insert(&e.name[0], 0, e.length)
			target.sum += e.sum
			target.count += e.count
			if e.min < target.min {
				target.min = e.min
			}
			if e.max > target.max {
				target.max = e.max
			}
		}
	}
	return result
}

struct MemoryMappedFile {
	size u64
mut:
	data &u8
	fd   int
}

fn mmap_file(path string) MemoryMappedFile {
	mut mf := MemoryMappedFile{
		fd:   C.open(path.str, 0, 0)
		size: os.file_size(path)
		data: C.NULL
	}
	if mf.fd < 0 {
		panic('failed to open file: ${path}')
	}
	mf.data = &u8(C.mmap(C.NULL, mf.size, prot_read, map_shared | map_populate, mf.fd, 0))
	if voidptr(mf.data) == voidptr(-1) {
		C.close(mf.fd)
		panic('mmap failed')
	}
	C.madvise(mf.data, mf.size, madv_sequential)
	return mf
}

fn (mut mf MemoryMappedFile) unmap() {
	if C.munmap(mf.data, mf.size) != 0 {
		panic('munmap() failed')
	}
	C.close(mf.fd)
}

fn get_monotonic_ns() u64 {
	mut ts := Stat{}
	C.clock_gettime(clock_monotonic, voidptr(&ts))
	return u64(ts.sec) * 1_000_000_000 + u64(ts.nsec)
}

fn process_in_parallel(mf MemoryMappedFile, thread_count u32) CityHashMap {
	mut threads := []thread CityHashMap{}
	approx_chunk_size := mf.size / u64(thread_count)
	mut from := u64(0)
	mut to := approx_chunk_size

	for _ in 0 .. thread_count - 1 {
		if to >= mf.size {
			break
		}
		p := unsafe { C.memchr(voidptr(&mf.data[int(to)]), int(`\n`), mf.size - to) }
		if p == C.NULL {
			break
		}
		to = unsafe { u64(p) - u64(voidptr(mf.data)) + 1 }
		if to >= mf.size {
			break
		}
		threads << spawn process_chunk(mf.data, from, to)
		from = to
		to = from + approx_chunk_size
	}
	to = mf.size
	threads << spawn process_chunk(mf.data, from, to)

	return combine_results(threads.wait())
}

fn main() {
	if os.args.len < 2 {
		eprintln('Usage: ${os.args[0]} ~/data.txt [-n threads]')
		exit(1)
	}
	path := os.args[1]

	mut thread_count := u32(runtime.nr_cpus())
	if os.args.len >= 4 && os.args[2] == '-n' {
		thread_count = os.args[3].u32()
	}

	t_start := get_monotonic_ns()

	mut mf := mmap_file(path)
	defer {
		mf.unmap()
	}
	results := if thread_count > 1 {
		process_in_parallel(mf, thread_count)
	} else {
		process_chunk(mf.data, 0, mf.size)
	}

	t_end := get_monotonic_ns()
	dur_ns := t_end - t_start
	dur_ms := f64(dur_ns) / 1_000_000.0
	dur_s := f64(dur_ns) / 1_000_000_000.0

	print_results(results, false)
	eprintln('\n[WSL Linux Multi-thread Timing]')
	eprintln('CPU cores used: ${thread_count}')
	eprintln('Total wall time: ${dur_s:.3f} s (${dur_ms:.2f} ms)')
}
