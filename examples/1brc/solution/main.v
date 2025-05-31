// vtest build: !windows // requires mmap
import flag
import math
import os

#include <sys/mman.h>

fn C.mmap(addr voidptr, len u64, prot int, flags int, fd int, offset i64) voidptr
fn C.munmap(addr voidptr, len u64) int

struct MemoryMappedFile {
	size u64
mut:
	data &u8
	file os.File
}

fn mmap_file(path string) MemoryMappedFile {
	mut mf := MemoryMappedFile{
		file: os.open_file(path, 'r', 0) or { panic('fail') }
		size: os.file_size(path)
		data: C.NULL
	}

	mf.data = &u8(C.mmap(C.NULL, mf.size, C.PROT_READ, C.MAP_SHARED, mf.file.fd, 0))
	return mf
}

fn (mut mf MemoryMappedFile) unmap() {
	if C.munmap(mf.data, mf.size) != 0 {
		panic('(${C.errno}) munmap() failed')
	}
	mf.file.close()
}

enum ReadState {
	city
	temp
}

struct Result {
pub mut:
	min   i32
	max   i32
	sum   i32
	count u32
}

fn format_value(value i32) string {
	return '${value / 10}.${math.abs(value % 10)}'
}

fn print_results(results map[string]Result, print_nicely bool) {
	mut output := []string{cap: results.len}
	mut cities := results.keys()
	cities.sort()
	for city in cities {
		v := results[city]
		mean := f64(v.sum) / v.count / 10
		output << '${city}=${format_value(v.min)}/${mean:.1f}/${format_value(v.max)}'
	}
	if print_nicely {
		println(output.join('\n'))
	} else {
		println('{' + output.join(', ') + '}')
	}
}

fn combine_results(results []map[string]Result) map[string]Result {
	mut combined_result := map[string]Result{}
	for result in results {
		for city, r in result {
			if city !in combined_result {
				combined_result[city] = r
			} else {
				if r.max > combined_result[city].max {
					combined_result[city].max = r.max
				}
				if r.min < combined_result[city].min {
					combined_result[city].min = r.min
				}
				combined_result[city].sum += r.sum
				combined_result[city].count += r.count
			}
		}
	}
	return combined_result
}

@[direct_array_access]
fn process_chunk(addr &u8, from u64, to u64) map[string]Result {
	mut results := map[string]Result{}
	mut state := ReadState.city
	mut city := ''
	mut temp := i32(0)
	mut mod := i32(1)
	mut j := int(0)
	for i in from .. to {
		c := unsafe { u8(addr[i]) }
		match state {
			.city {
				match c {
					`;` {
						state = .temp
						city = unsafe { tos(addr[i - u64(j)], j) }
					}
					else {
						j += 1
					}
				}
			}
			.temp {
				match c {
					`\n` {
						temp *= mod
						if city !in results {
							results[city] = Result{
								min:   temp
								max:   temp
								sum:   temp
								count: 1
							}
						} else {
							if temp > results[city].max {
								results[city].max = temp
							}
							if temp < results[city].min {
								results[city].min = temp
							}
							results[city].sum += temp
							results[city].count += 1
						}
						state = .city
						temp = 0
						mod = 1
						j = 0
					}
					`-` {
						mod = -1
					}
					`.` {}
					else {
						// ASCII 48 = '0' ... ASCII 57 = '9' => (ASCII value) - 48 = decimal value
						temp = temp * 10 + (c - 48)
					}
				}
			}
		}
	}
	return results
}

fn process_in_parallel(mf MemoryMappedFile, thread_count u32) map[string]Result {
	mut threads := []thread map[string]Result{}
	approx_chunk_size := mf.size / thread_count
	mut from := u64(0)
	mut to := approx_chunk_size
	for _ in 0 .. thread_count - 1 {
		unsafe {
			for mf.data[to] != `\n` {
				to += 1
			}
		}
		threads << spawn process_chunk(mf.data, from, to)
		from = to + 1
		to = from + approx_chunk_size
	}
	to = mf.size
	threads << spawn process_chunk(mf.data, from, to)
	res := threads.wait()
	return combine_results(res)
}

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.version('1brc v1.0.0')
	fp.skip_executable()
	fp.application('1 billion rows challenge')
	fp.description('The 1 billion rows challenge solved in V.\nFor details, see https://www.morling.dev/blog/one-billion-row-challenge/')
	thread_count := u32(fp.int('threads', `n`, 1, 'number of threads for parallel processing.'))
	print_nicely := fp.bool('human-readable', `h`, false, 'Print results with new lines rather than following challenge spec')
	quiet := fp.bool('quiet', `q`, false, 'Suppress the results output (e.g., if you only care about timing)')
	fp.limit_free_args_to_exactly(1)!
	path := fp.remaining_parameters()[0]

	mut mf := mmap_file(path)
	defer {
		mf.unmap()
	}

	results := if thread_count > 1 {
		process_in_parallel(mf, thread_count)
	} else {
		process_chunk(mf.data, 0, mf.size)
	}

	if !quiet {
		print_results(results, print_nicely)
	}
}
