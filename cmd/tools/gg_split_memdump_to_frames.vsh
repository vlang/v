#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

module main

import os
import log

fn write_chunk(n int, min_size int, original string, start int, end int) {
	size := end - start
	if size < min_size {
		return
	}
	frame_file := 'frame_${n:06}.bin'
	log.warn('writing ${frame_file}, from start: ${start:9}, to end: ${end:9} | size: ${size:9} >= ${min_size:9}, memuse: ${gc_memory_use()}')
	os.write_file(frame_file, original#[start..end]) or { log.error(err.str()) }
}

fn main() {
	log.info('Start.')
	fpath := os.args[2] or { 'memdump.bin' }
	separator := os.args[3] or { '@@ gg_memory_trace_frame' }
	min_size := os.args[1] or { '${separator.len}' }.int()
	log.info('Splitting chunk min_size (arg 1): ${min_size}, file (arg 2): ${fpath}, by string separator (arg 3): `${separator}` ...')
	log.info('Memory use before reading: ${gc_memory_use()}')
	original := os.read_file(fpath)!
	log.info('file size: ${original.len}')
	log.info('Memory use after reading: ${gc_memory_use()}')
	mut n := 0
	for start := 0; start < original.len; {
		gc_collect()
		idx := original.index_after_(separator, start + separator.len) // ensure that we advance each time
		if idx < 0 {
			write_chunk(n, min_size, original, start, original.len)
			break
		}
		write_chunk(n, min_size, original, start, idx)
		start = idx
		n++
	}
	log.info('Memory use after processing: ${gc_memory_use()}')
	log.info('Done.')
}
