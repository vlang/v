import os
import rand
import time
import rand.pcg32

const buf_len = os.getenv_opt('BUF_LEN') or { '100_000_000' }.int()
const nthreads = os.getenv_opt('VJOBS') or { '2' }.int()
const max_iterations = os.getenv_opt('MAX_ITERATIONS') or { '4' }.int()

fn main() {
	mut buf := []u8{len: buf_len}
	mut arr := []thread{}
	sw := time.new_stopwatch()
	for i in 0 .. nthreads {
		part_len := buf.len / nthreads
		start := i * part_len
		mut chunk := &WorkChunk{
			thread_id: i
			// make the last thread fill the remaining characters too:
			part: unsafe { buf[start..if i == nthreads - 1 { buf.len } else { start + part_len }] }
		}
		arr << spawn worker(mut chunk)
	}
	arr.wait()
	elapsed := sw.elapsed().milliseconds()
	mut histogram := []u64{len: 58}
	for b in buf {
		histogram[b]++
	}
	println('            buf: ${buf#[..10]} ... ${buf#[-10..]}')
	println('      histogram: ${histogram#[48..]}')
	println('Total took ${elapsed:6}ms, VJOBS: ${nthreads:2}, MAX_ITERATIONS: ${max_iterations:5}, BUF_LEN: ${buf_len:6}')
	println('')
}

struct WorkChunk {
	thread_id int
mut:
	part []u8
}

const charset = '0123456789'

fn worker(mut chunk WorkChunk) {
	mut rng := rand.PRNG(pcg32.PCG32RNG{})
	sw := time.new_stopwatch()
	for _ in 0 .. max_iterations {
		rng.fill_buffer_from_set(charset, mut chunk.part)
	}
	elapsed_time_ns := sw.elapsed().nanoseconds()
	fill_ms := u64(f64(elapsed_time_ns) / f64(max_iterations * 1_000_000))
	rand_character_ns := f64(elapsed_time_ns) / f64(max_iterations * chunk.part.len)
	println('      thread ${chunk.thread_id:2}, took ${elapsed_time_ns / 1_000_000:6}ms, per fill: ${fill_ms:6}ms, per character: ${rand_character_ns:5.0}ns, part: ${chunk.part#[..3]}...${chunk.part#[-3..]}, part.len: ${chunk.part.len:6}')
}
