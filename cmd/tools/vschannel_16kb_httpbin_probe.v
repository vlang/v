import flag
import net.http
import os
import time

const probe_url = 'https://httpbin.org/post'
const vschannel_16kb_boundary = 16 * 1024
const default_sizes_csv = '12000,15000,17000,24000,32768'

fn main() {
	if os.args.len >= 2 && os.args[1] == '--worker' {
		run_worker_mode()
		return
	}
	run_parent_mode()
}

fn run_worker_mode() {
	if os.args.len < 3 {
		eprintln('missing payload size for --worker')
		exit(2)
	}
	size := os.args[2].int()
	if size <= 0 {
		eprintln('invalid payload size `${os.args[2]}`')
		exit(2)
	}
	payload := '{"size":${size},"payload":"' + 'x'.repeat(size) + '"}'
	mut headers := http.new_header()
	headers.add(.content_type, 'application/json')
	mut req := http.Request{
		method: .post
		url: probe_url
		header: headers
		data: payload
		read_timeout: 60 * time.second
		write_timeout: 30 * time.second
	}
	resp := req.do() or {
		eprintln('request failed at size=${size}: ${err}')
		exit(1)
	}
	if resp.status_code != 200 {
		eprintln('unexpected status at size=${size}: ${resp.status_code}')
		exit(1)
	}
	if !resp.body.contains('httpbin.org/post') {
		eprintln('unexpected response body at size=${size}')
		exit(1)
	}
	println('OK size=${size} payload_bytes=${payload.len}')
}

fn run_parent_mode() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('vschannel_16kb_httpbin_probe')
	fp.version('0.0.1')
	fp.description('Probe net.http HTTPS POST behavior around 16KB payloads using https://httpbin.org/post.')
	fp.skip_executable()

	expect_mode := fp.string('expect', `e`, 'after',
		'Expected behavior mode: before|after|none. before expects >16KB failures, after expects all succeed.')
	sizes_csv := fp.string('sizes', `s`, default_sizes_csv, 'Comma separated payload sizes in bytes.')
	verbose := fp.bool('verbose', `v`, false, 'Print child process output for all sizes.')
	show_help := fp.bool('help', `h`, false, 'Show this help screen.')
	free_args := fp.finalize() or {
		eprintln('flag parse failed: ${err}')
		exit(2)
	}
	if free_args.len > 0 {
		eprintln('unexpected positional args: ${free_args}')
		exit(2)
	}
	if show_help {
		println(fp.usage())
		return
	}

	mode := expect_mode.to_lower()
	if mode !in ['before', 'after', 'none'] {
		eprintln('invalid --expect `${expect_mode}` (allowed: before|after|none)')
		exit(2)
	}
	sizes := parse_sizes_csv(sizes_csv) or {
		eprintln(err)
		exit(2)
	}

	self_exe := os.quoted_path(os.executable())
	println('probe url: ${probe_url}')
	println('expect mode: ${mode}')
	println('sizes: ${sizes}')

	mut mismatches := 0
	for size in sizes {
		cmd := '${self_exe} --worker ${size}'
		res := os.execute(cmd)
		success := res.exit_code == 0
		expected_success := expected_success_for_mode(mode, size)
		status := if success { 'OK' } else { 'FAIL' }
		expect_text := if expected_success { 'expect=OK' } else { 'expect=FAIL' }
		println('${status:4} size=${size:6} exit=${res.exit_code:3} ${expect_text}')
		if verbose || !success {
			output := res.output.trim_space()
			if output.len > 0 {
				println(output)
			}
		}
		if mode != 'none' && success != expected_success {
			mismatches++
		}
	}

	if mismatches > 0 {
		eprintln('mismatch count: ${mismatches}')
		exit(1)
	}
	println('probe completed without mismatches')
}

fn parse_sizes_csv(raw string) ![]int {
	mut sizes := []int{}
	for part in raw.split(',') {
		item := part.trim_space()
		if item.len == 0 {
			continue
		}
		size := item.int()
		if size <= 0 {
			return error('invalid size entry `${item}` in --sizes')
		}
		sizes << size
	}
	if sizes.len == 0 {
		return error('no sizes provided in --sizes')
	}
	return sizes
}

fn expected_success_for_mode(mode string, size int) bool {
	return match mode {
		'before' { size <= vschannel_16kb_boundary }
		'after' { true }
		else { true }
	}
}
