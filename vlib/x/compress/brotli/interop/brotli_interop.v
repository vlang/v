module main

import os
import time
import compress.brotli as old_brotli
import x.compress.brotli as new_brotli

struct Case {
	quality     int
	window_size int
}

struct BenchmarkInput {
	name string
	data []u8
}

const benchmark_warmup_runs = 1
const benchmark_runs = 5
const benchmark_target_quality = 5
const benchmark_target_window = 22

struct TableRow {
	values []string
}

struct TableHeader {
	lines []string
}

fn median_microseconds(mut samples []i64) i64 {
	for index in 1 .. samples.len {
		value := samples[index]
		mut insert_at := index
		for insert_at > 0 && samples[insert_at - 1] > value {
			samples[insert_at] = samples[insert_at - 1]
			insert_at--
		}
		samples[insert_at] = value
	}
	return samples[samples.len / 2]
}

fn benchmark_inputs() []BenchmarkInput {
	base_text := 'V Brotli interop payload: the quick brown fox jumps over the lazy dog.\n'
	static_dictionary_words := ('the quick brown fox jumps over the lazy dog. ' +
		'compression dictionary words and transformed dictionary entries. ').repeat(128).bytes()
	custom_dictionary_words := ('custom dictionary prefix: '.repeat(8) +
		'dictionary-backed payload words appear here. ').repeat(128).bytes()
	mut pseudo_random := []u8{len: 65536}
	mut small_alphabet := []u8{len: 65536}
	for index in 0 .. pseudo_random.len {
		pseudo_random[index] = u8((index * 73 + index / 17 * 19 + 41) & 255)
		small_alphabet[index] = u8((index * 3 + index / 11) & 3)
	}
	pattern := '0123456789abcdefghijklmnopqrstuvwxyz'.bytes()
	mut json_like := []u8{}
	for index in 0 .. 2048 {
		json_like << '{"id":${index},"name":"item-${index % 31}","active":${index % 2 == 0}}\n'.bytes()
	}
	mut exact_boundary := []u8{len: 65536}
	mut boundary := []u8{len: 65537}
	for index in 0 .. boundary.len {
		value := u8((index * 37 + 5) & 255)
		boundary[index] = value
		if index < exact_boundary.len {
			exact_boundary[index] = value
		}
	}
	return [
		BenchmarkInput{ name: 'empty', data: []u8{} },
		BenchmarkInput{ name: 'tiny', data: 'tiny Brotli input'.bytes() },
		BenchmarkInput{ name: 'repetitive-text', data: (base_text.repeat(256) + '0123456789 abcdefghijklmnopqrstuvwxyz\n'.repeat(128)).bytes() },
		BenchmarkInput{ name: 'natural-text', data: (base_text + 'Compression compares repeated structures while preserving every byte.\n').repeat(512).bytes() },
		BenchmarkInput{ name: 'json-like', data: json_like },
		BenchmarkInput{ name: 'pseudo-random', data: pseudo_random },
		BenchmarkInput{ name: 'small-alphabet', data: small_alphabet },
		BenchmarkInput{ name: 'periodic', data: pattern.repeat(2048) },
		BenchmarkInput{ name: 'static-dictionary-words', data: static_dictionary_words },
		BenchmarkInput{ name: 'custom-dictionary-words', data: custom_dictionary_words },
		BenchmarkInput{ name: 'exact-64k-boundary', data: exact_boundary },
		BenchmarkInput{ name: 'block-boundary', data: boundary },
	]
}

fn pad_table_value(value string, width int, right_align bool) string {
	padding := ' '.repeat(width - value.len)
	return if right_align { padding + value } else { value + padding }
}

fn print_table(headers []TableHeader, rows []TableRow, right_aligned []bool) {
	mut widths := []int{len: headers.len}
	for index, header in headers {
		for line in header.lines {
			if line.len > widths[index] {
				widths[index] = line.len
			}
		}
	}
	for row in rows {
		for index, value in row.values {
			if value.len > widths[index] {
				widths[index] = value.len
			}
		}
	}
	mut border := '+'
	for width in widths {
		border += '-'.repeat(width + 2) + '+'
	}
	println(border)
	for line_index in 0 .. 2 {
		mut header_line := '|'
		for index, header in headers {
			line := if line_index < header.lines.len { header.lines[line_index] } else { '' }
			header_line += ' ${pad_table_value(line, widths[index], false)} |'
		}
		println(header_line)
	}
	println(border)
	for row in rows {
		mut row_line := '|'
		for index, value in row.values {
			row_line += ' ${pad_table_value(value, widths[index], right_aligned[index])} |'
		}
		println(row_line)
	}
	println(border)
}

fn is_representative_benchmark(name string) bool {
	return name !in ['empty', 'tiny', 'pseudo-random']
}

fn size_delta_percent(size int, baseline int) f64 {
	if baseline == 0 {
		return 0.0
	}
	return 100.0 * f64(size - baseline) / f64(baseline)
}

fn throughput_mib_per_second(input_size int, elapsed_us i64) f64 {
	if elapsed_us <= 0 {
		return 0.0
	}
	return f64(input_size) / f64(elapsed_us) / 1_048_576.0 * 1_000_000.0
}

fn run_cli(brotli_path string, input_path string, output_path string, c Case) !time.Duration {
	started := time.new_stopwatch()
	result := os.exec([brotli_path, '-f', '-q', '${c.quality}', '-w', '${c.window_size}', '-o',
		output_path, input_path])
	if result.exit_code != 0 {
		return error('brotli failed: ${result.output}')
	}
	return started.elapsed()
}

fn benchmark_old(data []u8, c Case) !([]u8, i64) {
	mut output := []u8{}
	for _ in 0 .. benchmark_warmup_runs {
		output = old_brotli.compress(data, quality: c.quality, lgwin: c.window_size)!
	}
	mut samples := []i64{cap: benchmark_runs}
	for _ in 0 .. benchmark_runs {
		started := time.new_stopwatch()
		output = old_brotli.compress(data, quality: c.quality, lgwin: c.window_size)!
		samples << started.elapsed().microseconds()
	}
	return output, median_microseconds(mut samples)
}

fn benchmark_new(data []u8, c Case) !([]u8, i64) {
	mut output := []u8{}
	for _ in 0 .. benchmark_warmup_runs {
		output = new_brotli.compress(data, quality: c.quality, window_size: c.window_size)!
	}
	mut samples := []i64{cap: benchmark_runs}
	for _ in 0 .. benchmark_runs {
		started := time.new_stopwatch()
		output = new_brotli.compress(data, quality: c.quality, window_size: c.window_size)!
		samples << started.elapsed().microseconds()
	}
	return output, median_microseconds(mut samples)
}

fn benchmark_cli(brotli_path string, input_path string, output_path string, c Case) !([]u8, i64) {
	for _ in 0 .. benchmark_warmup_runs {
		run_cli(brotli_path, input_path, output_path, c)!
	}
	mut samples := []i64{cap: benchmark_runs}
	for _ in 0 .. benchmark_runs {
		samples << run_cli(brotli_path, input_path, output_path, c)!.microseconds()
	}
	return os.read_bytes(output_path)!, median_microseconds(mut samples)
}

fn check_system_decodes(system_path string, compressed []u8, input []u8, workdir string, name string) bool {
	compressed_path := os.join_path(workdir, '${name}.br')
	decoded_path := os.join_path(workdir, '${name}.decoded')
	os.write_file_array(compressed_path, compressed) or { return false }
	result := os.exec([system_path, '-d', '-f', '-o', decoded_path, compressed_path])
	if result.exit_code != 0 {
		eprintln('system decoder error for ${name}: ${result.output.trim_space()}')
		return false
	}
	return (os.read_bytes(decoded_path) or { []u8{} }) == input
}

fn check_v_decodes(compressed []u8, input []u8) bool {
	decoded := new_brotli.decompress(compressed) or {
		eprintln('V decoder error: ${err.msg()}')
		return false
	}
	if decoded.len != input.len {
		eprintln('V decoder length mismatch: got ${decoded.len}, want ${input.len}')
	}
	return decoded == input
}

fn check_system_brotli_decodes(compressed []u8, input []u8) bool {
	decoded := old_brotli.decompress(compressed) or { return false }
	return decoded == input
}

fn main() {
	brotli_path := os.find_abs_path_of_executable('brotli') or {
		eprintln('brotli command not found; install Brotli before running this comparison')
		exit(1)
	}
	if !old_brotli.is_available() {
		eprintln('libbrotli encoder/decoder libraries not found; install Brotli before running this comparison')
		exit(1)
	}
	base_tmp := os.join_path(os.temp_dir(), 'v_brotli_interop')
	workdir := os.join_path(base_tmp, 'run_${os.getpid()}')
	os.mkdir_all(workdir) or {
		eprintln('could not create work directory: ${err.msg()}')
		exit(1)
	}
	defer {
		os.rmdir_all(workdir) or {}
	}

	benchmark_data := benchmark_inputs()
	mut compatibility_failures := 0
	mut size_regressions := 0
	enforce_size_gate := '--check-size' in os.args
	input := benchmark_data[2].data
	input_path := os.join_path(workdir, 'input.bin')
	os.write_file_array(input_path, input) or {
		eprintln('could not write input: ${err.msg()}')
		exit(1)
	}

	println('timing: ${benchmark_warmup_runs} warmup, ${benchmark_runs} measured runs, median microseconds')
	println('CLI timings include process-launch and file-I/O overhead')
	println('size gate: new <= old at q${benchmark_target_quality}/w${benchmark_target_window} for representative corpora')
	println('csv,corpus,case,input_size,old_size,old_us,old_mib_per_s,new_size,new_us,new_mib_per_s,cli_size,cli_us,cli_mib_per_s,old_minus_cli_size,new_minus_cli_size,old_minus_cli_us,new_minus_cli_us,new_size_delta_pct,old_decodes_cli,new_decodes_cli,cli_decodes_old,cli_decodes_new,size_gate')
	uname := os.exec(['uname', '-srmo']).output.trim_space().replace(',', ';')
	v_version := os.exec([@VEXE, 'version']).output.trim_space().replace(',', ';')
	cli_version := os.exec([brotli_path, '--version']).output.trim_space().replace(',', ';')
	println('environment,uname=${uname},v=${v_version},cli=${cli_version}')
	for benchmark_input in benchmark_data {
		benchmark_input_path := os.join_path(workdir, '${benchmark_input.name}.input')
		os.write_file_array(benchmark_input_path, benchmark_input.data) or {
			eprintln('could not write ${benchmark_input.name}: ${err.msg()}')
			exit(1)
		}
		headers := [
			TableHeader{ lines: ['corpus'] },
			TableHeader{ lines: ['case'] },
			TableHeader{ lines: ['old', 'size'] },
			TableHeader{ lines: ['old', 'us'] },
			TableHeader{ lines: ['old', 'MiB/s'] },
			TableHeader{ lines: ['new', 'size'] },
			TableHeader{ lines: ['new', 'us'] },
			TableHeader{ lines: ['new', 'MiB/s'] },
			TableHeader{ lines: ['CLI', 'size'] },
			TableHeader{ lines: ['CLI', 'us'] },
			TableHeader{ lines: ['CLI', 'MiB/s'] },
			TableHeader{ lines: ['old-CLI', 'size'] },
			TableHeader{ lines: ['new-CLI', 'size'] },
			TableHeader{ lines: ['old-CLI', 'us'] },
			TableHeader{ lines: ['new-CLI', 'us'] },
			TableHeader{ lines: ['new size', '% delta'] },
			TableHeader{ lines: ['old<-', 'CLI'] },
			TableHeader{ lines: ['new<-', 'CLI'] },
			TableHeader{ lines: ['CLI<-', 'old'] },
			TableHeader{ lines: ['CLI<-', 'new'] },
			TableHeader{ lines: ['size', 'gate'] },
		]
		mut rows := []TableRow{}
		for c in [
			Case{ quality: 0, window_size: 16 },
			Case{ quality: 5, window_size: 16 },
			Case{ quality: 11, window_size: 16 },
			Case{ quality: 0, window_size: 22 },
			Case{ quality: 5, window_size: 22 },
			Case{ quality: 11, window_size: 22 },
			Case{ quality: 0, window_size: 24 },
			Case{ quality: 5, window_size: 24 },
			Case{ quality: 11, window_size: 24 },
		] {
			old_output, old_us := benchmark_old(benchmark_input.data, c) or {
				eprintln('old Brotli compression failed for ${benchmark_input.name} q${c.quality}/w${c.window_size}: ${err.msg()}')
				continue
			}
			new_output, new_us := benchmark_new(benchmark_input.data, c) or {
				eprintln('new Brotli compression failed for ${benchmark_input.name} q${c.quality}/w${c.window_size}: ${err.msg()}')
				continue
			}
			cli_path := os.join_path(workdir, 'cli_${benchmark_input.name}_q${c.quality}_w${c.window_size}.br')
			cli_output, cli_us := benchmark_cli(brotli_path, benchmark_input_path, cli_path, c) or {
				eprintln('CLI compression failed for ${benchmark_input.name} q${c.quality}/w${c.window_size}: ${err.msg()}')
				continue
			}
			old_cli := if check_system_brotli_decodes(cli_output, benchmark_input.data) {
				'yes'
			} else {
				'no'
			}
			new_cli := if check_v_decodes(cli_output, benchmark_input.data) { 'yes' } else { 'no' }
			cli_old := if check_system_decodes(brotli_path, old_output, benchmark_input.data, workdir,
				'old_${benchmark_input.name}_q${c.quality}_w${c.window_size}') {
				'yes'
			} else {
				'no'
			}
			cli_new := if check_system_decodes(brotli_path, new_output, benchmark_input.data, workdir,
				'new_${benchmark_input.name}_q${c.quality}_w${c.window_size}') {
				'yes'
			} else {
				'no'
			}
			if old_cli != 'yes' || new_cli != 'yes' || cli_old != 'yes' || cli_new != 'yes' {
				compatibility_failures++
				eprintln('compatibility gate failed for ${benchmark_input.name} q${c.quality}/w${c.window_size}')
			}
			old_mib_per_s := throughput_mib_per_second(benchmark_input.data.len, old_us)
			new_mib_per_s := throughput_mib_per_second(benchmark_input.data.len, new_us)
			cli_mib_per_s := throughput_mib_per_second(benchmark_input.data.len, cli_us)
			new_size_delta_pct := size_delta_percent(new_output.len, old_output.len)
			size_gate := if c.quality == benchmark_target_quality
				&& c.window_size == benchmark_target_window && is_representative_benchmark(benchmark_input.name) {
				if new_output.len <= old_output.len {
					'pass'
				} else {
					size_regressions++
					eprintln('size gate regression for ${benchmark_input.name}: new ${new_output.len} > old ${old_output.len}')
					'fail'
				}
			} else {
				'not-targeted'
			}
			rows << TableRow{
				values: [
					benchmark_input.name,
					'q${c.quality}/w${c.window_size}',
					'${old_output.len}',
					'${old_us}',
					'${old_mib_per_s:0.2f}',
					'${new_output.len}',
					'${new_us}',
					'${new_mib_per_s:0.2f}',
					'${cli_output.len}',
					'${cli_us}',
					'${cli_mib_per_s:0.2f}',
					'${old_output.len - cli_output.len}',
					'${new_output.len - cli_output.len}',
					'${old_us - cli_us}',
					'${new_us - cli_us}',
					'${new_size_delta_pct:0.2f}',
					old_cli,
					new_cli,
					cli_old,
					cli_new,
					size_gate,
				]
			}
			println('csv,${benchmark_input.name},q${c.quality}/w${c.window_size},${benchmark_input.data.len},${old_output.len},${old_us},${old_mib_per_s:0.2f},${new_output.len},${new_us},${new_mib_per_s:0.2f},${cli_output.len},${cli_us},${cli_mib_per_s:0.2f},${old_output.len - cli_output.len},${new_output.len - cli_output.len},${old_us - cli_us},${new_us - cli_us},${new_size_delta_pct:0.2f},${old_cli},${new_cli},${cli_old},${cli_new},${size_gate}')
		}
		print_table(headers, rows, [false, false, true, true, true, true, true, true, true, true,
			true, true, true, true, true, true, true, false, false, false, false, false])
	}

	comment_path := os.join_path(workdir, 'cli_comment.br')
	comment_result := os.exec([brotli_path, '-f', '-C', 'SGVsbG8=', '-q', '5', '-o', comment_path,
		input_path])
	comment_stream := os.read_bytes(comment_path) or { []u8{} }
	comment_decoded := new_brotli.decompress(comment_stream, comment: 'Hello'.bytes()) or { []u8{} }
	v_comment := new_brotli.compress(input, comment: 'Hello'.bytes()) or { []u8{} }
	comment_cli_ok := comment_result.exit_code == 0 && comment_decoded == input
	comment_v_ok := check_system_decodes(brotli_path, v_comment, input, workdir, 'v_comment')
	if !comment_cli_ok || !comment_v_ok {
		compatibility_failures++
		eprintln('compatibility gate failed for comment option')
	}
	println('comment: CLI exit=${comment_result.exit_code}, V<-CLI=${comment_cli_ok}, CLI<-V=${comment_v_ok}')

	large_path := os.join_path(workdir, 'cli_large_window.br')
	large_result := os.exec([brotli_path, '-f', '--large_window=25', '-q', '5', '-o', large_path,
		input_path])
	large_stream := os.read_bytes(large_path) or { []u8{} }
	large_decoded := new_brotli.decompress(large_stream) or {
		eprintln('large-window V decode: ${err.msg()}')
		[]u8{}
	}
	v_large := new_brotli.compress(input, large_window: 25) or { []u8{} }
	large_v_ok := large_result.exit_code == 0 && large_decoded == input
	large_cli_ok := check_system_decodes(brotli_path, v_large, input, workdir, 'v_large_window')
	if !large_v_ok || !large_cli_ok {
		compatibility_failures++
		eprintln('compatibility gate failed for large-window option')
	}
	println('large-window: CLI exit=${large_result.exit_code}, V<-CLI=${large_v_ok}, CLI<-V=${large_cli_ok}')

	dictionary := 'custom dictionary prefix: '.repeat(8).bytes()
	dictionary_input := dictionary.clone()
	dictionary_path := os.join_path(workdir, 'dictionary.bin')
	dictionary_input_path := os.join_path(workdir, 'dictionary_input.bin')
	os.write_file_array(dictionary_path, dictionary) or { exit(1) }
	os.write_file_array(dictionary_input_path, dictionary_input) or { exit(1) }
	dictionary_output_path := os.join_path(workdir, 'cli_dictionary.br')
	dictionary_result := os.exec([brotli_path, '-f', '-D', dictionary_path, '-q', '5', '-o',
		dictionary_output_path, dictionary_input_path])
	dictionary_stream := os.read_bytes(dictionary_output_path) or { []u8{} }
	dictionary_decoded := new_brotli.decompress(dictionary_stream, dictionary: dictionary) or { []u8{} }
	v_dictionary := new_brotli.compress(dictionary_input, dictionary: dictionary) or { []u8{} }
	dictionary_v_path := os.join_path(workdir, 'v_dictionary.br')
	dictionary_v_decoded_path := os.join_path(workdir, 'v_dictionary.decoded')
	os.write_file_array(dictionary_v_path, v_dictionary) or { exit(1) }
	dictionary_cli_decode := os.exec([brotli_path, '-d', '-f', '-D', dictionary_path, '-o',
		dictionary_v_decoded_path, dictionary_v_path])
	dictionary_cli_ok := dictionary_cli_decode.exit_code == 0 && (os.read_bytes(dictionary_v_decoded_path) or { []u8{} }) == dictionary_input
	dictionary_v_ok := dictionary_result.exit_code == 0 && dictionary_decoded == dictionary_input
	if !dictionary_v_ok || !dictionary_cli_ok {
		compatibility_failures++
		eprintln('compatibility gate failed for custom dictionary option')
	}
	println('dictionary: input=${dictionary_input.len}, v_size=${v_dictionary.len}, cli_size=${dictionary_stream.len}, CLI exit=${dictionary_result.exit_code}, V<-CLI=${dictionary_v_ok}, CLI<-V=${dictionary_cli_ok}')

	file_input_path := os.join_path(workdir, 'file_options.input')
	os.write_file_array(file_input_path, input) or { exit(1) }

	suffix_source_path := os.join_path(workdir, 'suffix_source.bin')
	os.write_file_array(suffix_source_path, input) or { exit(1) }
	suffix_result := os.exec([brotli_path, '-S', '.custom', '-k', suffix_source_path])
	suffix_output_path := '${suffix_source_path}.custom'
	suffix_stream := os.read_bytes(suffix_output_path) or { []u8{} }
	suffix_ok := suffix_result.exit_code == 0 && os.exists(suffix_source_path)
	suffix_decoded := check_system_decodes(brotli_path, suffix_stream, input, workdir, 'suffix')
	if !suffix_ok || !suffix_decoded {
		compatibility_failures++
		eprintln('compatibility gate failed for suffix option')
	}
	println('suffix: CLI exit=${suffix_result.exit_code}, source-kept=${suffix_ok}, CLI-decodes=${suffix_decoded}')

	mut rm_source_path := os.join_path(workdir, 'rm_source.bin')
	os.write_file_array(rm_source_path, input) or { exit(1) }
	rm_result := os.exec([brotli_path, '-j', rm_source_path])
	rm_output_path := '${rm_source_path}.br'
	rm_stream := os.read_bytes(rm_output_path) or { []u8{} }
	rm_removed := !os.exists(rm_source_path)
	rm_decoded := check_system_decodes(brotli_path, rm_stream, input, workdir, 'rm')
	if rm_result.exit_code != 0 || !rm_removed || !rm_decoded {
		compatibility_failures++
		eprintln('compatibility gate failed for remove-source option')
	}
	println('rm: CLI exit=${rm_result.exit_code}, source-removed=${rm_removed}, CLI-decodes=${rm_decoded}')

	stdout_result := os.exec([brotli_path, '-c', file_input_path])
	stdout_stream := stdout_result.output.bytes()
	stdout_path := os.join_path(workdir, 'stdout.br')
	os.write_file_array(stdout_path, stdout_stream) or { exit(1) }
	stdout_decoded := check_system_decodes(brotli_path, stdout_stream, input, workdir, 'stdout')
	if stdout_result.exit_code != 0 || !stdout_decoded {
		compatibility_failures++
		eprintln('compatibility gate failed for stdout option')
	}
	println('stdout: CLI exit=${stdout_result.exit_code}, CLI-decodes=${stdout_decoded}')

	test_path := os.join_path(workdir, 'test_option.br')
	test_compress_result := os.exec([brotli_path, '-f', '-o', test_path, file_input_path])
	test_result := os.exec([brotli_path, '-t', test_path])
	mut corrupt_stream := os.read_bytes(test_path) or { []u8{} }
	if corrupt_stream.len > 0 {
		corrupt_stream.delete(corrupt_stream.len - 1)
	}
	corrupt_path := os.join_path(workdir, 'test_option_corrupt.br')
	os.write_file_array(corrupt_path, corrupt_stream) or { exit(1) }
	corrupt_test_result := os.exec([brotli_path, '-t', corrupt_path])
	test_ok := test_compress_result.exit_code == 0 && test_result.exit_code == 0 && corrupt_test_result.exit_code != 0
	if !test_ok {
		compatibility_failures++
		eprintln('compatibility gate failed for test option')
	}
	println('test: compress-exit=${test_compress_result.exit_code}, valid-exit=${test_result.exit_code}, corrupt-rejected=${corrupt_test_result.exit_code != 0}')

	overwrite_path := os.join_path(workdir, 'overwrite.br')
	first_overwrite := os.exec([brotli_path, '-f', '-o', overwrite_path, file_input_path])
	refused_overwrite := os.exec([brotli_path, '-o', overwrite_path, file_input_path])
	forced_overwrite := os.exec([brotli_path, '-f', '-o', overwrite_path, file_input_path])
	overwrite_ok := first_overwrite.exit_code == 0 && refused_overwrite.exit_code != 0 && forced_overwrite.exit_code == 0
	if !overwrite_ok {
		compatibility_failures++
		eprintln('compatibility gate failed for overwrite option')
	}
	println('overwrite: initial=${first_overwrite.exit_code}, refused-without-force=${refused_overwrite.exit_code != 0}, forced=${forced_overwrite.exit_code}')
	if compatibility_failures > 0 || enforce_size_gate && size_regressions > 0 {
		eprintln('interop gates failed: compatibility=${compatibility_failures}, size=${size_regressions}')
		exit(1)
	}
}
