import os
import time

const (
	vexe                = os.getenv('VEXE')
	source_file         = os.join_path(os.temp_dir(), 'generated_live_program.v')
	output_file         = os.join_path(os.temp_dir(), 'generated_live_program.output.txt')
	live_program_source = "
module main
import time

[live]
fn pmessage() {
	println('ORIGINAL')
}

fn main() {
	println('START')
	for i := 0; i<3*100; i++ {
		pmessage()
		time.sleep_ms(10)
	}
	println('END')
}
"
)

//
fn testsuite_begin() {
	if os.user_os() !in ['linux', 'solaris'] && os.getenv('FORCE_LIVE_TEST').len == 0 {
		eprintln('Testing the runtime behaviour of -live mode,')
		eprintln('is reliable only on Linux for now.')
		eprintln('You can still do it by setting FORCE_LIVE_TEST=1 .')
		exit(0)
	}
	os.write_file(source_file, live_program_source)
}

fn testsuite_end() {
	os.rm(source_file)
	eprintln('source: $source_file')
	eprintln('output: $output_file')
	$if !windows {
		os.system('cat $output_file | sort | uniq -c | sort -n')
	}
	println('---------------------------------------------------------------------------')
	output_lines := os.read_lines(output_file) or {
		return
	}
	mut histogram := map[string]int
	for line in output_lines {
		histogram[line] = histogram[line] + 1
	}
	for k, v in histogram {
		println('> found ${k} $v times.')
	}
	println('---------------------------------------------------------------------------')
	assert histogram['START'] > 0
	assert histogram['END'] > 0
	assert histogram['ORIGINAL'] > 0
	assert histogram['CHANGED'] + histogram['ANOTHER'] > 0
}

fn change_source(new string) {
	time.sleep_ms(250)
	eprintln('> change ORIGINAL to: $new')
	os.write_file(source_file, live_program_source.replace('ORIGINAL', new))
	time.sleep_ms(1000)
	eprintln('> done.')
}

//
fn test_live_program_can_be_compiled() {
	cmd := '$vexe -live run $source_file > $output_file &'
	eprintln('Compiling and running with: $cmd')
	res := os.system(cmd)
	eprintln('... running in the background')
	time.sleep_ms(1500)
	assert res == 0
}

fn test_live_program_can_be_changed_1() {
	change_source('CHANGED')
	assert true
}

fn test_live_program_can_be_changed_2() {
	change_source('ANOTHER')
	assert true
}

fn test_live_program_has_ended() {
	time.sleep_ms(3500)
	assert true
}
