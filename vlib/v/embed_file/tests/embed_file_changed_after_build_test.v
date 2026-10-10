// A development build keeps only the path of an `$embed_file`, and loads the file
// when its bytes are first asked for. The size that the compiler recorded is the
// size the file had at compile time, so a file that was edited since then used to
// be cut to its old size, or read past its end.
//
// The compiler embeds its C headers, which turned that into a broken compiler. A
// `git pull` that added one declaration to `manual_stdlib_c_headers.h` left the `v`
// that had to rebuild itself writing a header that stopped 77 bytes short, inside
// an enum, into every program it compiled:
//
//	error: expected ',' or '}' before 'void'
//	error: unterminated #ifndef
//
// So the bytes that were read decide the size, and a build of the compiler carries
// its embedded files instead of loading them.
import os

const program = "fn main() {
	file := \$embed_file('data.txt')
	text := file.to_string()
	bytes := file.to_bytes()
	println('len=\${file.len} string=\${text.len} bytes=\${bytes.len}')
	print(text)
}
"

// text_of returns `count` numbered lines. A few hundred of them are past what the
// generated C spells as one string literal, like the headers of the compiler are.
fn text_of(label string, count int) string {
	mut lines := []string{cap: count}
	for i in 0 .. count {
		lines << 'line ${i:04} of the ${label} version'
	}
	return lines.join('\n') + '\n'
}

// printed is what the program prints for an embedded file that holds `text`.
fn printed(text string) string {
	return 'len=${text.len} string=${text.len} bytes=${text.len}\n${text}'
}

struct Probe {
	dir  string
	data string
	exe  string
}

// build_probe compiles the program with `flags`, while `data.txt` holds `text`.
fn build_probe(name string, flags []string, text string) !Probe {
	// The build under test is the one that the flags below describe.
	os.unsetenv('VFLAGS')
	dir := os.join_path(os.vtmp_dir(), 'embed_file_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	probe := Probe{
		dir:  dir
		data: os.join_path(dir, 'data.txt')
		exe:  os.join_path(dir, $if windows { 'probe.exe' } $else { 'probe' })
	}
	os.write_file(probe.data, text)!
	source := os.join_path(dir, 'probe.v')
	os.write_file(source, program)!
	mut args := [@VEXE]
	args << flags
	args << ['-o', probe.exe, source]
	res := os.exec(args)
	if res.exit_code != 0 {
		return error('could not build ${source}:\n${res.output}')
	}
	return probe
}

fn (p Probe) run() string {
	res := os.exec([p.exe])
	assert res.exit_code == 0, res.output
	return res.output
}

fn test_development_build_reads_the_file_as_it_is_when_the_program_runs() {
	first := text_of('first', 200)
	probe := build_probe('edited', [], first)!
	defer {
		os.rmdir_all(probe.dir) or {}
	}
	assert probe.run() == printed(first)
	// The file grows: nothing of it may be cut off.
	longer := text_of('second, longer', 250)
	assert longer.len > first.len
	os.write_file(probe.data, longer)!
	assert probe.run() == printed(longer)
	// The file shrinks: nothing may be read past its end.
	shorter := text_of('third', 20)
	assert shorter.len < first.len
	os.write_file(probe.data, shorter)!
	assert probe.run() == printed(shorter)
}

fn test_build_of_the_compiler_carries_its_embedded_files() {
	first := text_of('first', 200)
	probe := build_probe('carried', ['-building-v'], first)!
	defer {
		os.rmdir_all(probe.dir) or {}
	}
	assert probe.run() == printed(first)
	// What is in the source tree afterwards does not reach the executable.
	os.write_file(probe.data, text_of('second, longer', 250))!
	assert probe.run() == printed(first)
	os.rm(probe.data)!
	assert probe.run() == printed(first)
}
