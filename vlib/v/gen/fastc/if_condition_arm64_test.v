// vtest build: arm64? && macos

module fastc

import os
import v.pref

fn test_fastc_arm64_if_condition_validation() {
	$if arm64 ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_arm64_if_condition_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		path := os.join_path(root, 'main.v')
		output := os.join_path(root, 'program')
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		for translated in [false, true] {
			marker := if translated { '@[translated]\n' } else { '' }
			for condition in ['if a { b } else { c }', '(if a { b } else { c })',
				'((if a { b } else { c }))', '(if a { 1 } else { 2 }) < 2',
				'(\n\tif a {\n\t\tb\n\t} else {\n\t\tc\n\t})', 'a', '(a)', '((a))', '(a && b) || c'] {
				for body in ['if ${condition} { return 1 }; return 0',
					'if c { return 2 } else if ${condition} { return 1 }; return 0',
					'return if ${condition} { 1 } else { 0 }',
					'return if c { 2 } else if ${condition} { 1 } else { 0 }'] {
					source := '${marker}module main
fn choose(a bool, b bool, c bool) int { ${body} }
fn main() {
	if choose(true, true, false) != 1 || choose(false, true, false) != 0 {
		println("wrong")
		return
	}
	println("ok")
}
'
					os.write_file(path, source)!
					if !translated && condition.contains('if a') {
						generate_arm64_files([path], prefs, output) or {
							assert err.msg().contains('assign it to a variable first'), '${body}: ${err}'
							offset := source.index('if a') or { panic('missing nested if') }
							assert err.msg().contains('at byte ${offset} in ${os.real_path(path)}'), '${body}: expected byte ${offset}: ${err.msg()}'
							continue
						}
						assert false, 'ARM64 should reject: ${body}'
					}
					generate_arm64_files([path], prefs, output)!
					result := os.exec([output])
					assert result.exit_code == 0, '${translated}: ${body}: ${result.output}'
					assert result.output == 'ok\n', '${translated}: ${body}: ${result.output}'
				}
			}
		}
	}
}

fn test_fastc_arm64_if_condition_offsets_in_comptime_branches() {
	$if arm64 ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_arm64_if_condition_offsets_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		path := os.join_path(root, 'main.v')
		output := os.join_path(root, 'program')
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		for template in [
			r'$if arm64 ? {
fn main() { flag := true; if CONDITION { println("ok") } }
}',
			r'fn main() { flag := true
$if arm64 ? { if CONDITION { println("ok") } }
}',
			r'fn main() { flag := true
value := $if arm64 ? { if CONDITION { 1 } else { 0 } } $else { 0 }
if value == 1 { println("ok") }
}',
			r'$if arm64 ? {
fn main() { flag := true
$if macos { $if arm64 ? {
value := $if arm64 ? { if CONDITION { 1 } else { 0 } } $else { 0 }
if value == 1 { println("ok") }
} }
}
}',
			r'$if disabled ? { fn main() {} } $else {
fn main() { flag := true; if CONDITION { println("ok") } }
}',
			r'fn main() { flag := true
$if disabled ? {} $else $if arm64 ? { if CONDITION { println("ok") } }
}',
			r'fn main() { flag := true
value := $if disabled ? { 0 } $else { if CONDITION { 1 } else { 0 } }
if value == 1 { println("ok") }
}',
			r'$if arm64 ? { fn enabled() bool { return true } }
fn main() { flag := enabled(); if CONDITION { println("ok") } }',
			r'fn main() { flag := true
$if arm64 ? { if !flag { println("wrong") } }
if CONDITION { println("ok") }
}',
			r'fn main() {
value := $if arm64 ? { 1 } $else { 0 }
flag := value == 1
if CONDITION { println("ok") }
}',
			r'fn main() { flag := true
$if arm64 ? { defer { if CONDITION { println("ok") } } }
}',
			r'fn main() { flag := true
defer { if CONDITION { println("ok") } }
$if arm64 ? { return }
}',
			r'const enabled = true
fn main() {
$if arm64 ? { flag := enabled; if CONDITION { println("ok") } }
}',
			r'struct Settings {
enabled bool = true
}
fn main() {
$if arm64 ? { settings := Settings{}; flag := settings.enabled
if CONDITION { println("ok") }
}
}',
			r'$if arm64 ? {
const flag = true
const enabled = $if arm64 ? { if CONDITION { true } else { false } } $else { false }
}
fn main() { if enabled { println("ok") } }',
			r'const flag = true
$if arm64 ? {
struct Settings {
enabled bool = $if arm64 ? { if CONDITION { true } else { false } } $else { false }
}
}
fn main() { settings := Settings{}; if settings.enabled { println("ok") } }',
		] {
			for condition in ['if flag { true } else { false }', '((if flag { true } else { false }))',
				'flag'] {
				source := 'module main\n// Keep diagnostic offsets relative to the original file.\n' +
					template.replace('CONDITION', condition) + '\n'
				os.write_file(path, source)!
				if condition != 'flag' {
					generate_arm64_files([path], prefs, output) or {
						offset := source.index('if flag') or { panic('missing nested if') }
						assert err.msg().contains('assign it to a variable first'), err.msg()
						assert err.msg().contains('at byte ${offset} in ${os.real_path(path)}'), '${source}: ${err}'
						continue
					}
					assert false, 'ARM64 should reject: ${source}'
				}
				generate_arm64_files([path], prefs, output)!
				result := os.exec([output])
				assert result.exit_code == 0, '${source}: ${result.output}'
				assert result.output == 'ok\n', '${source}: ${result.output}'
			}
		}
	}
}
