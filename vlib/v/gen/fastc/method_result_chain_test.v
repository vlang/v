module fastc

import os
import v.cmdexec

fn test_fastc_negated_receivers_and_enum_result_methods_short_circuit() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_method_result_chains_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import v.token
struct Holder { value string }
enum Kind { identifier keyword }
type Alias = Kind
fn (kind Kind) is_keyword() bool { return kind == .keyword }
fn Kind.lookup(value string) Kind { return if value == "for" { .keyword } else { .identifier } }
fn lookup(value string) Kind { return Kind.lookup(value) }
fn alias_lookup(value string) Alias { return Alias(Kind.lookup(value)) }
fn (holder Holder) node_source(index int) string { return holder.value }
fn source(value string) string { return value }
fn check(holder Holder) bool {
	return !holder.value.starts_with("@") || token.Token.from_string_tinyv(holder.value[1..]).is_keyword()
}
fn main() {
	assert check(Holder{})
	assert check(Holder{value: "plain"})
	assert check(Holder{value: "@for"})
	assert !check(Holder{value: "@custom"})
	assert lookup("for").is_keyword()
	assert !lookup("custom").is_keyword()
	assert alias_lookup("for").is_keyword()
	assert Kind.lookup("for").is_keyword()
	keyword_holder := Holder{value: "@for"}
	flag := keyword_holder.value.len > 1 && token.Token.from_string_tinyv("for").is_keyword()
	assert flag
	assert keyword_holder.value.starts_with("@") && !token.Token.from_string_tinyv("custom").is_keyword()
	assert false && token.Token.from_string_tinyv("for").is_keyword() || true
	assert true || token.Token.from_string_tinyv("custom").is_keyword() && false
	spaced_holder := Holder{value: "  for  "}
	op := spaced_holder.node_source(0).trim_space()[..1]
	assert op == "f"
	assert spaced_holder.node_source(0).trim_space()[1..] == "or"
	assert source("  for  ").trim_space()[0] == u8(102)
	println("method chains work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'method chains work', run.output
}
