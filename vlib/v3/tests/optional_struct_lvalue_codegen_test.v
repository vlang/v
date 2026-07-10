import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_optional_struct_selector_assignment_mutates_payload_storage() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_input.v')
	os.write_file(src, "struct AFoo {
mut:
	name string
}

struct Inner {
mut:
	name string
}

struct Holder {
mut:
	opt ?Inner
}

fn holder_from_inner(inner Inner) Holder {
	return Holder{
		opt: inner
	}
}

struct Borrowed[^a] {
	path &^a string
}

fn Borrowed.new[^a](path &^a string) Borrowed[^a] {
	return Borrowed[^a]{
		path: path
	}
}

fn (b Borrowed[^a]) path_len[^a]() int {
	return (*b.path).len
}

struct GenericHolder[^a, W] {
	opt ?Borrowed[^a]
	w   W
}

fn generic_holder_from_path[^a, W](path &^a string, w W) GenericHolder[^a, W] {
	borrowed := Borrowed.new(path)
	return GenericHolder[^a, W]{
		opt: borrowed
		w:   w
	}
}

struct Link {
	n int
}

struct LinkCache[^a] {
	tag &^a string
mut:
	link ?Link
}

fn LinkCache.new[^a](tag &^a string) LinkCache[^a] {
	return LinkCache[^a]{
		tag: tag
	}
}

fn (mut cache LinkCache[^a]) get[^a]() ?&Link {
	if cache.link == none {
		cache.link = Link{
			n: 41
		}
	}
	return unsafe { &cache.link? }
}

struct Stats {
	n int
}

struct SearchResult {
	stats ?Stats
}

fn (result &^a SearchResult) stats_ref[^a]() ?&^a Stats {
	if result.stats != none {
		return unsafe { &result.stats? }
	}
	return none
}

struct ResultHolder {
mut:
	res !Inner
}

struct Outer {
mut:
	inner Inner
}

fn (mut f AFoo) opt_string(arr ?[]int) ?string {
	return arr?.len.str()
}

fn guarded_failure() ?AFoo {
	mut m := ?AFoo(none)
	m?.name = 'bad'
	return m
}

fn fail_inner() !Inner {
	return error('boom')
}

fn mutate_result_source(mut h ResultHolder) ! {
	h.res!.name = 'result'
}

fn mutate_explicit_or_source(mut holder Holder) {
	(holder.opt or {
		assert false
		return
	}).name = 'ok'
}

fn mutate_explicit_or_none() {
	mut holder := Holder{
		opt: none
	}
	(holder.opt or {
		assert err.msg() == ''
		println('none')
		return
	}).name = 'bad'
	assert false
}

fn mutate_result_explicit_or_ok(mut h ResultHolder) {
	(h.res or {
		assert false
		return
	}).name = 'explicit-result'
}

fn mutate_result_explicit_or_err(mut h ResultHolder) {
	(h.res or {
		assert err.msg() == 'boom'
		println('boom')
		return
	}).name = 'bad'
	assert false
}

fn main() {
	mut m := ?AFoo(AFoo{})
	assert m?.opt_string([1, 2, 3])? == '3'
	m?.name = 'foo'
	assert m?.name == 'foo'
	mut holder := Holder{
		opt: Inner{}
	}
	holder.opt?.name = 'holder'
	assert holder.opt?.name == 'holder'
	wrapped_holder := holder_from_inner(Inner{
		name: 'wrapped'
	})
	assert wrapped_holder.opt?.name == 'wrapped'
	path_text := 'wrapped-path'
	generic_holder := generic_holder_from_path(&path_text, 4)
	generic_borrowed := generic_holder.opt or { panic('missing borrowed') }
	assert generic_borrowed.path_len() == path_text.len
	cache_tag := 'cache'
	mut cache := LinkCache.new(&cache_tag)
	link := cache.get() or { panic('missing link') }
	assert link.n == 41
	search_result := SearchResult{
		stats: Stats{
			n: 7
		}
	}
	stats := search_result.stats_ref() or { panic('missing stats') }
	assert stats.n == 7
	mut outer := ?Outer(Outer{})
	outer?.inner.name = 'deep'
	assert outer?.inner.name == 'deep'
	mut result_ok := ResultHolder{
		res: Inner{}
	}
	mutate_result_source(mut result_ok)!
	assert result_ok.res!.name == 'result'
	mutate_result_explicit_or_ok(mut result_ok)
	assert result_ok.res!.name == 'explicit-result'
	mut result_bad := ResultHolder{
		res: fail_inner()
	}
	mut saw_result_err := false
	mutate_result_source(mut result_bad) or {
		assert err.msg() == 'boom'
		saw_result_err = true
	}
	assert saw_result_err
	mutate_result_explicit_or_err(mut result_bad)
	mutate_explicit_or_source(mut holder)
	assert holder.opt?.name == 'ok'
	mutate_explicit_or_none()
	guarded_failure() or {
		assert err.msg() == ''
		println('ok')
		return
	}
	assert false
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_optional_struct_lvalue_input')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('m.value.name ='), c_code
	assert c_code.contains('holder.opt.value.name ='), c_code
	assert c_code.contains('.opt = (Optional_Inner){.ok = true, .value = inner}')
		|| c_code.contains('.opt = (Optional_main__Inner){.ok = true, .value = inner}'), c_code
	assert c_code.contains('.opt = (Optional_Borrowed') && c_code.contains('.value = borrowed'), c_code
	assert c_code.contains('if (!cache->link.ok)') || c_code.contains('if (!cache.link.ok)'), c_code
	assert c_code.contains('&cache->link.value') || c_code.contains('&cache.link.value'), c_code
	assert c_code.contains('&result->stats.value') || c_code.contains('&result.stats.value'), c_code
	assert !c_code.contains('result->stats.value.ok'), c_code
	assert !c_code.contains('result->stats.value.value'), c_code
	assert c_code.contains('outer.value.inner.name ='), c_code
	assert c_code.contains('holder->opt.value.name =') || c_code.contains('holder.opt.value.name ='), c_code

	assert c_code.contains('if (!m.ok)'), c_code
	assert c_code.contains('if (!holder.opt.ok)'), c_code
	assert c_code.contains('if (!outer.ok)'), c_code
	assert c_code.contains('.err = h->res.err') || c_code.contains('.err = h.res.err'), c_code
	assert c_code.contains('IError err = holder.opt.err')
		|| c_code.contains('IError err = holder->opt.err'), c_code
	assert c_code.contains('IError err = h->res.err') || c_code.contains('IError err = h.res.err'), c_code

	mutate_result_start := c_code.index('\nOptional mutate_result_source(ResultHolder* h) {') or {
		-1
	}
	assert mutate_result_start >= 0, c_code
	mutate_result_tail := c_code[mutate_result_start..]
	mutate_result_guard := mutate_result_tail[..mutate_result_tail.index('h->res.value.name') or {
		mutate_result_tail.len
	}]
	assert !mutate_result_guard.contains('return (Optional){.ok = false};'), mutate_result_guard
	for line in c_code.split_into_lines() {
		assert !(line.contains('__or_val_') && line.contains('.name =') && !line.contains('= (')), line
		assert !(line.contains('__or_val_') && line.contains('.inner.name =')
			&& !line.contains('= (')), line
	}

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'boom\nnone\nok'
}
