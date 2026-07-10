import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_pointer_payload_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn c_fn_body(c_code string, header string) string {
	start := c_code.index(header) or { return '' }
	tail := c_code[start..]
	end := tail.index('\n}\n') or { return tail }
	return tail[..end + 3]
}

fn c_first_loop_return(c_body string) string {
	loop_start := c_body.index('for (') or { return '' }
	loop_tail := c_body[loop_start..]
	return_start := loop_tail.index('return ') or { return '' }
	return_tail := loop_tail[return_start..]
	end := return_tail.index(';\n') or { return return_tail }
	return return_tail[..end + 1]
}

fn c_compact(s string) string {
	return s.replace(' ', '').replace('\t', '').replace('\n', '')
}

fn test_local_pointer_ierror_payload_is_heap_copied() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_local_pointer_payload_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct PtrErr {
	text string
}

fn (err &PtrErr) msg() string {
	return err.text
}

fn (err &PtrErr) code() int {
	return 7
}

struct Holder {
	err PtrErr
}

fn local_ptr() !int {
	err := PtrErr{
		text: 'stack error'
	}
	return &err
}

fn alias_local_ptr() !int {
	err := PtrErr{
		text: 'alias stack error'
	}
	p := &err
	return p
}

fn direct_local() IError {
	err := PtrErr{
		text: 'direct error'
	}
	return &err
}

fn from_param(err PtrErr) !int {
	return &err
}

fn local_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'local field error'
		}
	}
	return &holder.err
}

fn from_param_field(holder Holder) !int {
	return &holder.err
}

fn alias_local_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'alias local field error'
		}
	}
	p := &holder.err
	return p
}

fn alias_chain_local_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'alias chain field error'
		}
	}
	p := &holder.err
	q := p
	return q
}

fn reassign_stable_to_stack(stable &Holder) !int {
	mut p := &stable.err
	holder := Holder{
		err: PtrErr{
			text: 'reassigned stack field error'
		}
	}
	p = &holder.err
	return p
}

fn reassign_stack_to_stable(stable &Holder) !int {
	holder := Holder{
		err: PtrErr{
			text: 'discarded stack field error'
		}
	}
	mut p := &holder.err
	p = &stable.err
	return p
}

fn conditional_stack_to_stable(cond bool, stable &Holder) !int {
	holder := Holder{
		err: PtrErr{
			text: 'conditional stack field error'
		}
	}
	mut p := &holder.err
	if cond {
		p = &stable.err
	}
	return p
}

fn conditional_stable_to_stack(cond bool, stable &Holder) !int {
	mut p := &stable.err
	if cond {
		holder := Holder{
			err: PtrErr{
				text: 'conditional stack branch error'
			}
		}
		p = &holder.err
	}
	return p
}

fn maybe_ptr(stable &Holder) ?&PtrErr {
	return &stable.err
}

fn guard_shadow_stable(stable &Holder) !int {
	holder := Holder{
		err: PtrErr{
			text: 'guard shadow stack field'
		}
	}
	p := &holder.err
	assert p.msg() == 'guard shadow stack field'
	if p := maybe_ptr(stable) {
		return p
	}
	return &stable.err
}

fn for_in_shadow_stable(stable &Holder) !int {
	holder := Holder{
		err: PtrErr{
			text: 'for shadow stack field'
		}
	}
	p := &holder.err
	assert p.msg() == 'for shadow stack field'
	for p in [&stable.err] {
		return p
	}
	return &stable.err
}

fn for_in_stack_direct() !int {
	err := PtrErr{
		text: 'for stack direct error'
	}
	for p in [&err] {
		return p
	}
	return &err
}

fn for_in_stack_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'for stack field error'
		}
	}
	for p in [&holder.err] {
		return p
	}
	return &holder.err
}

fn shadow_stable_outer(stable &Holder) !int {
	p := &stable.err
	if true {
		holder := Holder{
			err: PtrErr{
				text: 'shadow stack field'
			}
		}
		p := &holder.err
		assert p.msg() == 'shadow stack field'
	}
	return p
}

fn paren_alias_local_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'paren alias field error'
		}
	}
	p := (&holder.err)
	return p
}

fn cast_alias_local_field() !int {
	holder := Holder{
		err: PtrErr{
			text: 'cast alias field error'
		}
	}
	p := &PtrErr(&holder.err)
	return p
}

fn direct_alias_local_field() IError {
	holder := Holder{
		err: PtrErr{
			text: 'direct alias field error'
		}
	}
	p := &holder.err
	return p
}

fn from_pointer_param_field(holder &Holder) !int {
	return &holder.err
}

fn alias_pointer_param_field(holder &Holder) !int {
	p := &holder.err
	return p
}

fn alias_after_range_for_in(limit int) !int {
	holder := Holder{
		err: PtrErr{
			text: 'range alias stack field error'
		}
	}
	p := &holder.err
	for i in 0 .. limit {
		assert i >= 0
	}
	return p
}

fn main() {
	local_ptr() or {
		assert err.msg() == 'stack error'
		assert err.code() == 7
	}
	alias_local_ptr() or {
		assert err.msg() == 'alias stack error'
		assert err.code() == 7
	}
	direct := direct_local()
	assert direct.msg() == 'direct error'
	assert direct.code() == 7
	from_param(PtrErr{
		text: 'param error'
	}) or {
		assert err.msg() == 'param error'
		assert err.code() == 7
	}
	local_field() or {
		assert err.msg() == 'local field error'
		assert err.code() == 7
	}
	from_param_field(Holder{
		err: PtrErr{
			text: 'param field error'
		}
	}) or {
		assert err.msg() == 'param field error'
		assert err.code() == 7
	}
	alias_local_field() or {
		assert err.msg() == 'alias local field error'
		assert err.code() == 7
	}
	alias_chain_local_field() or {
		assert err.msg() == 'alias chain field error'
		assert err.code() == 7
	}
	paren_alias_local_field() or {
		assert err.msg() == 'paren alias field error'
		assert err.code() == 7
	}
	cast_alias_local_field() or {
		assert err.msg() == 'cast alias field error'
		assert err.code() == 7
	}
	direct_alias := direct_alias_local_field()
	assert direct_alias.msg() == 'direct alias field error'
	assert direct_alias.code() == 7
	stable := Holder{
		err: PtrErr{
			text: 'pointer field error'
		}
	}
	from_pointer_param_field(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	alias_pointer_param_field(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	alias_after_range_for_in(2) or {
		assert err.msg() == 'range alias stack field error'
		assert err.code() == 7
	}
	reassign_stable_to_stack(&stable) or {
		assert err.msg() == 'reassigned stack field error'
		assert err.code() == 7
	}
	reassign_stack_to_stable(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	conditional_stack_to_stable(false, &stable) or {
		assert err.msg() == 'conditional stack field error'
		assert err.code() == 7
	}
	conditional_stack_to_stable(true, &stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	conditional_stable_to_stack(false, &stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	conditional_stable_to_stack(true, &stable) or {
		assert err.msg() == 'conditional stack branch error'
		assert err.code() == 7
	}
	guard_shadow_stable(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	for_in_shadow_stable(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	for_in_stack_direct() or {
		assert err.msg() == 'for stack direct error'
		assert err.code() == 7
	}
	for_in_stack_field() or {
		assert err.msg() == 'for stack field error'
		assert err.code() == 7
	}
	shadow_stable_outer(&stable) or {
		assert err.msg() == 'pointer field error'
		assert err.code() == 7
	}
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_local_pointer_payload_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	local_body := c_fn_body(c_code, 'Optional local_ptr(void) {')
	assert local_body.contains('._object = memdup('), local_body
	assert local_body.contains('sizeof(PtrErr)'), local_body
	assert !local_body.contains('._object = &err'), local_body

	alias_local_body := c_fn_body(c_code, 'Optional alias_local_ptr(void) {')
	assert alias_local_body.contains('PtrErr* err = (PtrErr*)memdup('), alias_local_body
	assert alias_local_body.contains('PtrErr* p = err;'), alias_local_body
	assert alias_local_body.contains('sizeof(PtrErr)'), alias_local_body
	assert alias_local_body.contains('._object = \tp') || alias_local_body.contains('._object = p'), alias_local_body

	direct_body := c_fn_body(c_code, 'IError direct_local(void) {')
	assert direct_body.contains('._object = memdup('), direct_body
	assert direct_body.contains('sizeof(PtrErr)'), direct_body
	assert !direct_body.contains('._object = &err'), direct_body

	param_body := c_fn_body(c_code, 'Optional from_param(PtrErr err) {')
	assert param_body.contains('._object = memdup('), param_body
	assert param_body.contains('sizeof(PtrErr)'), param_body
	assert !param_body.contains('._object = &err'), param_body

	local_field_body := c_fn_body(c_code, 'Optional local_field(void) {')
	assert local_field_body.contains('._object = memdup('), local_field_body
	assert local_field_body.contains('sizeof(PtrErr)'), local_field_body
	assert !local_field_body.contains('._object = &holder.err'), local_field_body

	param_field_body := c_fn_body(c_code, 'Optional from_param_field(Holder holder) {')
	assert param_field_body.contains('._object = memdup('), param_field_body
	assert param_field_body.contains('sizeof(PtrErr)'), param_field_body
	assert !param_field_body.contains('._object = &holder.err'), param_field_body

	alias_field_body := c_fn_body(c_code, 'Optional alias_local_field(void) {')
	assert alias_field_body.contains('._object = memdup('), alias_field_body
	assert alias_field_body.contains('sizeof(PtrErr)'), alias_field_body
	assert !alias_field_body.contains('._object = p'), alias_field_body

	alias_chain_body := c_fn_body(c_code, 'Optional alias_chain_local_field(void) {')
	assert alias_chain_body.contains('._object = memdup('), alias_chain_body
	assert alias_chain_body.contains('sizeof(PtrErr)'), alias_chain_body
	assert !alias_chain_body.contains('._object = q'), alias_chain_body

	paren_alias_body := c_fn_body(c_code, 'Optional paren_alias_local_field(void) {')
	assert paren_alias_body.contains('._object = memdup('), paren_alias_body
	assert paren_alias_body.contains('sizeof(PtrErr)'), paren_alias_body
	assert !paren_alias_body.contains('._object = p'), paren_alias_body

	cast_alias_body := c_fn_body(c_code, 'Optional cast_alias_local_field(void) {')
	assert cast_alias_body.contains('._object = memdup('), cast_alias_body
	assert cast_alias_body.contains('sizeof(PtrErr)'), cast_alias_body
	assert !cast_alias_body.contains('._object = p'), cast_alias_body

	direct_alias_field_body := c_fn_body(c_code, 'IError direct_alias_local_field(void) {')
	assert direct_alias_field_body.contains('._object = memdup('), direct_alias_field_body
	assert direct_alias_field_body.contains('sizeof(PtrErr)'), direct_alias_field_body
	assert !direct_alias_field_body.contains('._object = p'), direct_alias_field_body

	pointer_param_field_body := c_fn_body(c_code,
		'Optional from_pointer_param_field(Holder* holder) {')
	assert !pointer_param_field_body.contains('._object = memdup('), pointer_param_field_body

	alias_pointer_param_field_body := c_fn_body(c_code,
		'Optional alias_pointer_param_field(Holder* holder) {')
	assert !alias_pointer_param_field_body.contains('._object = memdup('), alias_pointer_param_field_body

	alias_after_range_body := c_fn_body(c_code, 'Optional alias_after_range_for_in(int limit) {')
	alias_after_range_body_compact := c_compact(alias_after_range_body)
	assert alias_after_range_body.contains('._object = memdup('), alias_after_range_body
	assert alias_after_range_body.contains('sizeof(PtrErr)'), alias_after_range_body
	assert !alias_after_range_body_compact.contains('._object=p'), alias_after_range_body

	reassign_stable_to_stack_body := c_fn_body(c_code,
		'Optional reassign_stable_to_stack(Holder* stable) {')
	assert reassign_stable_to_stack_body.contains('._object = memdup('), reassign_stable_to_stack_body
	assert reassign_stable_to_stack_body.contains('sizeof(PtrErr)'), reassign_stable_to_stack_body
	assert !reassign_stable_to_stack_body.contains('._object = p'), reassign_stable_to_stack_body

	reassign_stack_to_stable_body := c_fn_body(c_code,
		'Optional reassign_stack_to_stable(Holder* stable) {')
	assert !reassign_stack_to_stable_body.contains('._object = memdup('), reassign_stack_to_stable_body

	conditional_stack_to_stable_body := c_fn_body(c_code,
		'Optional conditional_stack_to_stable(bool cond, Holder* stable) {')
	assert conditional_stack_to_stable_body.contains('._object = memdup('), conditional_stack_to_stable_body
	assert conditional_stack_to_stable_body.contains('sizeof(PtrErr)'), conditional_stack_to_stable_body
	assert !conditional_stack_to_stable_body.contains('._object = p'), conditional_stack_to_stable_body

	conditional_stable_to_stack_body := c_fn_body(c_code,
		'Optional conditional_stable_to_stack(bool cond, Holder* stable) {')
	assert conditional_stable_to_stack_body.contains('._object = memdup('), conditional_stable_to_stack_body
	assert conditional_stable_to_stack_body.contains('sizeof(PtrErr)'), conditional_stable_to_stack_body
	assert !conditional_stable_to_stack_body.contains('._object = p'), conditional_stable_to_stack_body

	guard_shadow_stable_body := c_fn_body(c_code, 'Optional guard_shadow_stable(Holder* stable) {')
	assert !guard_shadow_stable_body.contains('._object = memdup('), guard_shadow_stable_body

	for_in_shadow_stable_body := c_fn_body(c_code,
		'Optional for_in_shadow_stable(Holder* stable) {')
	for_in_shadow_stable_loop_return := c_first_loop_return(for_in_shadow_stable_body)
	assert !for_in_shadow_stable_loop_return.contains('._object = memdup('), for_in_shadow_stable_loop_return

	for_in_stack_direct_body := c_fn_body(c_code, 'Optional for_in_stack_direct(void) {')
	for_in_stack_direct_loop_return := c_first_loop_return(for_in_stack_direct_body)
	for_in_stack_direct_loop_return_compact := c_compact(for_in_stack_direct_loop_return)
	assert for_in_stack_direct_loop_return.contains('._object = memdup('), for_in_stack_direct_loop_return
	assert for_in_stack_direct_loop_return.contains('sizeof(PtrErr)'), for_in_stack_direct_loop_return
	assert !for_in_stack_direct_loop_return_compact.contains('._object=p'), for_in_stack_direct_loop_return

	for_in_stack_field_body := c_fn_body(c_code, 'Optional for_in_stack_field(void) {')
	for_in_stack_field_loop_return := c_first_loop_return(for_in_stack_field_body)
	for_in_stack_field_loop_return_compact := c_compact(for_in_stack_field_loop_return)
	assert for_in_stack_field_loop_return.contains('._object = memdup('), for_in_stack_field_loop_return
	assert for_in_stack_field_loop_return.contains('sizeof(PtrErr)'), for_in_stack_field_loop_return
	assert !for_in_stack_field_loop_return_compact.contains('._object=p'), for_in_stack_field_loop_return

	shadow_stable_outer_body := c_fn_body(c_code, 'Optional shadow_stable_outer(Holder* stable) {')
	assert !shadow_stable_outer_body.contains('._object = memdup('), shadow_stable_outer_body
}
