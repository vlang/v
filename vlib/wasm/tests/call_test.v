module main

import wasm

fn test_call() {
	mut m := wasm.Module{}
	mut a1 := m.new_function('const-i32', [], [.i32_t])
	{
		a1.i32_const(0x132)
	}
	m.commit(a1, false)
	mut a2 := m.new_function('const-i64', [], [.i64_t])
	{
		a2.i64_const(0x164)
	}
	m.commit(a2, false)
	mut a3 := m.new_function('const-f32', [], [.f32_t])
	{
		a3.f32_const(0.2)
	}
	m.commit(a3, false)
	mut a4 := m.new_function('const-f64', [], [.f64_t])
	{
		a4.f64_const(0.4)
	}
	m.commit(a4, false)
	mut a5 := m.new_function('const-i32-i64', [], [.i32_t, .i64_t])
	{
		a5.i32_const(0x132)
		a5.i64_const(0x164)
	}
	m.commit(a5, false)

	mut b1 := m.new_function('type-i32', [], [.i32_t])
	{
		b1.call('const-i32')
	}
	m.commit(b1, true)
	mut b2 := m.new_function('type-i64', [], [.i64_t])
	{
		b2.call('const-i64')
	}
	m.commit(b2, true)
	mut b3 := m.new_function('type-f32', [], [.f32_t])
	{
		b3.call('const-f32')
	}
	m.commit(b3, true)
	mut b4 := m.new_function('type-f64', [], [.f64_t])
	{
		b4.call('const-f64')
	}
	m.commit(b4, true)
	mut b5 := m.new_function('type-i32-i64', [], [.i32_t, .i64_t])
	{
		b5.call('const-i32-i64')
	}
	m.commit(b5, true)

	mut fac := m.new_function('fac', [.i64_t], [.i64_t])
	{
		fac.local_get(0)
		fac.eqz(.i64_t)
		ifs := fac.c_if([], [.i64_t])
		{
			fac.i64_const(1)
		}
		fac.c_else(ifs)
		{
			{
				fac.local_get(0)
			}
			{
				fac.local_get(0)
				fac.i64_const(1)
				fac.sub(.i64_t)
				fac.call('fac')
			}
			fac.mul(.i64_t)
		}
		fac.c_end(ifs)
	}
	m.commit(fac, true)

	validate(m.compile()) or { panic(err) }
}
