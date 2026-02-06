module main

import wasm

fn test_block() {
	mut m := wasm.Module{}
	mut a1 := m.new_function('param', [], [.i32_t])
	{
		a1.i32_const(1)
		blk := a1.c_block([.i32_t], [.i32_t])
		{
			a1.i32_const(2)
			a1.add(.i32_t)
		}
		a1.c_end(blk)
	}
	m.commit(a1, true)
	mut a2 := m.new_function('params', [], [.i32_t])
	{
		a2.i32_const(1)
		a2.i32_const(2)
		blk := a2.c_block([.i32_t, .i32_t], [.i32_t])
		{
			a2.add(.i32_t)
		}
		a2.c_end(blk)
	}
	m.commit(a2, true)
	mut a3 := m.new_function('params-id', [], [.i32_t])
	{
		a3.i32_const(1)
		a3.i32_const(2)
		blk := a3.c_block([.i32_t, .i32_t], [.i32_t, .i32_t])
		{
		}
		a3.c_end(blk)
		a3.add(.i32_t)
	}
	m.commit(a3, true)

	mut b1 := m.new_function('param-break', [], [.i32_t])
	{
		b1.i32_const(1)
		blk := b1.c_block([.i32_t], [.i32_t])
		{
			b1.i32_const(2)
			b1.add(.i32_t)
			b1.c_br(blk)
		}
		b1.c_end(blk)
	}
	m.commit(b1, true)
	mut b2 := m.new_function('params-break', [], [.i32_t])
	{
		b2.i32_const(1)
		b2.i32_const(2)
		blk := b2.c_block([.i32_t, .i32_t], [.i32_t])
		{
			b2.add(.i32_t)
			b2.c_br(blk)
		}
		b2.c_end(blk)
	}
	m.commit(b2, true)
	mut b3 := m.new_function('params-id-break', [], [.i32_t])
	{
		b3.i32_const(1)
		b3.i32_const(2)
		blk := b3.c_block([.i32_t, .i32_t], [.i32_t, .i32_t])
		{
			b3.c_br(blk)
		}
		b3.c_end(blk)
		b3.add(.i32_t)
	}
	m.commit(b3, true)

	mut dummy := m.new_function('dummy', [], [])
	{
	}
	m.commit(dummy, false)

	mut c1 := m.new_function('singular', [], [.i32_t])
	{
		blk1 := c1.c_block([], [])
		{
			c1.nop()
		}
		c1.c_end(blk1)
		blk2 := c1.c_block([], [.i32_t])
		{
			c1.i32_const(7)
		}
		c1.c_end(blk2)
	}
	m.commit(c1, true)
	mut c2 := m.new_function('nested', [], [.i32_t])
	{
		blk := c2.c_block([], [.i32_t])
		{
			blk1 := c2.c_block([], [])
			{
				c2.call('dummy')
				blk2 := c2.c_block([], [])
				{
				}
				c2.c_end(blk2)
				c2.nop()
			}
			c2.c_end(blk1)
			blk2 := c2.c_block([], [.i32_t])
			{
				c2.call('dummy')
				c2.i32_const(9)
			}
			c2.c_end(blk2)
		}
		c2.c_end(blk)
	}
	m.commit(c2, true)

	validate(m.compile()) or { panic(err) }
}
