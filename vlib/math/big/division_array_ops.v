module big

import math.bits

// help routines for cleaner code but inline for performance
// quicker than BitField.set_bit
@[direct_array_access; inline]
fn bit_set(mut a []u64, n int) {
	byte_offset := n / digit_bits
	mask := u64(1) << u32(n % digit_bits)
	$if debug {
		assert a.len >= byte_offset
	}
	a[byte_offset] |= mask
}

@[direct_array_access]
fn knuth_divide_array_by_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	m := operand_a.len - operand_b.len
	n := operand_b.len
	mut u := []u64{len: operand_a.len + 1}
	mut v := []u64{len: n}
	leading_zeros := bits.leading_zeros_64(operand_b.last()) - (64 - digit_bits)

	if leading_zeros > 0 {
		mut carry := u64(0)
		amount := digit_bits - leading_zeros
		for i in 0 .. operand_a.len {
			temp := (operand_a[i] << leading_zeros) | carry
			u[i] = temp & max_digit
			carry = operand_a[i] >> amount
		}
		u[operand_a.len] = carry
		carry = 0
		for i in 0 .. operand_b.len {
			temp := (operand_b[i] << leading_zeros) | carry
			v[i] = temp & max_digit
			carry = operand_b[i] >> amount
		}
	} else {
		for i in 0 .. operand_a.len {
			u[i] = operand_a[i]
		}
		for i in 0 .. operand_b.len {
			v[i] = operand_b[i]
		}
	}

	if remainder.len >= (n + 1) {
		remainder.trim(n + 1)
	} else {
		remainder = []u64{len: n + 1}
	}

	v_n_1 := v[n - 1]
	v_n_2 := v[n - 2]
	for j := m; j >= 0; j-- {
		u_j_n := u[j + n]
		u_j_n_1 := u[j + n - 1]
		u_j_n_2 := u[j + n - 2]

		mut qhat, mut rhat := bits.div_64(u_j_n >> (64 - digit_bits), (u_j_n << digit_bits) | u_j_n_1,
			v_n_1)
		mut x1, mut x2 := bits.mul_64(qhat, v_n_2)
		x2 = x2 & max_digit
		x1 = (x1 << (64 - digit_bits)) | (x2 >> digit_bits)
		for greater_than(x1, x2, rhat, u_j_n_2) {
			qhat--
			prev := rhat
			rhat += v_n_1
			if rhat < prev {
				break
			}
			x1, x2 = bits.mul_64(qhat, v_n_2)
			x2 = x2 & max_digit
			x1 = (x1 << (64 - digit_bits)) | (x2 >> digit_bits)
		}
		mut carry := u64(0)
		for i in 0 .. n {
			hi, lo := bits.mul_add_64(v[i], qhat, carry)
			remainder[i] = lo & max_digit
			carry = (hi << (64 - digit_bits)) | (lo >> digit_bits)
		}
		remainder[n] = carry

		mut borrow := u64(0)
		for i in 0 .. n + 1 {
			result := u[j + i] - remainder[i] - borrow
			u[j + i] = result & max_digit
			borrow = (result >> digit_bits) & 1
		}
		if borrow == 1 {
			qhat--
			carry = u64(0)
			for i in 0 .. n {
				sum := u[j + i] + v[i] + carry
				u[j + i] = sum & max_digit
				carry = sum >> digit_bits
			}
		}
		quotient[j] = qhat
	}

	remainder.delete_last()
	if leading_zeros > 0 {
		mut carry := u64(0)
		max_leading_digit := (u64(1) << leading_zeros) - 1
		for i := n - 1; i >= 0; i-- {
			current_limb := u[i]
			remainder[i] = (current_limb >> leading_zeros) | carry
			carry = (current_limb & max_leading_digit) << (digit_bits - leading_zeros)
		}
	} else {
		for i in 0 .. n {
			remainder[i] = u[i]
		}
	}
	shrink_tail_zeros(mut quotient)
	shrink_tail_zeros(mut remainder)
}

@[inline]
fn greater_than(x1 u64, x2 u64, y1 u64, y2 u64) bool {
	return x1 > y1 || (x1 == y1 && x2 > y2)
}
