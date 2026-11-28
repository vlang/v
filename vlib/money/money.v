module money

import math.decimal

// Money is an amount of money in a specific Currency. Amounts are stored as
// exact fixed-point decimals, so sums, differences and products are exact.
pub struct Money {
pub:
	amount   decimal.Decimal
	currency Currency
}

// zero returns a zero amount in the given currency.
pub fn zero(c Currency) Money {
	return Money{
		amount:   decimal.zero
		currency: c
	}
}

// from_minor_units creates a Money value from an integer count of minor units,
// e.g. from_minor_units(1999, usd) is USD 19.99.
pub fn from_minor_units(units i64, c Currency) !Money {
	amount := decimal.from_minor_units(units, c.decimals)!
	return Money{
		amount:   amount
		currency: c
	}
}

// from_string parses a decimal string into a Money value in the given currency.
pub fn from_string(s string, c Currency) !Money {
	amount := decimal.from_string(s)!
	return Money{
		amount:   amount
		currency: c
	}
}

// from_decimal wraps an existing decimal.Decimal as a Money value.
pub fn from_decimal(d decimal.Decimal, c Currency) Money {
	return Money{
		amount:   d
		currency: c
	}
}

// currency returns the currency of the money value.
pub fn (m Money) currency_code() string {
	return m.currency.code
}

// minor_units returns the amount expressed as an integer count of minor units.
// Returns an error if the amount has more precision than the currency allows
// or does not fit in an i64.
pub fn (m Money) minor_units() !i64 {
	return m.amount.to_minor_units(m.currency.decimals)
}

// str returns a human readable representation, e.g. "$19.99".
pub fn (m Money) str() string {
	scaled := m.amount.str_fixed(m.currency.decimals) or { m.amount.str() }
	return '${m.currency.symbol}${scaled}'
}

// str_code returns an ISO-style representation, e.g. "USD 19.99".
pub fn (m Money) str_code() string {
	scaled := m.amount.str_fixed(m.currency.decimals) or { m.amount.str() }
	return '${m.currency.code} ${scaled}'
}

// round_to_currency rounds the amount to the currency's number of minor units
// using banker's rounding (half to even).
pub fn (m Money) round_to_currency() Money {
	rounded := m.amount.round(m.currency.decimals, .half_even) or { m.amount }
	return Money{
		amount:   rounded
		currency: m.currency
	}
}

// add returns the sum of two money values. Panics on currency mismatch.
pub fn (a Money) add(b Money) Money {
	if a.currency != b.currency {
		panic('money: currency mismatch: ${a.currency.code} vs ${b.currency.code}')
	}
	return Money{
		amount:   a.amount + b.amount
		currency: a.currency
	}
}

// sub returns the difference of two money values. Panics on currency mismatch.
pub fn (a Money) sub(b Money) Money {
	if a.currency != b.currency {
		panic('money: currency mismatch: ${a.currency.code} vs ${b.currency.code}')
	}
	return Money{
		amount:   a.amount - b.amount
		currency: a.currency
	}
}

// mul scales a money value by a decimal factor (rates, quantities, taxes).
pub fn (a Money) mul(factor decimal.Decimal) Money {
	return Money{
		amount:   a.amount * factor
		currency: a.currency
	}
}

// div divides a money value by a decimal factor. Panics on a zero divisor.
pub fn (a Money) div(factor decimal.Decimal) Money {
	return Money{
		amount:   a.amount / factor
		currency: a.currency
	}
}

// neg returns the negated amount.
pub fn (a Money) neg() Money {
	return Money{
		amount:   a.amount.neg()
		currency: a.currency
	}
}

// abs returns the absolute amount.
pub fn (a Money) abs() Money {
	mut v := a.amount
	if v < decimal.zero {
		v = v.neg()
	}
	return Money{
		amount:   v
		currency: a.currency
	}
}

// cmp compares two money values of the same currency. Returns -1, 0 or 1.
pub fn (a Money) cmp(b Money) int {
	if a.currency != b.currency {
		panic('money: currency mismatch: ${a.currency.code} vs ${b.currency.code}')
	}
	if a.amount < b.amount {
		return -1
	}
	if a.amount > b.amount {
		return 1
	}
	return 0
}

// is_zero reports whether the amount is zero.
pub fn (a Money) is_zero() bool {
	return a.amount == decimal.zero
}

// is_positive reports whether the amount is greater than zero.
pub fn (a Money) is_positive() bool {
	return a.amount > decimal.zero
}

// is_negative reports whether the amount is less than zero.
pub fn (a Money) is_negative() bool {
	return a.amount < decimal.zero
}

// equal reports whether two money values are equal in amount and currency.
pub fn (a Money) equal(b Money) bool {
	return a.currency == b.currency && a.amount == b.amount
}

// Operator overloads for ergonomic use. These panic on currency mismatch,
// mirroring decimal's panicking operators (e.g. division by zero).
pub fn (a Money) + (b Money) Money {
	return a.add(b)
}

pub fn (a Money) - (b Money) Money {
	return a.sub(b)
}

pub fn (a Money) == (b Money) bool {
	return a.equal(b)
}

pub fn (a Money) < (b Money) bool {
	return a.cmp(b) < 0
}

// allocate splits an amount into `parts` parts without losing minor units.
// The remainder is distributed one minor unit at a time to the first parts.
pub fn (a Money) allocate(parts int) ![]Money {
	if parts <= 0 {
		return error('money: parts must be positive')
	}
	rounded := a.round_to_currency()
	minor := rounded.minor_units()!
	mut base := minor / i64(parts)
	mut rem := minor % i64(parts)
	if rem < 0 {
		rem = -rem
		base = (minor - rem) / i64(parts)
	}
	mut result := []Money{cap: parts}
	for i in 0 .. parts {
		mut v := base
		if i64(i) < rem {
			if minor >= 0 {
				v += 1
			} else {
				v -= 1
			}
		}
		m := from_minor_units(v, a.currency)!
		result << m
	}
	return result
}
