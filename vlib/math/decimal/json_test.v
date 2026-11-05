import json2
import math.decimal

struct Invoice {
	total decimal.Decimal
	tax   ?decimal.Decimal
	lines []decimal.Decimal
}

struct Ledger {
	balances map[string]decimal.Decimal
}

type StringOrDecimal = string | decimal.Decimal
type DecimalOrString = decimal.Decimal | string

fn d(s string) decimal.Decimal {
	return decimal.from_string(s) or { panic(err) }
}

fn test_to_json_keeps_scale() {
	assert decimal.zero.to_json() == '0'
	assert d('10.50').to_json() == '10.50'
	assert d('-0.001').to_json() == '-0.001'
	assert d('-0.00').to_json() == '0.00'
	assert d('1.5e2').to_json() == '150'
	assert d('12345678901234567890.123456789').to_json() == '12345678901234567890.123456789'
}

fn test_from_json_number_and_string() {
	mut a := decimal.zero
	a.from_json_number('-12.340') or { panic(err) }
	assert a.coefficient().str() == '-12340'
	assert a.scale() == 3
	a.from_json_number('1.5e-2') or { panic(err) }
	assert a == d('0.015')
	assert a.scale() == 3
	mut b := decimal.zero
	b.from_json_string('19.99') or { panic(err) }
	assert b == d('19.99')
	assert b.scale() == 2
	b.from_json_string('-1.25E+1') or { panic(err) }
	assert b == d('-12.5')
}

fn test_from_json_string_requires_json_number_syntax() {
	for raw in ['', 'abc', ' 10.50', '10.50 ', '+1', '.5', '5.', '007', '-', '1e', '1e+', '0x10',
		'1_000', 'NaN', 'Infinity'] {
		mut v := decimal.zero
		v.from_json_string(raw) or {
			assert err.msg() == 'math.decimal: invalid JSON number `${raw}`'
			continue
		}
		assert false, 'from_json_string(`${raw}`) should have failed'
	}
}

fn test_json2_round_trip() {
	src := '{"total":10.50,"tax":0.84,"lines":[1,2.25,7.250]}'
	invoice := json2.decode[Invoice](src)!
	assert invoice.total.scale() == 2
	assert invoice.tax or { panic('tax should be set') } == d('0.84')
	assert invoice.lines.map(it.scale()) == [0, 2, 3]
	assert json2.encode(invoice) == src
}

fn test_json2_decodes_strings_null_and_missing_values() {
	invoice := json2.decode[Invoice]('{"total":"10.50","tax":null,"lines":["0.1","2e1"]}')!
	assert invoice.total == d('10.50')
	assert invoice.total.scale() == 2
	assert invoice.tax == none
	assert invoice.lines == [d('0.1'), d('20')]
	missing := json2.decode[Invoice]('{"lines":[]}')!
	assert missing.total.is_zero()
	assert missing.tax == none
	// Outside strict mode a `null` field decodes to zero, like an integer field.
	assert json2.decode[Invoice]('{"total":null,"lines":[]}')!.total.is_zero()
}

fn test_json2_root_and_map_values() {
	assert json2.decode[decimal.Decimal]('-3.10')!.to_json() == '-3.10'
	assert json2.decode[decimal.Decimal]('"0.007"')!.scale() == 3
	ledger := json2.decode[Ledger]('{"balances":{"bank":"-5.5","cash":100.00}}')!
	assert ledger.balances['cash'] or { panic('cash') }.scale() == 2
	assert ledger.balances['bank'] or { panic('bank') } == d('-5.5')
	assert json2.encode(ledger) == '{"balances":{"bank":-5.5,"cash":100.00}}'
}

fn test_json2_rejects_invalid_values() {
	cases := {
		'{"total":"abc","lines":[]}': 'invalid JSON number `abc`'
		'{"total":"","lines":[]}':    'invalid JSON number ``'
		'{"total":" 1","lines":[]}':  'invalid JSON number ` 1`'
		'{"total":1e101,"lines":[]}': 'exponent out of range `1e101`'
		// json2 words these errors itself; only the failure is checked.
		'{"total":true,"lines":[]}':  ''
		'{"total":[1],"lines":[]}':   ''
	}
	for src, message in cases {
		json2.decode[Invoice](src) or {
			assert err.msg().contains(message), '`${src}`: ${err.msg()}'
			continue
		}
		assert false, 'decoding `${src}` should have failed'
	}
}

fn test_json2_decodes_json5_numbers() {
	cases := {
		'{total: 0x1F, lines: []}':                               '31'
		'{total: -0X1f, lines: []}':                              '-31'
		'{total: +0x0, lines: []}':                               '0'
		'{total: .5, lines: []}':                                 '0.5'
		'{total: 5., lines: []}':                                 '5'
		'{total: +1.50, lines: []}':                              '1.50'
		"{total: '1.25', lines: []}":                             '1.25'
		// 2^124, beyond 64 bits
		'{total: 0x10000000000000000000000000000000, lines: []}': '21267647932558653966460912964485513216'
	}
	for src, expected in cases {
		invoice := json2.decode[Invoice](src, json5: true) or {
			assert false, '`${src}`: ${err.msg()}'
			continue
		}
		assert invoice.total.to_json() == expected, src
	}
	for src in ['{total: Infinity, lines: []}', '{total: -Infinity, lines: []}',
		'{total: NaN, lines: []}'] {
		json2.decode[Invoice](src, json5: true) or { continue }
		assert false, 'decoding `${src}` should have failed'
	}
	for raw in ['0x', '-0x', '0x-1', '0x+1', '0xg', '0x1.5'] {
		mut v := decimal.zero
		v.from_json_number(raw) or {
			assert err.msg() == 'math.decimal: invalid decimal value `${raw}`'
			continue
		}
		assert false, 'from_json_number(`${raw}`) should have failed'
	}
}

fn test_json2_sum_types_with_a_string_variant() {
	// json2 gives a JSON string to the first of `string` and `decimal.Decimal` declared.
	text := json2.decode[StringOrDecimal]('"x"')!
	assert text as string == 'x'
	numeric_text := json2.decode[StringOrDecimal]('"1.5"')!
	assert numeric_text as string == '1.5'
	number := json2.decode[StringOrDecimal]('1.50')!
	assert (number as decimal.Decimal).to_json() == '1.50'
	quoted := json2.decode[DecimalOrString]('"1.5"')!
	assert quoted as decimal.Decimal == d('1.5')
	json2.decode[DecimalOrString]('"x"') or {
		assert err.msg().contains('invalid JSON number `x`'), err.msg()
		return
	}
	assert false, 'a non-numeric string should not decode into `decimal.Decimal | string`'
}
