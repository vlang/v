import math
import i18n
import time

fn test_formatter_numbers() {
	en := i18n.new_formatter('en')!
	de := i18n.new_formatter('de-DE')!
	fr := i18n.new_formatter('fr-FR')!
	assert en.number_int(1234567) == '1,234,567'
	assert de.number_int(1234567) == '1.234.567'
	assert fr.number_int(1234567) == '1 234 567'
	assert en.number_int(-42) == '-42'
	assert en.number(1234567.891, 2) == '1,234,567.89'
	assert de.number(1234567.891, 2) == '1.234.567,89'
	assert fr.number(1234567.891, 2) == '1 234 567,89'
	assert en.number(0.125, 2) == '0.13', 'half must round up'
	assert en.number(-9876.543, 2) == '-9,876.54'
	assert en.number(42.0, 0) == '42'
	assert en.number(-0.001, 2) == '0.00', 'negative zero normalizes'
	i18n.new_formatter('xx') or {
		assert err.msg() == 'i18n: unsupported locale `xx`'
		return
	}
	assert false, 'unknown locale must error'
}

fn test_formatter_currency_percent() {
	en := i18n.new_formatter('en-US')!
	de := i18n.new_formatter('de')!
	assert en.currency(1234.5, 'USD') == '\$1,234.50'
	assert en.currency(-1234.5, 'USD') == '-\$1,234.50'
	assert de.currency(-1234.5, 'EUR') == '-1.234,50 €'
	assert en.currency(-0.001, 'USD') == '\$0.00'
	assert de.currency(1234.5, 'EUR') == '1.234,50 €'
	assert en.currency(1234.5, 'JPY') == '¥1,235'
	assert de.currency(99.99, 'CHF') == '99,99 CHF'
	assert en.currency(7.0, 'XXQ') == 'XXQ7.00', 'unknown code falls back'
	assert en.percent(0.123) == '12.3%'
	assert de.percent(0.123) == '12,3 %'
}

fn test_formatter_date() {
	en := i18n.new_formatter('en')!
	de := i18n.new_formatter('de')!
	fr := i18n.new_formatter('fr')!
	d := time.new(time.Time{
		year:  2024
		month: 3
		day:   5
	})
	assert en.date(d) == '03/05/2024'
	assert de.date(d) == '05.03.2024'
	assert fr.date(d) == '05/03/2024'
}

fn test_sort_collated() {
	mut words := ['zebra', 'äpfel', 'apple', 'Éclair', 'eagle', 'ßtreet']
	i18n.sort_collated(mut words)
	// folded keys: apfel < apple < eagle < eclair < sstreet < zebra
	assert words == ['äpfel', 'apple', 'eagle', 'Éclair', 'ßtreet', 'zebra']
	assert i18n.compare_collated('a', 'ä') < 0, 'tie breaks on byte order'
	assert i18n.compare_collated('z', 'a') > 0
	assert i18n.compare_collated('abc', 'abc') == 0
}

fn test_formatter_rounding_boundaries_and_special_values() {
	en := i18n.new_formatter('en')!
	de := i18n.new_formatter('de')!
	assert en.number(1.234499999999, 3) == '1.234'
	assert en.number(1.2345, 3) == '1.235'
	assert en.number(-1.2345, 3) == '-1.235'
	assert en.number(9.9995, 3) == '10.000'
	assert en.number(1e-300, 9) == '0.000000000'
	assert en.number(-0.0, 2) == '0.00'
	assert en.number(1e301, 2).replace(',', '') == '1' + '0'.repeat(301) + '.00'
	for x in [math.nan(), math.inf(1), math.inf(-1)] {
		assert en.number(x, 2) == x.str()
		assert de.number(x, 0) == x.str()
	}
}

fn test_collation_latin_accents_and_combining_marks() {
	assert i18n.compare_collated('Řeka', 'sea') < 0
	assert i18n.compare_collated('Ǐ', 'b') > 0
	assert i18n.compare_collated('Þing', 'zebra') < 0
	assert i18n.compare_collated('ápfel', 'apple') < 0
	assert i18n.compare_collated('ẞ', 'street') < 0
}
