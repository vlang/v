fn test_simple_string_interpolation() {
	a := 'Hello'
	b := 'World'
	res := '$a $b'
	assert res == 'Hello World'
}

fn test_mixed_string_interpolation() {
	num := 7
	str := 'abc'
	s1 := 'number=$num'
	assert s1 == 'number=7'
	s2 := 'string=$str'
	assert s2 == 'string=abc'
	s3 := 'a: $num | b: $str'
	assert s3 == 'a: 7 | b: abc'
}

fn test_formatted_string_interpolation() {
	x := 'abc'
	axb := 'a:$x:b'
	assert axb == 'a:abc:b'
	x_10 := 'a:${x:10s}:b'
	x10_ := 'a:${x:-10s}:b'
	assert x_10 == 'a:       abc:b'
	assert x10_ == 'a:abc       :b'
	i := 23
	si_right := '${i:10d}'
	si__left := '${i:-10d}'
	assert si_right == '        23'
	assert si__left == '23        '
}

fn test_excape_dollar_in_string() {
	i := 42
	assert '($i)' == '(42)'
	assert '(\$i)'.contains('i') && !'(\$i)'.contains('42')
	assert !'(\\$i)'.contains('i') && '(\\$i)'.contains('42') && '(\\$i)'.contains('\\')
	assert '(\\\$i)'.contains('i') && !'(\\\$i)'.contains('42') && '(\\$i)'.contains('\\')
	assert !'(\\\\$i)'.contains('i') && '(\\\\$i)'.contains('42') && '(\\\\$i)'.contains('\\\\')
	assert '($i)' == '(42)'
	assert '(\${i})'.contains('i') && !'(\${i})'.contains('42')
	assert !'(\\$i)'.contains('i') && '(\\$i)'.contains('42') && '(\\$i)'.contains('\\')
	assert '(\\\${i})'.contains('i') && !'(\\\${i})'.contains('42') && '(\\$i)'.contains('\\')
	assert !'(\\\\$i)'.contains('i') && '(\\\\$i)'.contains('42') && '(\\\\$i)'.contains('\\\\')
	assert i == 42
}

fn test_implicit_str() {
	i := 42
	assert 'int $i' == 'int 42'
	assert '$i' == '42'
	check := '$i' == '42'
	assert check
	text := '$i' + '42'
	assert text == '4242'
}

fn test_string_interpolation_percent_escaping() {
	test := 'hello'
	hello := 'world'
	x := '%.*s$hello$test |${hello:-30s}|'
	assert x == '%.*sworldhello |world                         |'
}

fn test_string_interpolation_string_prefix() {
	// `r`, `c` and `js` are also used as a string prefix.
	r := 'r'
	rr := '$r$r'
	assert rr == 'rr'
	c := 'c'
	cc := '$c$c'
	assert cc == 'cc'
	js := 'js'
	jsjs := '$js$js'
	assert jsjs == 'jsjs'
}

fn test_interpolation_string_prefix_expr() {
	r := 1
	c := 2
	js := 1
	assert '>${3 + r}<' == '>4<'
	assert '${r == js} $js' == 'true 1'
	assert '>${js + c} ${js + r == c}<' == '>3 true<'
}

fn test_inttypes_string_interpolation() {
	c := i8(-103)
	uc := byte(217)
	uc2 := byte(13)
	s := i16(-23456)
	us := u16(54321)
	i := -1622999040
	ui := u32(3421958087)
	vp := voidptr(ui)
	mut bp := byteptr(0)
	$if x64 {
		bp = byteptr(15541149836)
	} $else {
		bp = byteptr(3541149836)
	}
	l := i64(-7694555558525237396)
	ul := u64(17234006112912956370)
	assert '$s $us' == '-23456 54321'
	assert '$ui $i' == '3421958087 -1622999040'
	assert '$l $ul' == '-7694555558525237396 17234006112912956370'
	assert '>${s:11}:${us:-13}<' == '>     -23456:54321        <'
	assert '0x${ul:-19x}:${l:22d}' == '0xef2b7d4001165bd2   :  -7694555558525237396'
	assert '${c:5}${uc:-7}x' == ' -103217    x'
	assert '${c:x}:${uc:x}:${uc2:02X}' == '99:d9:0D'
	assert '${s:X}:${us:x}:${u16(uc):04x}' == 'A460:d431:00d9'
	assert '${i:x}:${ui:X}:${int(s):x}' == '9f430000:CBF6EFC7:ffffa460'
	assert '${l:x}:${ul:X}' == '9537727cad98876c:EF2B7D4001165BD2'
	// default pointer format is platform dependent, so try a few
	eprintln("platform pointer format: '${vp:p}:$bp'")
	$if x64 {
		assert '${vp:p}:$bp' == '0xcbf6efc7:0x39e53208c' || '${vp:p}:$bp' == 'CBF6EFC7:39E53208C'
			|| '${vp:p}:$bp' == 'cbf6efc7:39e53208c'
			|| '${vp:p}:$bp' == '00000000CBF6EFC7:000000039E53208C'
	} $else {
		assert '${vp:p}:$bp' == 'CBF6EFC7:D311A88C' || '${vp:p}:$bp' == 'cbf6efc7:d311a88c'
			|| '${vp:p}:$bp' == '0xcbf6efc7:0xd311a88c'
	}
}

fn test_utf8_string_interpolation() {
	a := 'à-côté'
	st := 'Sträßle'
	m := '10€'
	assert '$a $st $m' == 'à-côté Sträßle 10€'
	zz := '>${a:10}< >${st:-8}< >${m:5}<-'
	zz_expected := '>    à-côté< >Sträßle < >  10€<-'
	eprintln('         zz: $zz')
	eprintln('zz_expected: $zz_expected')
	assert zz == zz_expected
	// e := '\u20AC' // Eurosign doesn' work with MSVC and tcc
	e := '€'
	assert '100.00 $e' == '100.00 €'
	m2 := 'Москва́' // cyrillic а́: combination of U+0430 and U+0301, UTF-8: d0 b0 cc 81
	d := 'Antonín Dvořák' // latin á: U+00E1, UTF-8: c3 a1
	assert ':${m2:7}:${d:-15}:' == ': Москва́:Antonín Dvořák :'
	g := 'Πελοπόννησος'
	assert '>${g:-13}<' == '>Πελοπόννησος <'
}

struct Sss {
	v1 int
	v2 f64
}

fn (s Sss) str() string {
	return '[$s.v1, ${s.v2:.3f}]'
}

fn test_string_interpolation_str_evaluation() {
	mut x := Sss{17, 13.455893}
	assert '$x' == '[17, 13.456]'
}

fn test_string_interpolation_with_negative_format_width_should_compile_and_run_without_segfaulting() {
	// discovered during debugging VLS
	i := 3
	input := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
	eprintln('---------------------------------------------------------------------------------------------')
	eprintln('+60 ${i:10} | input.len: ${input.len:10} | ${input.bytes().hex():60} | $input')
	eprintln('-60 ${i:10} | input.len: ${input.len:10} | ${input.bytes().hex():-60} | $input')
	eprintln('---------------------------------------------------------------------------------------------')
	assert true
}

struct Aa {
	a int
}

struct Bb {
	b Aa
}

fn (x Bb) f() Aa {
	return x.b
}

fn test_method_interpolation() {
	y := Bb{
		b: Aa{
			a: 2
		}
	}
	assert '>$y.f().a<' == '>2<'
	assert '>$y.f().a<' == '>2<'
}

fn f(i int) int {
	return i
}

fn test_call() {
	s := '${f(4)}'
	assert s == '4'
}
