fn test_fn(s1 string, s2 string) {
	print(if s1 == s2 { 'true' } else { 'false' })
	print('\t=> ')
	println('"$s1", "$s2"')
}

fn simple_string_interpolation() {
	a := 'Hello'
	b := 'World'
	res := '$a $b'
	test_fn(res, 'Hello World')
}

fn mixed_string_interpolation() {
	num := 7
	str := 'abc'
	s1 := 'number=$num'
	test_fn(s1, 'number=7')
	s2 := 'string=$str'
	test_fn(s2, 'string=abc')
	s3 := 'a: $num | b: $str'
	test_fn(s3, 'a: 7 | b: abc')
}

fn formatted_string_interpolation() {
	x := 'abc'
	axb := 'a:$x:b'
	test_fn(axb, 'a:abc:b')
	x_10 := 'a:${x:10s}:b'
	x10_ := 'a:${x:-10s}:b'
	test_fn(x_10, 'a:       abc:b')
	test_fn(x10_, 'a:abc       :b')
	i := 23
	si_right := '${i:10d}'
	si__left := '${i:-10d}'
	test_fn(si_right, '        23')
	test_fn(si__left, '23        ')
}

/*
excape_dollar_in_string()
fn excape_dollar_in_string() {
	i := 42
	test_fn('($i)', '(42)')
	println('(\$i)'.contains('i') && !'(\$i)'.contains('42'))
	println(!'(\\$i)'.contains('i') && '(\\$i)'.contains('42') && '(\\$i)'.contains('\\'))
	println('(\\\$i)'.contains('i') && !'(\\\$i)'.contains('42') && '(\\$i)'.contains('\\'))
	println(!'(\\\\$i)'.contains('i') && '(\\\\$i)'.contains('42') && '(\\\\$i)'.contains('\\\\'))
	test_fn('(${i})', '(42)')
	println('(\${i})'.contains('i') && !'(\${i})'.contains('42'))
	println(!'(\\${i})'.contains('i') && '(\\${i})'.contains('42') && '(\\${i})'.contains('\\'))
	println('(\\\${i})'.contains('i') && !'(\\\${i})'.contains('42') && '(\\${i})'.contains('\\'))
	println(!'(\\\\${i})'.contains('i') && '(\\\\${i})'.contains('42') && '(\\\\${i})'.contains('\\\\'))
	test_fn(i, 42)
}
*/

fn implicit_str() {
	i := 42
	test_fn('int $i', 'int 42')
	test_fn('$i', '42')
	check := '$i' == '42'
	// println(check)
	text := '$i' + '42'
	test_fn(text, '4242')
}

fn string_interpolation_percent_escaping() {
	test := 'hello'
	hello := 'world'
	x := '%.*s$hello$test |${hello:-30s}|'
	test_fn(x, '%.*sworldhello |world                         |')
}

fn string_interpolation_string_prefix() {
	// `r`, `c` and `js` are also used as a string prefix.
	r := 'r'
	rr := '$r$r'
	test_fn(rr, 'rr')
	c := 'c'
	cc := '$c$c'
	test_fn(cc, 'cc')
	js := 'js'
	jsjs := '$js$js'
	test_fn(jsjs, 'jsjs')
}

fn interpolation_string_prefix_expr() {
	r := 1
	c := 2
	js := 1
	test_fn('>${3 + r}<', '>4<')
	test_fn('${r == js} $js', 'true 1')
	test_fn('>${js + c} ${js + r == c}<', '>3 true<')
}

/*
inttypes_string_interpolation()
fn inttypes_string_interpolation() {
	c := i8(-103)
	uc := byte(217)
	uc2 := byte(13)
	s := i16(-23456)
	us := u16(54321)
	i := -1622999040
	ui := u32(3421958087)
	vp := voidptr(ui)
	bp := byteptr(15541149836)
	l := i64(-7694555558525237396)
	ul := u64(17234006112912956370)
	test_fn('$s $us', '-23456 54321')
	test_fn('$ui $i', '3421958087 -1622999040')
	test_fn('$l $ul', '-7694555558525237396 17234006112912956370')
	test_fn('>${s:11}:${us:-13}<', '>     -23456:54321        <')
	test_fn('0x${ul:-19x}:${l:22d}', '0xef2b7d4001165bd2   :  -7694555558525237396')
	test_fn('${c:5}${uc:-7}x', ' -103217    x')
	test_fn('${c:x}:${uc:x}:${uc2:02X}', '99:d9:0D')
	test_fn('${s:X}:${us:x}:${u16(uc):04x}', 'A460:d431:00d9')
	test_fn('${i:x}:${ui:X}:${int(s):x}', '9f430000:CBF6EFC7:ffffa460')
	test_fn('${l:x}:${ul:X}', '9537727cad98876c:EF2B7D4001165BD2')
	// default pointer format is platform dependent, so try a few
	println("platform pointer format: '${vp:p}:$bp'")
	test_fn('${vp:p}:$bp', '0xcbf6efc7:0x39e53208c' ||
		'${vp:p}:$bp' == 'CBF6EFC7:39E53208C' ||
		'${vp:p}:$bp' == 'cbf6efc7:39e53208c' ||
		'${vp:p}:$bp' == '00000000CBF6EFC7:000000039E53208C')
}
*/

fn utf8_string_interpolation() {
	a := 'à-côté'
	st := 'Sträßle'
	m := '10€'
	test_fn('$a $st $m', 'à-côté Sträßle 10€')
	zz := '>${a:10}< >${st:-8}< >${m:5}<-'
	zz_expected := '>    à-côté< >Sträßle < >  10€<-'
	// println('         zz: $zz')
	// println('zz_expected: $zz_expected')
	test_fn(zz, zz_expected)
	// e := '\u20AC' // Eurosign doesn' work with MSVC and tcc
	e := '€'
	test_fn('100.00 $e', '100.00 €')
	m2 := 'Москва́' // cyrillic а́: combination of U+0430 and U+0301, UTF-8: d0 b0 cc 81
	d := 'Antonín Dvořák' // latin á: U+00E1, UTF-8: c3 a1
	test_fn(':${m2:7}:${d:-15}:', ': Москва́:Antonín Dvořák :')
	g := 'Πελοπόννησος'
	test_fn('>${g:-13}<', '>Πελοπόννησος <')
}

struct Sss {
	v1 int
	v2 f64
}

fn (s Sss) str() string {
	return '[$s.v1, ${s.v2:.3f}]'
}

fn string_interpolation_str_evaluation() {
	mut x := Sss{17, 13.455893}
	test_fn('$x', '[17, 13.456]')
}

/*
string_interpolation_with_negative_format_width_should_compile_and_run_without_segfaulting()
fn string_interpolation_with_negative_format_width_should_compile_and_run_without_segfaulting() {
	// discovered during debugging VLS
	i := 3
	input := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
	println('---------------------------------------------------------------------------------------------')
	println('+60 ${i:10} | input.len: ${input.len:10} | ${input.bytes().hex():60} | $input')
	println('-60 ${i:10} | input.len: ${input.len:10} | ${input.bytes().hex():-60} | $input')
	println('---------------------------------------------------------------------------------------------')
	println(true)
}
*/

fn main() {
	simple_string_interpolation()
	mixed_string_interpolation()
	formatted_string_interpolation()
	implicit_str()
	string_interpolation_percent_escaping()
	string_interpolation_string_prefix()
	interpolation_string_prefix_expr()
	utf8_string_interpolation()
	string_interpolation_str_evaluation()
}
