
fn test_simple_string_interpolation(){
	a := 'Hello'
	b := 'World'
	res := '$a $b'
	assert res == 'Hello World'
}

fn test_excape_dollar_in_string() {
  i := 42

  assert '($i)' == '(42)'
  assert '(\$i)'.contains('i') && !'(\$i)'.contains('42')
  assert !'(\\$i)'.contains('i') && '(\\$i)'.contains('42') && '(\\$i)'.contains('\\')
  assert '(\\\$i)'.contains('i') && !'(\\\$i)'.contains('42') && '(\\$i)'.contains('\\')
  assert !'(\\\\$i)'.contains('i') && '(\\\\$i)'.contains('42') && '(\\\\$i)'.contains('\\\\')

  assert '(${i})' == '(42)'
  assert '(\${i})'.contains('i') && !'(\${i})'.contains('42')
  assert !'(\\${i})'.contains('i') && '(\\${i})'.contains('42') && '(\\${i})'.contains('\\')
  assert '(\\\${i})'.contains('i') && !'(\\\${i})'.contains('42') && '(\\${i})'.contains('\\')
  assert !'(\\\\${i})'.contains('i') && '(\\\\${i})'.contains('42') && '(\\\\${i})'.contains('\\\\')
  assert i==42
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
