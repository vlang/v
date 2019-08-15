
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
