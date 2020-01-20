fn test_array_to_string_conversion() {
  expected := '["1", "2", "3", "4"] '
  arr := ['1', '2', '3', '4']
  assert '$arr' == expected
}
