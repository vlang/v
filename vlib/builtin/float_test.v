fn test_float_decl() {
  x1 := 1e10
  x2 := -2e16
  x3 := 1e-15
  x4 := -9e-4
  assert typeof(x1) == 'f32'
  assert typeof(x2) == 'f32'
  assert typeof(x3) == 'f32'
  assert typeof(x4) == 'f32'
  x5 := 4e108
  x6 := -7e99
  x7 := 3e-205
  x8 := -6e-147
  assert typeof(x1) == 'f64'
  assert typeof(x2) == 'f64'
  assert typeof(x3) == 'f64'
  assert typeof(x4) == 'f64'
}
