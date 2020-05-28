import gx

fn test_hex() {
  // valid colors
  color_a := gx.hex(0x6c5ce7)
  color_b := gx.rgb(108, 92, 231)
  assert color_a.eq(color_b) == true

  // doesn't give right value with short hex value
  short_color := gx.hex(0xfff)
  white_color := gx.rgb(255, 255, 255)
  assert short_color.eq(white_color) == false
}
