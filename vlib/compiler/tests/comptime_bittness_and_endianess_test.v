
fn test_bitness(){
  $if x32 {
    println('system is 32 bit')
    assert true
  }
  $if x64 {
    println('system is 64 bit')
    assert true
  }
  assert C.IS_32_BIT > -1
  assert C.IS_64_BIT > -1 
  assert C.IS_32_BIT != C.IS_64_BIT
}

fn test_endianness(){
  $if little_endian {
    println('system is little endian')
    assert true
  }
  $if big_endian {
    println('system is big endian')
    assert true
  }
  assert C.IS_BIG_ENDIAN > -1
  assert C.IS_LITTLE_ENDIAN > -1
  assert C.IS_BIG_ENDIAN != C.IS_LITTLE_ENDIAN
}
