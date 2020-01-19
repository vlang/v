
fn test_cstring(){
  w := c'world'
  hlen := C.strlen(c'hello')
  wlen := C.strlen(w)
  assert  hlen == 5
  assert  wlen == 5
}
