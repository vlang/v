
fn test_cstring(){
  h := c'world'
  C.strlen(c'hello') == 5
  C.strlen(h) == 5
  assert true
}
