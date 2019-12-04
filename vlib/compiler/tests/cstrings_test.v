
fn C.puts(charptr) int

fn test_cstring(){
  h := c'world'
  C.puts(c'hello')
  C.puts(h)
  assert true
}
