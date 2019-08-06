import net.urllib 

fn test_escape_unescape() {
/* 
  original := 'те ст: т\\%'
  escaped := urllib.query_escape(original) or { assert false return} 
  assert escaped == '%D1%82%D0%B5%20%D1%81%D1%82%3A%20%D1%82%5C%25'
  unescaped := urllib.query_unescape(escaped) or { assert false return } 
  assert unescaped == original
*/ 
}

