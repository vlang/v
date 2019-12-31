// implements boolen

pub fn (b bool) str() string {
  return if b {
    'true'
  } else {
    'false'
  }
}
