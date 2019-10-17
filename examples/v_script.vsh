for _ in 0..5 {
  println('V script')
}
println('deploying...')
println(ls('.'))
mv('v.exe', 'bin/v.exe')
rm('tmp.c')
