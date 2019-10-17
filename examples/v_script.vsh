for _ in 0..5 {
  println('V script')
}

println('deploying...')
println(ls('.'))
println('')
mv('v.exe', 'bin/v.exe')
rm('tmp.c')

mkdir('name')
create('foo.txt')
println(ls('.'))
println('')

println('Removing name and foo.txt')
println('')
rmdir('name')
rm('foo.txt')

println(ls('.'))
