fn main() {

 for _ in 0..5 {
   println('V script')
 }

 println('deploying...')

 println('Files')
 foo := ls('.') or { panic(err) }
 println(foo)

 println('')
 rm('a.out')

 println('Making dir name and creating foo.txt')
 os.mkdir('name')? // TODO mkdir()
 create('foo.txt')?

 foo_ls := ls('.') or { panic(err) }
 println(foo_ls)
 println('')

 println('Entering into name')
 chdir('name')
 foo_ls2 := ls('.') or { panic(err) }
 println(foo_ls2)
 println('')

 println('Removing name and foo.txt')
 println('')
 chdir('../')
 rmdir('name')
 rm('foo.txt')

 again := ls('.') or { panic(err) }
 println(again)

}
