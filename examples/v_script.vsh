#!/usr/local/bin/v

// The shebang above associates the file to V on Unix-like systems,
// so it can be run just by specifying the path to the file
// once it's made executable using `chmod +x`.

// Note that you can also use: `#!/usr/bin/env -S v crun`, if your system supports the -S flag to env
// The benefit is that in this case, v could be anywhere in your path, while /usr/bin/env is guaranteed
// to be present on most Unix systems in that exact place.

for _ in 0 .. 3 {
	println('V script')
}

println('\nMaking dir "v_script_dir".')
mkdir('v_script_dir')!

println("\nEntering into v_script_dir and listing it's files.")
chdir('v_script_dir')!
files := ls('.') or { panic(err) }
println(files)

println('\nCreating foo.txt')
create('foo.txt')!

println('\nFiles:')
again_ls := ls('.') or { panic(err) }
println(again_ls)

println('\nRemoving foo.txt and v_script_dir')
rm('foo.txt')!
chdir('../')!
rmdir('v_script_dir')!

print('\nDoes v_script_dir still exist? ')
println(exists('v_script_dir'))
