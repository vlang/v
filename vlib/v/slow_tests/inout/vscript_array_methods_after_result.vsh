dir := join_path(temp_dir(), 'v_script_array_methods_${getpid()}')
mkdir_all(dir)!
defer {
	rmdir_all(dir) or {}
}
write_file(join_path(dir, 'one.v'), '')!
write_file(join_path(dir, 'two.txt'), '')!
println(ls(dir)!.filter(it.ends_with('.v')).sorted())
println(ls(dir)!.any(it == 'two.txt'))
println(ls(dir)!.all(it.len > 0))
println(ls(dir)!.map(it.to_upper()).sorted())
assert (ls(dir)!).filter(it.ends_with('.v')).len == 1
