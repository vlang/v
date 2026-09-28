struct LinkStat {
mut:
	nlink u64
}

interface ResourceWithStat {
mut:
	stat LinkStat
}

struct FileResourceWithStat {
mut:
	stat LinkStat
}

fn test_postfix_mutation_of_nested_interface_field_updates_the_implementation() {
	mut file := &FileResourceWithStat{
		stat: LinkStat{
			nlink: 2
		}
	}
	mut resource := ResourceWithStat(file)
	resource.stat.nlink++
	assert resource.stat.nlink == 3
	assert file.stat.nlink == 3
	resource.stat.nlink--
	assert resource.stat.nlink == 2
	assert file.stat.nlink == 2
}
