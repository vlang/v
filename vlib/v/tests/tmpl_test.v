fn one() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	downloads := map{
		'vlang/ui':  '3201'
		'vlang/vtl': '123'
	}
	ignored := true
	return $tmpl('tmpl/base.txt')
}

fn test_tmpl() {
	assert one().trim_space() == "name: Peter
age: 25
numbers: [1, 2, 3]


1
2
3


0 - 0
2 - 1
4 - 2
6 - 3
8 - 4
10 - 5
12 - 6
14 - 7
16 - 8
18 - 9


vlang/ui, downloaded 3201 times.
vlang/vtl, downloaded 123 times.


this is not ignored


so, it's basically true"
}

fn test_tmpl_in_anon_fn() {
	anon := fn (name string, age int, numbers []int, downloads map[string]string, ignored bool) string {

		return $tmpl('tmpl/base.txt')
	}

	assert anon('Peter', 25, [1, 2, 3], map{
		'vlang/ui':  '3201'
		'vlang/vtl': '123'
	}, true).trim_space() == "name: Peter
age: 25
numbers: [1, 2, 3]


1
2
3


0 - 0
2 - 1
4 - 2
6 - 3
8 - 4
10 - 5
12 - 6
14 - 7
16 - 8
18 - 9


vlang/ui, downloaded 3201 times.
vlang/vtl, downloaded 123 times.


this is not ignored


so, it's basically true"
}
