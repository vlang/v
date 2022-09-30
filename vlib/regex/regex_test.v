import regex
import rand
import strings

/******************************************************************************
*
* Test section
*
******************************************************************************/
struct TestItem {
	src string
	q   string
	s   int
	e   int
}

const(
match_test_suite = [
	// minus in CC
	TestItem{"d.def",r"abc.\.[\w\-]{,100}",-1,0},
	TestItem{"abc12345.asd",r"abc.\.[\w\-]{,100}",-1,4},
	TestItem{"abca.exe",r"abc.\.[\w\-]{,100}",0,8},
	TestItem{"abc2.exe-test_12",r"abc.\.[\w\-]{,100}",0,16},
	TestItem{"abcdefGHK",r"[a-f]+\A+",0,9},
	TestItem{"ab-cd-efGHK",r"[a-f\-g]+\A+",0,11},

	// base OR
	TestItem{"a",r"a|b",0,1},
	TestItem{"a",r"b|a",0,1},
	TestItem{"b",r"a|b",0,1},
	TestItem{"b",r"b|a",0,1},
	TestItem{"c",r"b|a",-1,0},

	// test base
	TestItem{"[ciao]",r"(.)ciao(.)",0,6},
	TestItem{"[ciao] da me",r"(.)ciao(.)",0,6},

	// positive
	TestItem{"this is a good.",r"this",0,4},
	TestItem{"this is a good.",r"good",10,14},
	TestItem{"this is a good.",r"go+d",10,14},
	TestItem{"this is a good.",r"g[oae]+d",10,14},
	TestItem{"this is a goed.",r"g[oae]+d",10,14},
	TestItem{"this is a good.",r"g[oae]*d",10,14},
	TestItem{"this is a goaezd.",r"g[ea-cm-z]*d",10,16},
	TestItem{"this is a good.",r"this (\w+) a",0,9},
	TestItem{"this is a good.",r"this( \w+){2} g",0,11},
	TestItem{"this is a good.",r"( ?\w+){,1}",0,4},
	TestItem{"this is a good.",r"( ?\w+)+",0,14},
	TestItem{"this is a good.",r"this( \w+)+",0,14},
	TestItem{"this is a good sample.",r"( ?\w+){,2}",0,7},
	TestItem{"this is a good sample.",r"( ?\w+){,3}",0,9},
	TestItem{"this is a good sample.",r"( ?\w+){,4}",0,14},
	TestItem{"this is a good sample.",r"( ?\w+){,5}",0,21},
	TestItem{"this is a good sample.",r"( ?\w+){2,3}",0,9},
	TestItem{"this is a good sample.",r"(\s?\w+){2,3}",0,9},
	TestItem{"this these those.",r"(th[ei]se?\s|\.)+",0,11},
	TestItem{"this these those ",r"(th[eio]se? ?)+",0,17},
	TestItem{"this these those ",r"(th[eio]se? )+",0,17},
	TestItem{"this,these,those. over",r"(th[eio]se?[,. ])+",0,17},
	TestItem{"soday,this,these,those. over",r".+(th[eio]se?[,. ])+",0,23},

	TestItem{"cpapaz",r"(c(pa)+z)",0,6},
	TestItem{"this is a cpapaz over",r"(c(pa)+z)",10,16},
	TestItem{"this is a cpapapez over",r"(c(p[ae])+z)",10,18},
	TestItem{"test@post.pip.com",r"[a-z0-9_]+@([a-z0-9_]+\.?)+",0,17},
	TestItem{"test1@post.pip.com, pera",r"[\w]+@([\w]+\.)+\w+",0,18},
	TestItem{"pippo@pera.com ",r"[a-z0-9_]+@([a-z0-9_]+\.?)+",0,14},
	TestItem{"adce aabe",r"(a(ab)+)|(a(dc)+)e",0,4},
	TestItem{"zadce aabe",r"(a(ab)+)|(a(dc)+)e",1,5},
	TestItem{"abbz accz addz.",r"c|(d)|e|(ab+)",0,3},
	TestItem{"this those these ciao",r"((t[hieo]+se?)\s*)+",0,17},
	TestItem{"this ciao",r"((t[hieo]+se?)\s*)+",0,5},
	TestItem{"this cpapaz adce aabe",r"(c(pa)+z)(\s[\a]+){2}",5,21},
	TestItem{"1234this cpapaz adce aabe",r"(c(pa)+z)(\s[\a]+){2}$",9,25},
	TestItem{"this cpapaz adce aabe third",r"(c(pa)+z)(\s[\a]+){2}",5,21},
	TestItem{"123cpapaz ole. pippo",r"(c(pa)+z)(\s+\a+[\.,]?)+",3,20},

	TestItem{"this is a good sample.",r".*i(\w)+",0,4},
	TestItem{"soday,this,these,those. over",r".*,(th[eio]se?[,. ])+",0,23},
	TestItem{"soday,this,these,thesa.thesi over",r".*,(th[ei]se?[,. ])+(thes[ai][,. ])+",0,29},
	TestItem{"cpapaz ole. pippo,",r".*(c(pa)+z)(\s+\a+[\.,]?)+",0,18},
	TestItem{"cpapaz ole. pippo",r"(c(pa)+z)(\s+\a+[\.,]?)+",0,17},
	TestItem{"cpapaz ole. pippo, 852",r".*(c(pa)+z)(\s+\a+[\.,]?)+",0,18},
	TestItem{"123cpapaz ole. pippo",r".*(c(pa)+z)(\s+\a+[\.,]?)+",0,20},
	TestItem{"...cpapaz ole. pippo",r".*(c(pa)+z)(\s+\a+[\.,]?)+",0,20},

	TestItem{"cpapaz ole. pippo,",r".*c.+ole.*pi",0,14},
	TestItem{"cpapaz ole. pipipo,",r".*c.+ole.*p([ip])+o",0,18},
	TestItem{"cpapaz ole. pipipo",r"^.*c.+ol?e.*p([ip])+o$",0,18},
	TestItem{"abbb",r"ab{2,3}?",0,3},
	TestItem{" pippo pera",r"\s(.*)pe(.*)",0,11},
	TestItem{" abb",r"\s(.*)",0,4},

	TestItem{"/home/us_er/pippo/info-01.txt", r"(/?[-\w_]+)*\.txt$",0,29}

	// negative
	TestItem{"zthis ciao",r"((t[hieo]+se?)\s*)+",-1,0},
	TestItem{"this is a good.",r"thes",-1,2},
	TestItem{"test1post.pip.com, pera",r"[\w]+@([\w]+\.)+\w+",-1,9},
	TestItem{"this cpapaz adce",r"(c(pa)+z)(\s[\a]+){2}",-1,0},
	TestItem{"this cpapaz adce aabe third",r"(c(pa)+z)(\s[\a]+){2}$",-1,0},
	TestItem{"1234this cpapaz adce aabe ter",r"(c(pa)+z)(\s[\a]+){2}$",-1,0},
	TestItem{"cpapaz ole. pipipo,",r"^.*c.+ol?e.*p([ip])+o$",-1,0},
	TestItem{"/home/us_er/pippo/info-01.jpeg", r"(/?[-\w_]+)*\.txt$",-1,26}

	// check unicode
	TestItem{"this is a Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ test",r".*a [Ⅰ-Ⅵ ]+",0,34},
	TestItem{"123Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ test",r"[Ⅰ-Ⅴ\s]+",3,23},

	// new edge cases
	TestItem{"12345678", r"[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]",-1,8},
	TestItem{"12345678", r"[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]",0,8},
	TestItem{"123456789", r"^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$",0,9}
	TestItem{"12345678", r"^\d{8}$",0,8},
	TestItem{"12345678", r"^\d{7}$",-1,0},
	TestItem{"12345678", r"^\d{9}$",-1,8},

	TestItem{"eth", r"(oth)|(eth)",0,3},
	TestItem{"et", r"(oth)|(eth)",-1,2},
	TestItem{"et", r".*(oth)|(eth)",-1,2},
	TestItem{"peoth", r".*(ith)|(eth)",-1,5},

	TestItem{"poth", r"(eth)|(oth)",1,4},
	TestItem{"poth", r"(oth)|(eth)",1,4},
	TestItem{"poth", r".(oth)|(eth)$",0,4},
	TestItem{"poth", r"^.(oth)|(eth)$",0,4},
	TestItem{"poth", r"^\w+$",0,4},

	// test dot_char
	TestItem{"8-11 l: qllllqllklhlvtl", r"^(\d+)-(\d+) ([a-z]): (.*)$",0,23},
	TestItem{"accccb deer", r"^a(.*)b d(.+)r",0,11},
	TestItem{"accccb deer", r"^a(.*)b d(.+)",0,11},
	TestItem{"accccb deer", r"^(.*)$",0,11},
	TestItem{"accccb deer", r"^a(.*)b d(.+)p",-1,11},
	TestItem{"##.#....#.##.####...#.##", r".{18}[.#]",0,19},
	TestItem{"#.#......##.#..#..##........##....###...##...######.......#.....#..#......#...#........###.#..#.", r'.*#[.#]{4}##[.#]{4}##[.#]{4}###',0,49},

	// test bcksls chars
	TestItem{"[ an s. s! ]( wi4ki:something )", r"\[.*\]\( *(\w*:*\w+) *\)",0,31},
	TestItem{"[ an s. s! ](wiki:something)", r"\[.*\]\( *(\w*:*\w+) *\)",0,28},
	TestItem{"p_p", r"\w+",0,3},
	TestItem{"p_é", r"\w+",0,2},

	// Crazywulf tests (?:^|[()])(\d+)(*)(\d+)(?:$|[()])
    TestItem{"1*1", r"(\d+)([*])(\d+)",0,3},
    TestItem{"+1*1", r"^(\d+)([*])(\d+)",-1,0},
    TestItem{"*1*1", r"(?:^|[*])(\d+)([*])(\d+)",0,4},
    TestItem{"*1*1", r"(?:^|[*()])(\d+)([*])(\d+)",0,4},
    TestItem{")1*1", r"(?:^|[*()])(\d+)([*])(\d+)",0,4},
    TestItem{"(1*1", r"(?:^|[*()])(\d+)([*])(\d+)",0,4},
    TestItem{"*1*1(", r"(?:^|[*()])(\d+)([*])(\d+)(?:$|[*()])",0,5},
    TestItem{" 1*1(", r"(?:^|[*()])(\d+)([*])(\d+)(?:$|[*()])",-1,0},
    TestItem{"1*1 ", r"(?:^|[*()])(\d+)([*])(\d+)(?:$|[*()])",-1,0},

    // particular groups
    TestItem{"ababababac", r"ab(.*)(ac)",0,10},

    // backslash on finish string
    TestItem{"a", r"\S+",0,1},
    TestItem{"aaaa", r"\S+",0,4},
    TestItem{"aaaa ", r"\S+",0,4},

    // multiple dot char
    TestItem{"aba", r"a*(b*)*a",0,3},
    TestItem{"/*x*/", r"/\**(.*)\**/",0,5},
    TestItem{"/*x*/", r"/*(.*)*/",0,5},

    // test last IST check
    TestItem{"refs/remotes/origin/mastep", r"refs/remotes/origin/(.*)",0,26},
    TestItem{"refs/remotes/origin/master", r"refs/remotes/origin/(.*)",0,26},
    TestItem{"refs/remotes/origin/mastep", r"refs/remotes/origin/(\w*)",0,26},
    TestItem{"refs/remotes/origin/master", r"refs/remotes/origin/(\w*)",0,26},
]
)

struct TestItemRe {
	src string
	q   string
	rep string
	r   string
}

const (
match_test_suite_replace = [
	// replace tests
	TestItemRe{
		"oggi pibao è andato a casa di pbababao ed ha trovato pibabababao",
		r"(pi?(ba)+o)",
		"CIAO",
		"oggi CIAO è andato a casa di CIAO ed ha trovato CIAO"
	},
	TestItemRe{
		"Today is a good day and tomorrow will be for sure.",
		r"[Tt]o\w+",
		"CIAO",
		"CIAO is a good day and CIAO will be for sure."
	},
	TestItemRe{
		"Today is a good day and tomorrow will be for sure.",
		r"(a\w) ",
		r"[\0] ",
		"Tod[ay] is a good d[ay] and tomorrow will be for sure."
	},
	TestItemRe{
		"Today is a good day and tomorrow will be for sure.",
		r"(a\w) ",
		r"[\0_\0] ",
		"Tod[ay_ay] is a good d[ay_ay] and tomorrow will be for sure."
	},
	TestItemRe{
		"Today is a good day and tomorrow will be for sure.",
		r"(a\w) ",
		r"[\0\1] ",
		"Tod[ay] is a good d[ay] and tomorrow will be for sure."
	},
]

match_test_suite_replace_simple = [
	// replace tests
	TestItemRe{
		"oggi pibao è andato a casa di pbababao ed ha trovato pibabababao",
		r"(pi?(ba)+o)",
		"CIAO",
		"oggi CIAO è andato a casa di CIAO ed ha trovato CIAO"
	},
	TestItemRe{
		"Today is a good day and tomorrow will be for sure.",
		r"[Tt]o\w+",
		"CIAO",
		"CIAO is a good day and CIAO will be for sure."
	},
]
)

struct TestItemCGroup {
	src string
	q   string
	s   int
	e   int
	cg  []int // [number of items (3*# item), id_group_0, start_0, end_0, id_group_1, start1, start2,... ]
	cgn map[string]int
}

const (
cgroups_test_suite = [
	TestItemCGroup{
		"http://www.ciao.mondo/hello/pippo12_/pera.html",
		r"(?P<format>https?)|(?:ftps?)://(?P<token>[\w_]+[\.|/])+",0,42,
		[7, 0, 0, 4, 1, 7, 11, 1, 11, 16, 1, 16, 22, 1, 22, 28, 1, 28, 37, 1, 37, 42],
		{'format':int(0),'token':1}
	},
	TestItemCGroup{
		"http://www.ciao.mondo/hello/pippo12_/pera.html",
		r"(?P<format>https?)|(?P<format>ftps?)://(?P<token>[\w_]+.)+",0,46,
		[8, 0, 0, 4, 1, 7, 11, 1, 11, 16, 1, 16, 22, 1, 22, 28, 1, 28, 37, 1, 37, 42, 1, 42, 46]
		//[8, 0, 0, 4, 1, 7, 10, 1, 11, 15, 1, 16, 21, 1, 22, 27, 1, 28, 36, 1, 37, 41, 1, 42, 46],
		{'format':int(0),'token':1}
	},
	TestItemCGroup{
		"http://www.ciao.mondo/hello/pippo12_/pera.html",
		r"(?P<format>https?)|(?P<format>ftps?)://([\w_]+\.)+",0,16,
		[3, 0, 0, 4, 1, 7, 11, 1, 11, 16],
		{'format':int(0)}
	},
	TestItemCGroup{
		"acc +13 pippo",
		r"(\w+)\s(.)([0-9]+) \w+",0,13,
		[0, 3, 4, 5, 5, 7],
		map[string]int{}
	},
	TestItemCGroup{
		"acc +13",
		r"(\w+)\s(.)([0-9]+)",0,7,
		[0, 3, 4, 5, 5, 7],
		map[string]int{}
	},
	TestItemCGroup{
		"ababababac",
		r"ab(.*)(ac)",0,10,
		[2, 8, 8, 10],
		map[string]int{}
	},
]
)

struct Test_find_all {
	src     string
	q       string
	res     []int    // [0,4,5,6...]
	res_str []string // ['find0','find1'...]
}

const (
find_all_test_suite = [
	Test_find_all{
		"abcd 1234 efgh 1234 ghkl1234 ab34546df",
		r"\d+",
		[5, 9, 15, 19, 24, 28, 31, 36],
		['1234', '1234', '1234', '34546']
	},
	Test_find_all{
		"abcd 1234 efgh 1234 ghkl1234 ab34546df",
		r"\a+",
		[0, 4, 10, 14, 20, 24, 29, 31, 36, 38],
		['abcd', 'efgh', 'ghkl', 'ab', 'df']
	},
	Test_find_all{
		"oggi pippo è andato a casa di pluto ed ha trovato pippo",
		r"p[iplut]+o",
		[5, 10, 31, 36, 51, 56],
		['pippo', 'pluto', 'pippo']
	},
	Test_find_all{
		"oggi pibao è andato a casa di pbababao ed ha trovato pibabababao",
		r"(pi?(ba)+o)",
		[5, 10, 31, 39, 54, 65],
		['pibao', 'pbababao', 'pibabababao']
	},
	Test_find_all{
		"Today is a good day and tomorrow will be for sure.",
		r"[Tt]o\w+",
		[0, 5, 24, 32],
		['Today', 'tomorrow']
	},
	Test_find_all{
		"pera\nurl = https://github.com/dario/pig.html\npippo",
		r"url *= *https?://[\w./]+",
		[5, 44],
		['url = https://github.com/dario/pig.html']
	},
	Test_find_all{
		"pera\nurl = https://github.com/dario/pig.html\npippo",
		r"url *= *https?://.*"+'\n',
		[5, 45],
		['url = https://github.com/dario/pig.html\n']
	},
	Test_find_all{
		"#.#......##.#..#..##........##....###...##...######.......#.....#..#......#...#........###.#..#.",
		r"#[.#]{4}##[.#]{4}##[.#]{4}###",
		[29, 49],
		['#....###...##...####']
	},
	Test_find_all{
		"#.#......##.#..#..##........##....###...##...######.......#.....#..#......#...#........###.#..#.",
		r".*#[.#]{4}##[.#]{4}##[.#]{4}###",
		[0, 49],
		['#.#......##.#..#..##........##....###...##...####']
	},
	Test_find_all{
		"1234 Aa dddd Aaf 12334 Aa opopo Aaf",
		r"Aa.+Aaf",
		[5, 16, 23, 35],
		['Aa dddd Aaf', 'Aa opopo Aaf']
	},
	Test_find_all{
		"@for something @endfor @for something else @endfor altro testo @for body @endfor uno due @for senza dire più @endfor pippo",
		r"@for.+@endfor",
		[0, 22, 23, 50, 63, 80, 89, 117],
		['@for something @endfor', '@for something else @endfor', '@for body @endfor', '@for senza dire più @endfor']
	},
	Test_find_all{
		"+++pippo+++\n elvo +++ pippo2 +++ +++ oggi+++",
		r"\+{3}.*\+{3}",
		[0, 11, 18, 32, 33, 44],
		['+++pippo+++', '+++ pippo2 +++', '+++ oggi+++']
	}

]
)

struct Test_split {
	src string
	q   string
	res []string // ['abc','def',...]
}

const (
	split_test_suite = [
		Test_split{'abcd 1234 efgh 1234 ghkl1234 ab34546df', r'\d+', ['abcd ', ' efgh ', ' ghkl',
			' ab', 'df']},
		Test_split{'abcd 1234 efgh 1234 ghkl1234 ab34546df', r'\a+', [' 1234 ', ' 1234 ', '1234 ',
			'34546']},
		Test_split{'oggi pippo è andato a casa di pluto ed ha trovato pippo', r'p[iplut]+o', [
			'oggi ', ' è andato a casa di ', ' ed ha trovato ']},
		Test_split{'oggi pibao è andato a casa di pbababao ed ha trovato pibabababao', r'(pi?(ba)+o)', [
			'oggi ', ' è andato a casa di ', ' ed ha trovato ']},
		Test_split{'Today is a good day and tomorrow will be for sure.', r'[Tt]o\w+', [
			' is a good day and ', ' will be for sure.']},
		Test_split{'pera\nurl = https://github.com/dario/pig.html\npippo', r'url *= *https?://[\w./]+', [
			'pera\n', '\npippo']},
		Test_split{'pera\nurl = https://github.com/dario/pig.html\npippo', r'url *= *https?://.*' +
			'\n', ['pera\n', 'pippo']},
		Test_split{'#.#......##.#..#..##........##....###...##...######.......#.....#..#......#...#........###.#..#.', r'#[.#]{4}##[.#]{4}##[.#]{4}###', [
			'#.#......##.#..#..##........#', '##.......#.....#..#......#...#........###.#..#.']},
		Test_split{'#.#......##.#..#..##........##....###...##...######.......#.....#..#......#...#........###.#..#.', r'.*#[.#]{4}##[.#]{4}##[.#]{4}###', [
			'##.......#.....#..#......#...#........###.#..#.']},
		Test_split{'1234 Aa dddd Aaf 12334 Aa opopo Aaf', r'Aa.+Aaf', ['1234 ', ' 12334 ']},
		Test_split{'@for something @endfor @for something else @endfor altro testo @for body @endfor uno due @for senza dire più @endfor pippo', r'@for.+@endfor', [
			' ', ' altro testo ', ' uno due ', ' pippo']},
		Test_split{'+++pippo+++\n elvo +++ pippo2 +++ +++ oggi+++', r'\+{3}.*\+{3}', [
			'\n elvo ', ' ']},
		Test_split{'foobar', r'\d', ['foobar']},
		Test_split{'1234', r'\d+', []},
	]
)

const (
	debug = true // true for debug println
)

fn test_regex() {
	// check capturing groups
	for c, to in cgroups_test_suite {
		// debug print
		if debug {
			println('$c [$to.src] [q$to.q] ($to.s, $to.e)')
		}

		mut re := regex.regex_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}

		if to.cgn.len > 0 {
			re.group_csave_flag = true
			// re.group_csave = [-1].repeat(3*20+1)
			if debug {
				println('continuous save')
			}
		} else {
			if debug {
				println('NO continuous save')
			}
		}

		start, end := re.match_string(to.src)

		mut tmp_str := ''
		if start >= 0 && end > start {
			tmp_str = to.src[start..end]
		}

		if start != to.s || end != to.e {
			println('#$c [$to.src] q[$to.q] res[$tmp_str] base:[$to.s,$to.e] $start, $end')
			eprintln('ERROR!')
			assert false
			continue
		}

		// check cgroups
		if to.cgn.len > 0 {
			if re.group_csave.len == 0 || re.group_csave[0] != to.cg[0] {
				eprintln('Capturing group len error! found: ${re.group_csave[0]} true ground: ${to.cg[0]}')
				assert false
				continue
			}

			// check captured groups
			mut ln := re.group_csave[0] * 3
			for ln > 0 {
				if re.group_csave[ln] != to.cg[ln] {
					eprintln('Capturing group failed on $ln item!')
					assert false
				}
				ln--
			}

			// check named captured groups
			for k in to.cgn.keys() {
				if to.cgn[k] != (re.group_map[k] - 1) { // we have -1 because the map not found is 0, in groups we start from 0 and we store using +1
					eprintln('Named capturing group error! [$k]')
					assert false
					continue
				}
			}
		} else {
			// check normal captured groups
			if re.groups.len != to.cg.len {
				assert false
			}
			for ln := 0; ln < re.groups.len; ln++ {
				if re.groups[ln] != to.cg[ln] {
					eprintln("Capture group doesn't match:")
					eprintln('true ground: $to.cg')
					eprintln('elaborated : $re.groups')
					assert false
				}
			}
		}
	}

	// check find_all
	for c, to in find_all_test_suite {
		// debug print
		if debug {
			println('#$c [$to.src] q[$to.q] ($to.res, $to.res_str)')
		}

		mut re := regex.regex_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}

		re.reset()
		res := re.find_all(to.src)
		if res != to.res {
			eprintln('err: find_all !!')
			if debug {
				println('#$c exp: $to.res calculated: $res')
			}
			assert false
		}

		res_str := re.find_all_str(to.src)
		if res_str != to.res_str {
			eprintln('err: find_all_str !!')
			if debug {
				println('#$c exp: $to.res_str calculated: $res_str')
			}
			assert false
		}
	}

	// check split
	for c, to in split_test_suite {
		// debug print
		if debug {
			println('#$c [$to.src] q[$to.q] ($to.res)')
		}

		mut re := regex.regex_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}

		re.reset()
		res := re.split(to.src)
		if res != to.res {
			eprintln('err: split !!')
			if debug {
				println('#$c exp: $to.res calculated: $res')
			}
			assert false
		}
	}

	// check replace
	for c, to in match_test_suite_replace {
		// debug print
		if debug {
			println('#$c [$to.src] q[$to.q] $to.r')
		}

		mut re := regex.regex_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}

		res := re.replace(to.src, to.rep)
		if res != to.r {
			eprintln('ERROR: replace.')
			assert false
			continue
		}
	}

	// check replace simple
	for c, to in match_test_suite_replace_simple {
		// debug print
		if debug { println('#$c [$to.src] q[$to.q] $to.r') }

		mut re := regex.regex_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}

		res := re.replace_simple(to.src, to.rep)
		if res != to.r {
			eprintln('ERROR: replace.')
			assert false
			continue
		}
	}

	// check match and find
	for c, to in match_test_suite {
		// debug print
		if debug { println('#$c [$to.src] q[$to.q] $to.s $to.e') }

		// test the find
		if to.s > 0 {
			mut re := regex.regex_opt(to.q) or {
				eprintln('err: $err')
				assert false
				continue
			}
			// q_str := re.get_query()
			// eprintln("Query: $q_str")
			start, end := re.find(to.src)

			if start != to.s || end != to.e {
				err_str := re.get_parse_error_string(start)
				eprintln('ERROR : $err_str start: $start end: $end')
				assert false
			} else {
				// tmp_str := text[start..end]
				// println("found in [$start, $end] => [$tmp_str]")
				assert true
			}
			continue
		}

		// test the match
		mut re := regex.new()
		// re.debug = true

		re.compile_opt(to.q) or {
			eprintln('err: $err')
			assert false
			continue
		}
		// println("#$c [$to.src] q[$to.q]")
		start, end := re.match_string(to.src)

		mut tmp_str := ''
		if start >= 0 && end > start {
			tmp_str = to.src[start..end]
		}

		if start != to.s || end != to.e {
			eprintln('#$c [$to.src] q[$to.q] res[$tmp_str] $start, $end')
			eprintln('ERROR!')
			assert false
			continue
		}

		// test the match predicate
		if to.s >= 0 {
			assert re.matches_string(to.src)
		} else {
			assert !re.matches_string(to.src)
		}

		// rerun to test consistency
		tmp_str1 := to.src.clone()
		start1, end1 := re.match_string(tmp_str1)
		if start1 != start || end1 != end {
			eprintln('two run ERROR!!')
			assert false
			continue
		}
	}

	if debug { println('DONE!') }
}

// test regex_base function
fn test_regex_func() {
	query := r'\d\dabcd'
	test_str := '78abcd'
	mut re, re_err, err_pos := regex.regex_base(query)
	if re_err == regex.compile_ok {
		start, end := re.match_string(test_str)
		assert (start == 0) && (end == 6)
	} else {
		eprintln('Error in query string in pos $err_pos')
		eprintln('Error: ${re.get_parse_error_string(re_err)}')
		assert false
	}
}

fn my_repl_1(re regex.RE, in_txt string, start int, end int) string {
	s0 := re.get_group_by_id(in_txt, 0)
	println('[$start, $end] => $s0')
	return 'a' + s0.to_upper()
}

fn test_regex_func_replace1() {
	txt := 'abbabbbabbbbaabba'
	query := r'a(b+)'
	mut re := regex.regex_opt(query) or { panic(err) }
	result := re.replace_by_fn(txt, my_repl_1)

	assert result == 'aBBaBBBaBBBBaaBBa'
}

fn my_repl(re regex.RE, in_txt string, start int, end int) string {
	s0 := re.get_group_by_id(in_txt, 0)[0..1] + 'X'
	s1 := re.get_group_by_id(in_txt, 1)[0..1] + 'X'
	s2 := re.get_group_by_id(in_txt, 2)[0..1] + 'X'
	return '$s0$s1$s2'
}

// test regex replace function
fn test_regex_func_replace() {
	filler := "E il primo dei tre regni dell'Oltretomba cristiano visitato da Dante nel corso del viaggio, con la guida di Virgilio."
	txt := r'"content": "They dont necessarily flag "you will be buying these shares on margin!"", "channel_id"'
	query := r'"(content":\s+")(.*)(, "channel_id")'
	mut re := regex.regex_opt(query) or { panic(err) }

	mut txt1 := ''
	mut txt2 := ''

	for _ in 0 .. 3 {
		rnd := int(10 + rand.u32() % 20)
		txt1 += txt + filler[0..rnd] + '\n'
		txt2 += 'cXTX,X' + filler[0..rnd] + '\n'
	}

	result := re.replace_by_fn(txt1, my_repl)
	if debug {
		eprintln(result)
		eprintln(txt2)
	}
	assert result == txt2
}

fn rest_regex_replace_n() {
	s := 'dario 1234 pepep 23454 pera'
	query := r'\d+'

	mut re := regex.regex_opt(query) or { panic(err) }

	assert re.replace_n(s, '[repl]', 0) == 'dario 1234 pepep 23454 pera'
	assert re.replace_n(s, '[repl]', -1) == 'dario 1234 pepep [repl] pera'
	assert re.replace_n(s, '[repl]', 1) == 'dario [repl] pepep 23454 pera'
	assert re.replace_n(s, '[repl]', 2) == 'dario [repl] pepep [repl] pera'
	assert re.replace_n(s, '[repl]', -2) == 'dario [repl] pepep [repl] pera'
	assert re.replace_n(s, '[repl]', 3) == 'dario [repl] pepep [repl] pera'
	assert re.replace_n(s, '[repl]', -3) == 'dario [repl] pepep [repl] pera'

	// mut res := re.replace_n(s, "[repl]", -1)
	// println("source: ${s}")
	// println("res   : ${res}")
}

// test quantifier wrong sequences
const (
	test_quantifier_sequences_list = [
		r'+{3}.*+{3}',
		r'+{3}.*?{3}',
		r'+{3}.**{3}',
		r'+{3}.*\+{3}*',
		r'+{3}.*\+{3}+',
		r'+{3}.*\+{3}??',
		r'+{3}.*\+{3}{4}',
	]
)

fn test_quantifier_sequences() {
	for pattern in test_quantifier_sequences_list {
		re, re_err, err_pos := regex.regex_base(pattern)
		if re_err != regex.err_syntax_error {
			eprintln('pattern: $pattern => $re_err')
		}
		assert re_err == regex.err_syntax_error
	}
}

// test group index in find
struct Test_find_groups {
	src string
	q   string
	s   int   // start index
	e   int   // end index
	res []int // groups indexes
}

const (
find_groups_test_suite = [
	Test_find_groups{
		"aabbbccccdd",
		r"(b+)(c+)",
		2,
		9,
		[2, 5, 5, 9],
	},
	Test_find_groups{
		"aabbbccccdd",
		r"(a+).*(c+)",
		0,
		9,
		[0, 2, 5, 9],
	},
	Test_find_groups{
		"aabbbccccdd",
		r"((b+).*)(d+)",
		2,
		11,
		[2, 9, 2, 5, 9, 11],
	},
]
)

fn test_groups_in_find() {
	for test_obj in find_groups_test_suite {
		src_text := test_obj.src
		query := test_obj.q
		mut re := regex.regex_opt(query) or { panic(err) }
		start, end := re.find(src_text)
		// Debug print do not remove!!
		/*
		println("---------")
		println("src_text:[${src_text}]")
		println("query   :[${query}]")
		println("[${start}, ${end}]")
		println(re.groups)
		mut gi := 0
		for gi < re.groups.len {
			if re.groups[gi] >= 0 {
				println('${gi / 2} :[${src_text[re.groups[gi]..re.groups[gi + 1]]}]')
			}
			gi += 2
		}
		*/
		// check
		assert start == test_obj.s
		assert end == test_obj.e
		assert re.groups == test_obj.res
	}
}

const (
	err_query_list = [
		r'([a]|[b])*',
	]
)

fn test_errors() {
	mut count := 0
	for query in err_query_list {
		_, err, _ := regex.regex_base(query)
		if err != regex.compile_ok {
			count++
		}
	}
	assert count == err_query_list.len
}

fn test_long_query() {
	test_len := 32768
	mut buf := strings.new_builder(test_len * 3)
	base_string := rand.string(test_len)

	for c in base_string {
		buf.write_u8(`(`)
		buf.write_u8(c)
		buf.write_u8(`)`)
	}

	mut query := buf.str()

	// println(base_string)
	// println(buf.str())

	// test 1
	mut re := regex.regex_opt(query) or { panic(err) }
	mut start, mut end := re.match_string(base_string)
	// println("$start, $end")
	assert start >= 0 && end == base_string.len

	// test 2
	buf.clear()
	for c in base_string {
		buf.write_u8(`(`)
		buf.write_u8(c)
	}
	for _ in 0 .. base_string.len {
		buf.write_u8(`)`)
	}
	query = buf.str()
	re = regex.regex_opt(query) or { panic(err) }
	start, end = re.match_string(base_string)
	// println("$start, $end")
	assert start >= 0 && end == base_string.len
}

struct Test_negation_group {
	src string
	res bool
}

const (
	negation_groups = [
		Test_negation_group{'automobile', false},
		Test_negation_group{'botomobile', true},
		Test_negation_group{'auto_caravan', false},
		Test_negation_group{'moto_mobile', true},
		Test_negation_group{'pippole', true},
		Test_negation_group{'boring test', false},
	]
)

fn test_negation_groups() {
	mut query := r'(?!auto)\w+le'
	mut re := regex.regex_opt(query) or { panic(err) }
	for test in negation_groups {
		start, end := re.match_string(test.src)
		assert (start >= 0) == test.res
	}
}
