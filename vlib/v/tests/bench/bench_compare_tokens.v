import v.token
import benchmark

const max_repetitions = 4_000_000

fn main() {
	km := token.new_keywords_matcher(token.keywords)
	for kw in ['for', 'val', 'int', 'f32', 'struct', 'return', 'if', 'in', 'as', 'or', 'else',
		'unsafe', 'return', 'assert', 'Abc', 'my_identifier', 'a'] {
		mut res := token.Kind{}
		mut bmark := benchmark.start()
		for _ in 0 .. max_repetitions {
			res = token.keywords[kw]
		}
		bmark.measure('$max_repetitions repetitions of token.keywords["$kw"] = $res')
		for _ in 0 .. max_repetitions {
			res = km.find(kw)
		}
		bmark.measure('$max_repetitions repetitions of km.find("$kw") = $res')
		println('--------------------------------')
	}
}
