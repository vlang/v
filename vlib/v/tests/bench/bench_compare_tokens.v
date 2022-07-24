import v.token
import benchmark

const max_repetitions = 4_000_000

fn main() {
	mut res := token.Kind{}
	km := token.new_keywords_matcher(token.keywords)
	km_trie := token.new_keywords_matcher_trie(token.keywords)
	for kw in ['for', 'val', 'int', 'f32', 'struct', 'return', 'if', 'in', 'as', 'or', 'else',
		'unsafe', 'return', 'assert', 'Abc', 'my_identifier', 'a', 'assez', 'returned'] {
		mut bmark := benchmark.start()
		for _ in 0 .. max_repetitions {
			res = token.keywords[kw]
		}
		bmark.measure('$max_repetitions repetitions of token.keywords["$kw"] = $res')

		for _ in 0 .. max_repetitions {
			res = token.Kind(km.find(kw))
		}
		bmark.measure('$max_repetitions repetitions of km.find("$kw") = $res')

		for _ in 0 .. max_repetitions {
			res = token.Kind(km_trie.find(kw))
		}
		bmark.measure('$max_repetitions repetitions of km_trie.find("$kw") = $res')

		println('--------------------------------')
	}
}
