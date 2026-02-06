module main

type SecondTokenizerValue = []rune | Keyword

enum AdvancedTokenType {
	identifier
	keyword
	newline
}

struct SecondTokenizerToken {
	type  AdvancedTokenType
	value ?SecondTokenizerValue
}

enum Keyword {
	module
	import
}

fn test_main() {
	mut tokens := []SecondTokenizerToken{}
	tokens << SecondTokenizerToken{
		type:  AdvancedTokenType.keyword
		value: Keyword.module
	}
	build_ast(tokens) or { assert err.msg() == 'Handling module' }
}

struct RootAST {
mut:
	module []rune
}

fn build_ast(tokens []SecondTokenizerToken) !RootAST {
	for _ in 0 .. 1 {
		token := tokens[0]
		match true {
			false {
				return error('Expected `module` keyword at the start of the file, but got `${token.type}`')
			}
			token.type == .keyword && token.value == ?SecondTokenizerValue(Keyword.module) {
				return error('Handling module')
			}
			else {
				continue
			}
		}
	}

	return error('Not implemented')
}
