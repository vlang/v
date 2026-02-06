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
	module []rune
}

fn build_ast(tokens []SecondTokenizerToken) !RootAST {
	token := tokens[0]
	match true {
		token.type == .keyword && token.value == ?SecondTokenizerValue(Keyword.module) {
			return error('Handling module')
		}
		token.type == .keyword && token.value == ?SecondTokenizerValue(Keyword.import) {
			return error('Handling import')
		}
		else {
			return error('else')
		}
	}

	return error('Not implemented')
}
