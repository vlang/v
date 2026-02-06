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

pub struct FileAST {
pub mut:
	module []rune
}

pub fn build_ast(tokens []SecondTokenizerToken) !FileAST {
	mut file_ast := FileAST{}
	mut i := -1

	for i < tokens.len - 1 {
		i++
		token := tokens[i]
		match true {
			file_ast.module.len == 0 && token.type == .keyword
				&& token.value == ?SecondTokenizerValue(Keyword.module) {
				file_ast.module = [`a`]
			}
			file_ast.module.len == 0 {
				return error('Expected `module` keyword at the start of the file, but got `${token.type}`')
			}
			token.type == .keyword && token.value == ?SecondTokenizerValue(Keyword.module) {
				return error('Multiple `module` declarations are not allowed, but got another one')
			}
			token.type == .keyword && token.value == ?SecondTokenizerValue(Keyword.import) {
				dump('import')
			}
			token.type == .newline {
				// ignore newlines
				continue
			}
			else {
				// TODO: turn this into an error after implementing all other AST nodes
				continue
			}
		}
	}

	return file_ast
}

fn test_main() {
	build_ast([]SecondTokenizerToken{}) or { panic(err) }
}
