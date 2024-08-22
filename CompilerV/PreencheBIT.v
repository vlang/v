fn generate_assembly(node &Node) string {
    mut assembly := ''
    
    match node.typ {
        .literal {
            assembly += 'mov r0, #' + node.value + '\n'
        }
        .variable {
            assembly += 'ldr r0, =var_' + node.value + '\n'
        }
        .assignment {
            left := generate_assembly(node.left)
            right := generate_assembly(node.right)
            assembly += left
            assembly += right
            assembly += 'str r1, [r0]\n'
        }
        .addition {
            left := generate_assembly(node.left)
            right := generate_assembly(node.right)
            assembly += left
            assembly += right
            assembly += 'add r0, r0, r1\n'
        }
        .function {
            assembly += 'bl ' + node.value + '\n'
        }
        .call {
            assembly += 'bl ' + node.value + '\n'
        }
        else {
            assembly += ''
        }
    }
    
    return assembly
}

fn main() {
    // Exemplo de tokens
    tokens := [
        Token{typ: .ident, value: 'five'},
        Token{typ: .assign, value: '='},
        Token{typ: .int, value: '5'},
        Token{typ: .semicolon, value: ';'},
        Token{typ: .ident, value: 'add'},
        Token{typ: .assign, value: '='},
        Token{typ: .function, value: 'fn'},
        Token{typ: .lparen, value: '('},
        Token{typ: .ident, value: 'x'},
        Token{typ: .comma, value: ','},
        Token{typ: .ident, value: 'y'},
        Token{typ: .rparen, value: ')'},
        Token{typ: .lbrace, value: '{'},
        Token{typ: .ident, value: 'x'},
        Token{typ: .plus, value: '+'},
        Token{typ: .ident, value: 'y'},
        Token{typ: .semicolon, value: ';'},
        Token{typ: .rbrace, value: '}'},
    ]
    
    lexer := new_lexer('')
    parser := new_parser(lexer)
    root := parse_expression(tokens)
    
    assembly := generate_assembly(root)
    println('Generated Assembly Code:\n$assembly')
}
