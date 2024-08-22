fn parse_expression(tokens []Token) &Node {
    mut current := 0
    mut root := &Node{}
    
    while current < tokens.len {
        token := tokens[current]
        match token.typ {
            .ident {
                root = new_node(.variable, token.value, null, null, [])
            }
            .int {
                root = new_node(.literal, token.value, null, null, [])
            }
            .assign {
                left := parse_expression(tokens[current + 1..])
                right := parse_expression(tokens[current + 3..])
                root = new_node(.assignment, '=', left, right, [])
                current += 3
            }
            .plus {
                left := parse_expression(tokens[current + 1..])
                right := parse_expression(tokens[current + 3..])
                root = new_node(.addition, '+', left, right, [])
                current += 3
            }
            .function {
                func_name := tokens[current + 1].value
                args := parse_expression(tokens[current + 2..])
                root = new_node(.function, func_name, null, null, [args])
                current += 2
            }
            .call {
                func_name := tokens[current + 1].value
                args := parse_expression(tokens[current + 2..])
                root = new_node(.call, func_name, null, null, [args])
                current += 2
            }
            else {
                current++
            }
        }
    }
    
    return root
}
