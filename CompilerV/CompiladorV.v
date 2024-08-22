module main

import (
    strconv
    io
)

enum TokenType {
    illegal
    eof
    ws
    ident
    int
    assign
    semicolon
    lparen
    rparen
    plus
    function
    comma
}

struct Token {
    typ   TokenType
    value string
}

struct Lexer {
    input        string
    position     int
    read_position int
    ch           byte
}

fn new_lexer(input string) &Lexer {
    mut l := &Lexer{
        input: input
    }
    l.read_char()
    return l
}

fn (mut l Lexer) read_char() {
    if l.read_position >= l.input.len {
        l.ch = 0
    } else {
        l.ch = l.input[l.read_position]
    }
    l.position = l.read_position
    l.read_position++
}

fn (mut l Lexer) next_token() Token {
    l.skip_whitespace()

    mut tok := Token{}

    match l.ch {
        0 {
            tok = Token{typ: .eof, value: ''}
        }
        `=` {
            tok = Token{typ: .assign, value: l.ch.ascii_str()}
        }
        `;` {
            tok = Token{typ: .semicolon, value: l.ch.ascii_str()}
        }
        `(` {
            tok = Token{typ: .lparen, value: l.ch.ascii_str()}
        }
        `)` {
            tok = Token{typ: .rparen, value: l.ch.ascii_str()}
        }
        `+` {
            tok = Token{typ: .plus, value: l.ch.ascii_str()}
        }
        `,` {
            tok = Token{typ: .comma, value: l.ch.ascii_str()}
        }
        else {
            if is_letter(l.ch) {
                value := l.read_identifier()
                if value == 'fn' {
                    tok = Token{typ: .function, value: value}
                } else {
                    tok = Token{typ: .ident, value: value}
                }
                return tok
            } else if is_digit(l.ch) {
                tok = Token{typ: .int, value: l.read_number()}
                return tok
            } else {
                tok = Token{typ: .illegal, value: l.ch.ascii_str()}
                println('Unexpected character: $l.ch')
            }
        }
    }

    l.read_char()
    return tok
}

fn (mut l Lexer) read_identifier() string {
    start := l.position
    for is_letter(l.ch) {
        l.read_char()
    }
    return l.input[start..l.position]
}

fn (mut l Lexer) read_number() string {
    start := l.position
    for is_digit(l.ch) {
        l.read_char()
    }
    return l.input[start..l.position]
}

fn (mut l Lexer) skip_whitespace() {
    for l.ch in [` `, `\t`, `\n`, `\r`] {
        l.read_char()
    }
}

fn is_letter(ch byte) bool {
    return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_`
}

fn is_digit(ch byte) bool {
    return ch >= `0` && ch <= `9`
}

struct Environment {
    variables map[string]bool
    functions map[string]bool
}

fn new_environment() &Environment {
    return &Environment{
        variables: map[string]bool{},
        functions: map[string]bool{}
    }
}

fn (mut env Environment) declare_variable(name string) {
    env.variables[name] = true
}

fn (mut env Environment) declare_function(name string) {
    env.functions[name] = true
}

fn (env &Environment) is_variable_declared(name string) bool {
    return env.variables[name]
}

fn (env &Environment) is_function_declared(name string) bool {
    return env.functions[name]
}

struct Parser {
    lexer         &Lexer
    current_token Token
    env           &Environment
}

fn new_parser(lexer &Lexer) &Parser {
    return &Parser{
        lexer: lexer,
        env: new_environment()
    }
}

fn (mut p Parser) parse() {
    for p.current_token.typ != .eof {
        match p.current_token.typ {
            .ident {
                if !p.env.is_variable_declared(p.current_token.value) {
                    println('Error: Variable \'$p.current_token.value\' used before declaration.')
                }
                p.next_token() // move to next token
            }
            .function {
                p.next_token()
                if p.current_token.typ == .ident {
                    p.env.declare_function(p.current_token.value)
                    p.next_token()
                } else {
                    println('Error: Expected function name after \'fn\'.')
                }
            }
            .assign {
                p.next_token()
                if p.current_token.typ == .ident {
                    p.env.declare_variable(p.current_token.value)
                    p.next_token()
                } else {
                    println('Error: Expected variable name after \'=\'.')
                }
            }
            else {
                p.next_token()
            }
        }
    }
}

fn (mut p Parser) next_token() {
    p.current_token = p.lexer.next_token()
}

fn main() {
    input := 'let five = 5;\nlet ten = 10;\nlet add = fn(x, y) {\n  x + y;\n};\nlet result = add(five, ten);\n'
    l := new_lexer(input)
    mut p := new_parser(l)

    p.next_token() // Initialize the parser with the first token
    p.parse()
}
