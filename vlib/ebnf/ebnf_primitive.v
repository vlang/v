module ebnf

enum Token {
    nil
    expression
    alternative
    term
    group
    option_tok
    repetion
}

struct Scanner {
    tree Token
}

fn parse() {

}