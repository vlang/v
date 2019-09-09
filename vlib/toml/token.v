import toml

enum Token{
    nil
    eof
    text
    str
    raw_str
    multiline_string
    boolean
    integer
    float
    datetime
    array_start
    array_end
    table_start
    table_end
    table_sep
    key_sep
    array_table_start
    array_table_end
    key_start
}

fn build_token_str() []string{
    mut s := ['', NrTokens]
    s[Token.eof] = 'eof'
    s[Token.table_start] = '['
    s[Token.table_end] = ']'
    s[Token.array_table_start] = '['
    s[Token.array_table_end] = ']'
}