(function() {
    Prism.languages.v = Prism.languages.extend('clike', {
        keyword: /\b(?:pub|break|const|continue|defer|else|for|fn|go(?:to)?|if|import|module|return|interface|struct|match|type|mut|is|as|map|__global|enum)\b/,
        builtin: /\b(?:bool|string|i8|i16|int|i64|i128|byte|u16|u32|u64|u128|rune|f32|f64|any_int|any_float|byteptr|voidptr|any)\b/,
        boolean: /\b(?:_|true|false)\b/,
        operator: /[*\/%^!=]=?|\+[=+]?|-[=-]?|\|[=|]?|&(?:=|&|\^=?)?|>(?:>=?|=)?|<(?:<=?|=|-)?|:=|\.\.\./,
        number: /(?:\b0x[a-f\d]+|(?:\b\d+\.?\d*|\B\.\d+)(?:e[-+]?\d+)?)i?/i,
        string: {
            pattern: /(["'`])(?:\\[\s\S]|(?!\1)[^\\])*\1/,
            greedy: true,
        },
    });
    delete Prism.languages.v['class-name'];
})();
