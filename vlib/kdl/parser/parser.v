module parser

import kdl.tokenizer
import kdl.document
import kdl.relaxed

// Parser is the KDL recursive-descent parser.
struct Parser {
mut:
	scan     tokenizer.Scanner
	tok      tokenizer.Token
	peeked   tokenizer.Token
	has_peek bool
}

fn new_parser(src string, opts relaxed.RelaxedNonCompliant, parse_comments bool) Parser {
	mut p := Parser{
		scan: tokenizer.new_scanner(src, opts, parse_comments)
	}
	p.advance()
	return p
}

fn (mut p Parser) advance() {
	if p.has_peek {
		p.tok = p.peeked
		p.has_peek = false
	} else {
		p.tok = p.scan.next()
	}
}

fn (mut p Parser) peek() tokenizer.Token {
	if !p.has_peek {
		p.peeked = p.scan.next()
		p.has_peek = true
	}
	return p.peeked
}

fn kdl_err(line int, col int, msg string) IError {
	return &document.KdlParseError{
		line:   line
		column: col
		offset: 0
		msg:    msg
	}
}

// parse parses KDL text into a Document (without options — backward compat).
pub fn parse(text string) !document.Document {
	return parse_opts(text, relaxed.RelaxedNonCompliant{}, false)
}

// parse_opts parses KDL text into a Document with the given options.
pub fn parse_opts(text string, opts relaxed.RelaxedNonCompliant, parse_comments bool) !document.Document {
	mut p := new_parser(text, opts, parse_comments)
	return p.parse_document()
}
