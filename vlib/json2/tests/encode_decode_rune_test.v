import json2

// Like the removed `json` module, a rune is encoded as a JSON string of its
// character, and decoded from one.

type Letter = rune

struct RuneHolder {
	ch     rune
	chars  []rune
	opt    ?rune
	by_key map[string]rune
	letter Letter
}

struct NumberHolder {
	a u32
	b i32
	c int
}

fn test_encode_rune() {
	assert json2.encode(`a`) == '"a"'
	holder := RuneHolder{
		ch:     `é`
		chars:  [`x`, `😀`]
		opt:    `"`
		by_key: {
			'k': `\n`
		}
		letter: Letter(`z`)
	}
	assert json2.encode(holder) == '{"ch":"é","chars":["x","😀"],"opt":"\\"","by_key":{"k":"\\n"},"letter":"z"}'
	assert json2.encode(RuneHolder{ ch: `é` }, escape_unicode: true) == '{"ch":"\\u00e9","chars":[],"by_key":{},"letter":"\\u0000"}'
}

fn test_decode_rune() {
	holder := json2.decode[RuneHolder]('{"ch":"é","chars":["x","😀"],"opt":"q","by_key":{"k":"\\n"},"letter":"z"}')!
	assert holder.ch == `é`
	assert holder.chars == [`x`, `😀`]
	assert holder.opt? == `q`
	assert holder.by_key['k'] == `\n`
	assert holder.letter == Letter(`z`)
	assert json2.decode[rune]('"z"')! == `z`
	assert json2.decode[rune]('"xyz"')! == `x`
	assert json2.decode[rune]('""')! == rune(0)
	assert json2.decode[rune]('97')! == `a`
	original := RuneHolder{
		ch:    `ü`
		chars: [`1`]
		opt:   `?`
	}
	assert json2.decode[RuneHolder](json2.encode(original, escape_unicode: true))! == original
}

fn test_integers_stay_numbers() {
	numbers := NumberHolder{
		a: 4000000000
		b: -5
		c: 7
	}
	assert json2.encode(numbers) == '{"a":4000000000,"b":-5,"c":7}'
	assert json2.decode[NumberHolder]('{"a":4000000000,"b":-5,"c":7}')! == numbers
}
