import json2 as json
import time
import math.big

type StrAlias = string
type BoolAlias = bool
type IntAlias = int
type FloatAlias = f64

enum TestEnum {
	a
	b
	c = 10
}

type EnumAlias = TestEnum

type Sum = int | string
type SumAlias = Sum

struct Basic {
	a int
	b string
	c bool
}

type BasicAlias = Basic

struct Opt {
	a ?int
}

type OptAlias = Opt

struct OptRequiered {
	a ?int @[required]
}

type OptRequieredAlias = OptRequiered

struct CustomString {
	data string
}

pub fn (cs CustomString) to_json() string {
	return '"<<<' + cs.data + '>>>"'
}

type CustomStringAlias = CustomString

type NullAlias = json.Null

type TimeAlias = time.Time

type BigAlias = big.Integer

struct NamedFields {
	a    int    @[json: 'id']
	name string @[json: 'Name']
}

type NamedFieldsAlias = NamedFields

struct SkipFields {
	a    int    @[json: '-']
	name string @[skip]
}

type SkipFieldsAlias = SkipFields

struct SkipSomeFields {
	a    int    @[json: '-']
	name string @[skip]
	hi   bool = true
}

type SkipSomeFieldsAlias = SkipSomeFields

struct PointerFields {
	next &PointerFields = unsafe { nil }
	data int
}

type PointerFieldsAlias = PointerFields

struct OmitFields {
	a ?bool   @[omitempty]
	b string  @[omitempty]
	c int     @[omitempty]
	d f64     @[omitempty]
	e ?string = '' @[omitempty]
	f ?int    = 0    @[omitempty]
	g ?f64    = 0.0    @[omitempty]
}

type OmitFieldsAlias = OmitFields

fn test_primitives() {
	assert json.encode('hello') == '"hello"'
	assert json.encode(StrAlias('hello')) == '"hello"'
	assert json.encode(true) == 'true'
	assert json.encode(BoolAlias(false)) == 'false'
	assert json.encode(-12345) == '-12345'
	assert json.encode(IntAlias(-12345)) == '-12345'
	assert json.encode(123.323) == '123.323'
	assert json.encode(FloatAlias(123.323)) == '123.323'
}

fn test_arrays() {
	assert json.encode([1, 2, 3, 4]) == '[1,2,3,4]'
}

fn test_maps() {
	assert json.encode({
		'hi':  0
		'bye': 1
	}) == '{"hi":0,"bye":1}'
}

fn test_enums() {
	assert json.encode(TestEnum.c) == '"c"'
	assert json.encode(EnumAlias(TestEnum.c)) == '"c"'
	assert json.encode(TestEnum.c, enum_as_int: true) == '10'
	assert json.encode(EnumAlias(TestEnum.c), enum_as_int: true) == '10'
}

fn test_sumtypes() {
	assert json.encode(Sum(10)) == '10'
	assert json.encode(Sum('hi')) == '"hi"'
	assert json.encode(SumAlias(10)) == '10'
	assert json.encode(SumAlias('hi')) == '"hi"'
}

fn test_basic_structs() {
	assert json.encode(Basic{
		a: 10
		b: 'hi'
		c: true
	}) == '{"a":10,"b":"hi","c":true}'

	assert json.encode(BasicAlias{
		a: 10
		b: 'hi'
		c: true
	}) == '{"a":10,"b":"hi","c":true}'
}

fn test_nested() {
	assert json.encode([
		{
			'hi':  Basic{ a: 1, b: 'a', c: false }
			'bye': Basic{
				a: 2
				b: 'b'
				c: true
			}
		},
		{
			'hi2':  Basic{
				a: 3
				b: 'c'
				c: false
			}
			'bye2': Basic{
				a: 4
				b: 'd'
				c: true
			}
		},
	]) == '[{"hi":{"a":1,"b":"a","c":false},"bye":{"a":2,"b":"b","c":true}},{"hi2":{"a":3,"b":"c","c":false},"bye2":{"a":4,"b":"d","c":true}}]'
	assert json.encode([
		{
			'hi':  Basic{ a: 1, b: 'a', c: false }
			'bye': Basic{
				a: 2
				b: 'b'
				c: true
			}
		},
		{
			'hi2':  Basic{
				a: 3
				b: 'c'
				c: false
			}
			'bye2': Basic{
				a: 4
				b: 'd'
				c: true
			}
		},
	],
		prettify: true
	) == '[
    {
        "hi": {
            "a": 1,
            "b": "a",
            "c": false
        },
        "bye": {
            "a": 2,
            "b": "b",
            "c": true
        }
    },
    {
        "hi2": {
            "a": 3,
            "b": "c",
            "c": false
        },
        "bye2": {
            "a": 4,
            "b": "d",
            "c": true
        }
    }
]'
}

fn test_string_escapes() {
	assert json.encode('normal escapes: ", \\ special control escapes: \b, \n, \f, \t, \r, other control escapes: \0, \u001b') == r'"normal escapes: \", \\ special control escapes: \b, \n, \f, \t, \r, other control escapes: \u0000, \u001b"'
	assert json.encode('ascii, é, 한, 😀, ascii') == r'"ascii, é, 한, 😀, ascii"'
	assert json.encode('ascii, é, 한, 😀, ascii', escape_unicode: true) == r'"ascii, \u00e9, \ud55c, \uD83D\ude00, ascii"'
}

fn test_options() {
	assert json.encode(Opt{none}) == '{}'
	assert json.encode(Opt{99}) == '{"a":99}'
	assert json.encode(OptAlias{none}) == '{}'
	assert json.encode(OptAlias{99}) == '{"a":99}'

	assert json.encode(OptRequiered{none}) == '{"a":null}'
	assert json.encode(OptRequiered{99}) == '{"a":99}'
	assert json.encode(OptRequieredAlias{none}) == '{"a":null}'
	assert json.encode(OptRequieredAlias{99}) == '{"a":99}'
}

fn test_custom_encoders() {
	assert json.encode(CustomString{'hi'}) == '"<<<hi>>>"'
	assert json.encode(CustomStringAlias{'hi'}) == '"<<<hi>>>"'

	assert json.encode(json.Null{}) == 'null'
	assert json.encode(NullAlias{}) == 'null'

	assert json.encode(time.Time{}) == '"0000-00-00T00:00:00.000Z"'
	assert json.encode(TimeAlias{}) == '"0000-00-00T00:00:00.000Z"'

	assert json.encode(big.integer_from_i64(1234567890)) == '1234567890'
	assert json.encode(BigAlias(big.integer_from_i64(1234567890))) == '1234567890'
}

fn test_named_fields() {
	assert json.encode(NamedFields{ a: 1, name: 'john' }) == '{"id":1,"Name":"john"}'
	assert json.encode(NamedFieldsAlias{ a: 1, name: 'john' }) == '{"id":1,"Name":"john"}'
}

fn test_skip_fields() {
	assert json.encode(SkipFields{ a: 1, name: 'john' }) == '{}'
	assert json.encode(SkipFieldsAlias{ a: 1, name: 'john' }) == '{}'
	assert json.encode(SkipFields{ a: 1, name: 'john' },
		prettify: true
	) == '{}'

	assert json.encode(SkipSomeFields{ a: 1, name: 'john' }) == '{"hi":true}'
	assert json.encode(SkipSomeFieldsAlias{ a: 1, name: 'john' }) == '{"hi":true}'
	assert json.encode(SkipSomeFields{ a: 1, name: 'john' },
		prettify: true
	) == '{
    "hi": true
}'
}

fn test_omit_fields() {
	assert json.encode(OmitFields{}) == '{}'
	assert json.encode(OmitFieldsAlias{}) == '{}'
}

fn test_pointer_fields() {
	assert json.encode(PointerFields{
		next: &PointerFields{
			next: &PointerFields{
				next: &PointerFields{
					data: 4
				}
				data: 3
			}
			data: 2
		}
		data: 1
	}) == '{"next":{"next":{"next":{"data":4},"data":3},"data":2},"data":1}'
	assert json.encode(PointerFieldsAlias{
		next: &PointerFieldsAlias{
			next: &PointerFieldsAlias{
				next: &PointerFieldsAlias{
					data: 4
				}
				data: 3
			}
			data: 2
		}
		data: 1
	}) == '{"next":{"next":{"next":{"data":4},"data":3},"data":2},"data":1}'
	assert json.encode(PointerFields{
		next: &PointerFields{
			next: &PointerFields{
				next: &PointerFields{
					data: 4
				}
				data: 3
			}
			data: 2
		}
		data: 1
	},
		prettify: true
	) == '{
    "next": {
        "next": {
            "next": {
                "data": 4
            },
            "data": 3
        },
        "data": 2
    },
    "data": 1
}'
}
