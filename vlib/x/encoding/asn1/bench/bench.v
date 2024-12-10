import time
import x.encoding.asn1

// This Benchmark was performs serialization and deserialization from ASN.1 schema defined as:
// ```asn.1
// Example ::= SEQUENCE {
//    greeting    UTF8String,
//    answer      INTEGER,
//    type        [1] EXPLICIT OBJECT IDENTIFIER
// }
// ```
struct Example {
	greeting asn1.Utf8String
	answer   asn1.Integer
	// you can tag your struct fields with supported options.
	tipe asn1.ObjectIdentifier @[context_specific: 1; explicit; inner: 6]
}

fn (ex Example) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

// you can build your payload manually or use `asn1.make_payload`, but be aware,
// if your structure contains generics, its may not work (currently).
// Updates: Thank to @felipensp. He has a great job to resolve this issue.
// Its was resolved in [22724](https://github.com/vlang/v/pull/22724)
fn (ex Example) payload() ![]u8 {
	kd := asn1.KeyDefault(map[string]asn1.Element{})
	payload := asn1.make_payload[Example](ex, kd)!

	return payload
}

// You can write custom decode routines for the Example structure. This is only one
// example, but its possible to do this in other ways with help from this module such
// as using the `Parser` codec.
fn Example.decode(bytes []u8) !Example {
	// just call raw .decode on bytes
	// for example, its should produce sequence type.
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag) // should true

	// cast produced element into Sequence type and get the fields.
	seq := elem.into_object[asn1.Sequence]()!
	fields := seq.fields()

	// and then, turn every field into desired objects based on your schema.
	// first two field is not wrapped element, so just turn into real object.
	greeting := fields[0].into_object[asn1.Utf8String]()!
	answer := fields[1].into_object[asn1.Integer]()!

	// the third field is a context_specific wrapped element, just unwrap it with the
	// same options used to encode.
	oid_tipe := fields[2].unwrap_with_options('context_specific:1;explicit; inner:6')!
	tipe := oid_tipe.into_object[asn1.ObjectIdentifier]()!

	// then build your Example struct
	ex := Example{
		greeting: greeting
		answer:   answer
		tipe:     tipe
	}
	return ex
}

fn main() {
	expected_output := [u8(0x30), 18, u8(12), 5, 72, 101, 108, 108, 111, u8(2), 1, 42, u8(0xA1),
		6, 6, 4, 43, 6, 1, 3]
	ex := Example{
		greeting: asn1.Utf8String.new('Hello')!
		answer:   asn1.Integer.from_int(42)
		tipe:     asn1.ObjectIdentifier.from_ints([1, 3, 6, 1, 3])!
	}
	iterations := 1000

	println('Benchmarking ASN.1 encode...')
	mut total_enc_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := asn1.encode(ex) or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_enc_time += elapsed
	}
	avg_enc_time := total_enc_time / iterations
	println('Average example encode time: ${avg_enc_time} µs')

	println('Benchmarking ASN.1 decode (with asn1.decode)...')
	mut total_dec_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := asn1.decode(expected_output) or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_dec_time += elapsed
	}
	avg_asn1dec_time := total_dec_time / iterations
	println('Average (asn1.decode) decode time: ${avg_asn1dec_time} µs')

	println('Benchmarking ASN.1 decode with Example.decode)...')
	mut total_exdec_time := i64(0)
	for _ in 0 .. iterations {
		sw := time.new_stopwatch()
		_ := asn1.decode(expected_output) or { panic(err) }
		elapsed := sw.elapsed().microseconds()
		total_exdec_time += elapsed
	}
	avg_exdec_time := total_exdec_time / iterations
	println('Average (Example.decode) decode time: ${avg_exdec_time} µs')
}
