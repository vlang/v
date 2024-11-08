module asn1

import math.big

fn test_parse_sequence_match_length() ! {
	// from https://en.wikipedia.org/wiki/ASN.1#Example_encoded_in_DER
	data := [u8(0x30), 0x13, 0x02, 0x01, 0x05, 0x16, 0x0e, 0x41, 0x6e, 0x79, 0x62, 0x6f, 0x64,
		0x79, 0x20, 0x74, 0x68, 0x65, 0x72, 0x65, 0x3f]

	seq, n := Sequence.decode(data)!
	assert n == data.len
	assert seq.payload()!.len == 19 // 0x13
	assert encoded_len(seq) == 21
	assert seq.tag().tag_class() == .universal
	assert seq.tag().is_constructed() == true
	assert seq.tag().tag_number() == 16

	els := seq.fields()
	assert els.len == 2
	assert els[0] is Integer
	assert els[1] is IA5String

	assert els[0].tag().tag_number() == int(TagType.integer)
	assert els[1].tag().tag_number() == int(TagType.ia5string)
	assert els[1].payload()! == 'Anybody there?'.bytes()

	/*
	30 — type tag indicating SEQUENCE
		13 — length in octets of value that follows
  			02 — type tag indicating INTEGER
  				01 — length in octets of value that follows
    			05 — value (5)
 			16 — type tag indicating IA5String
     			(IA5 means the full 7-bit ISO 646 set, including variants,
      			but is generally US-ASCII)
  				0e — length in octets of value that follows
    			41 6e 79 62 6f 64 79 20 74 68 65 72 65 3f — value ("Anybody there?")
	*/
}

/*
Certificate  ::=  SEQUENCE  {
        tbsCertificate       TBSCertificate,
        signatureAlgorithm   AlgorithmIdentifier,
        signatureValue       BIT STRING  }

TBSCertificate  ::=  SEQUENCE  {
     version         [0]  EXPLICIT Version DEFAULT v1,
     serialNumber         CertificateSerialNumber,
     signature            AlgorithmIdentifier,
     issuer               Name,SubjectPublicKeyInfo
     validity             Validity,
     subject              Name,
     subjectPublicKeyInfo SubjectPublicKeyInfo,
     issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
                          -- If present, version MUST be v2 or v3
     subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
                          -- If present, version MUST be v2 or v3
     extensions      [3]  EXPLICIT Extensions OPTIONAL
                          -- If present, version MUST be v3 }

Validity ::= SEQUENCE {
     notBefore      Time,
     notAfter       Time }

AlgorithmIdentifier  ::=  SEQUENCE  {
        algorithm   OBJECT IDENTIFIER,
        parameters  ANY DEFINED BY algorithm OPTIONAL
      }
*/

// FIXED: NOTE: Need to be fixed
fn test_x509_certificate_version() ! {
	// version         [0]  EXPLICIT Version DEFAULT v1,
	// Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }
	val := Integer.from_int(2)
	version := ContextElement.explicit_context(val, 0)!

	out := encode(version)!
	exp := [u8(0xA0), 0x03, 0x02, 0x01, 0x02]

	assert out == exp
}

fn test_x509_certificate_signature() ! {
	// signature            AlgorithmIdentifier,
	// AlgorithmIdentifier  ::=  SEQUENCE  {
	//    algorithm   OBJECT IDENTIFIER,
	//    parameters  ANY DEFINED BY algorithm OPTIONAL
	//  }
	oid := ObjectIdentifier.new('1.3.101.112')!

	mut seq := Sequence{}
	seq.add_element(oid)!

	out := encode(seq)!
	exp := [u8(0x30), 0x05, 0x06, 0x03, 0x2B, 0x65, 0x70]

	assert out == exp
	seqback := decode(exp)!

	assert seqback.equal(seq)
}

fn test_x509_certificate_serialnumber() ! {
	// serialNumber         CertificateSerialNumber,
	// CertificateSerialNumber  ::=  INTEGER
	sn := Integer.from_bigint(big.integer_from_string('711090297755414526861352146244170174161660335942')!)

	mut out := encode(sn)!
	exp := [u8(0x02), 0x14, 0x7C, 0x8E, 0x64, 0x49, 0xD7, 0x0E, 0xD9, 0x2D, 0x3E, 0x2E, 0x4A, 0x5D,
		0x2F, 0x76, 0xF6, 0x55, 0x42, 0x46, 0xD7, 0x46]

	assert out == exp
}
