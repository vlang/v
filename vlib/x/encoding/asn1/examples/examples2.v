module main

import x.encoding.asn1

// This example takes more complex scenario, in the sense of nested wrapping, the use of
// other class Element supported in this module.
//
// This examples is taken from ITU-T X.690 Information technology – ASN.1 encoding rules:
// Specification of Basic Encoding Rules (BER), Canonical Encoding Rules (CER) and
// Distinguished Encoding Rules (DER) document.
//
// Especially from Annex A. Example of encodings of the document.

// from A.1 ASN.1 description of the record structure.
// The structure of the hypothetical personnel record is formally described below using ASN.1 specified in
// ITU-T Rec. X.680 | ISO/IEC 8824-1 for defining types.
//
// PersonnelRecord ::= [APPLICATION 0] IMPLICIT SET {
//      name            Name,
//      title           [0] VisibleString,
//      number          EmployeeNumber,
//      dateOfHire      [1] Date,
//      nameOfSpouse    [2] Name,
//      children        [3] IMPLICIT SEQUENCE OF ChildInformation DEFAULT {}
// }
//
struct PersonnelRecord {
	name           Name
	title          asn1.VisibleString
	number         EmployeeNumber
	date_of_hire   Date
	name_of_spouse Name
	children       asn1.SequenceOf[ChildInformation]
}

fn (pr PersonnelRecord) tag() asn1.Tag {
	return asn1.default_set_tag
}

fn (pr PersonnelRecord) payload() ![]u8 {
	mut out := []u8{}
	out << asn1.encode(pr.name)!
	out << asn1.encode_with_options(pr.title, 'context_specific:0;explicit;inner:26')!
	out << asn1.encode(pr.number)!
	out << asn1.encode_with_options(pr.date_of_hire, 'context_specific: 1; explicit; inner:application,false,3')!
	out << asn1.encode_with_options(pr.name_of_spouse, 'context_specific: 2; explicit; inner:application,true,1')!
	out << asn1.encode_with_options(pr.children, 'context_specific: 3; implicit; inner:16')!

	return out
}

// deserializer of PersonellRecord bytes
fn PersonnelRecord.decode(bytes []u8) !PersonnelRecord {
	// we perfrom decoding as a reverse of the serialization with the same options.
	// after that, we get an Element (with the Set type of PersonellRecord)
	el := asn1.decode_with_options(bytes, 'application:0;implicit;inner:17')!
	assert el.tag() == asn1.default_set_tag
	set := el.into_object[asn1.Set]()!

	// fields is series of element ([]Element), PersonellRecord fields
	fields := set.fields()

	// we turn series of element into underlying desired type.
	// its rather clumsy to transforms it, because Sequenve (and Set) fields is an []Element, so you should care
	// to turn this into desired object
	name_app := fields[0].into_object[asn1.ApplicationElement]()!
	name := Name(name_app)

	// Title
	title_elem := fields[1].unwrap_with_options('context_specific:0;explicit;inner:26')!
	title := title_elem.into_object[asn1.VisibleString]()!

	// EmployeNumber
	emp_num := fields[2].into_object[asn1.ApplicationElement]()!
	employe_number := EmployeeNumber(emp_num)

	// dateOfHire
	doh := fields[3].unwrap_with_options('context_specific: 1; explicit; inner:application,false,3')!
	doh_app := doh.into_object[asn1.ApplicationElement]()!
	date_of_hire := Date(doh_app)

	// nameOfSpouse
	nosp := fields[4].unwrap_with_options('context_specific: 2; explicit; inner:application,true,1')!
	nosp_app := nosp.into_object[asn1.ApplicationElement]()!
	name_of_spouse := Name(nosp_app)

	// The children is Sequence of Set, first we unwrap it then turn into sequence.
	children := fields[5].unwrap_with_options('context_specific: 3; implicit; inner:16')!
	children_seq := children.into_object[asn1.Sequence]()!
	childset_fields := children_seq.fields() // []Element

	mut childset := []ChildInformation{}
	for item in childset_fields {
		// item is an Element
		obj := item.into_object[asn1.Set]()!
		i := ChildInformation.from_set(obj)!
		childset << i
	}

	pr := PersonnelRecord{
		name:           name
		title:          title
		number:         employe_number
		date_of_hire:   date_of_hire
		name_of_spouse: name_of_spouse
		children:       asn1.SequenceOf.from_list[ChildInformation](childset)!
	}
	return pr
}

// ChildInformation ::= SET {
//      name            Name,
//      dateOfBirth     [0] Date
// }
//
// Name ::= [APPLICATION 1] IMPLICIT SEQUENCE {
//      givenName       VisibleString,
//      initial         VisibleString,
//      familyName      VisibleString
// }
//
// EmployeeNumber ::= [APPLICATION 2] IMPLICIT INTEGER
// Date ::= [APPLICATION 3] IMPLICIT VisibleString -- YYYYMMDD

// ChildInformation ::= SET {
//      name            Name,
//      dateOfBirth     [0] Date
// }
struct ChildInformation {
	name          Name
	date_of_birth Date
}

// s should Set with series of Element with []ChildInformation within underlying fields.
fn ChildInformation.from_set(s asn1.Set) !ChildInformation {
	if s.fields().len != 2 {
		return error('Bad ChildInformation set')
	}
	fields := s.fields() // serialized name and dateOfBirth of ChildInformation
	name := fields[0].into_object[asn1.ApplicationElement]()!
	doh := fields[1].unwrap_with_options('context_specific: 0; explicit; inner:application,false,3')!
	date := doh.into_object[asn1.ApplicationElement]()!
	ch := ChildInformation{
		name:          Name(name)
		date_of_birth: Date(date)
	}
	return ch
}

fn (ci ChildInformation) tag() asn1.Tag {
	return asn1.default_set_tag
}

fn (ci ChildInformation) payload() ![]u8 {
	mut out := []u8{}
	out << asn1.encode(ci.name)!
	out << asn1.encode_with_options(ci.date_of_birth, 'context_specific: 0; explicit; inner:application,false,3')!

	return out
}

// EmployeeNumber ::= [APPLICATION 2] IMPLICIT INTEGER
type EmployeeNumber = asn1.ApplicationElement

fn EmployeeNumber.new(val asn1.Integer) !asn1.ApplicationElement {
	return asn1.ApplicationElement.from_element(val, 2, .implicit)!
}

// Date ::= [APPLICATION 3] IMPLICIT VisibleString -- YYYYMMDD
type Date = asn1.ApplicationElement

fn Date.new(val asn1.VisibleString) !asn1.ApplicationElement {
	return asn1.ApplicationElement.from_element(val, 3, .implicit)!
}

// Name ::= [APPLICATION 1] IMPLICIT SEQUENCE {
//      givenName       VisibleString,
//      initial         VisibleString,
//      familyName      VisibleString
// }
type Name = asn1.ApplicationElement

fn Name.new(el NameEntry) !asn1.ApplicationElement {
	return asn1.ApplicationElement.from_element(el, 1, .implicit)!
}

struct NameEntry {
	given_name  asn1.VisibleString
	initial     asn1.VisibleString
	family_name asn1.VisibleString
}

fn (n NameEntry) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (n NameEntry) payload() ![]u8 {
	mut out := []u8{}
	out << asn1.encode(n.given_name)!
	out << asn1.encode(n.initial)!
	out << asn1.encode(n.family_name)!

	return out
}

// The value of John Smith's personnel record is formally described below using ASN.1.
// { name {givenName "John",initial "P",familyName "Smith"},
//		title 	"Director",
//		number 	51,
//		dateOfHire "19710917",
//		nameOfSpouse {givenName "Mary",initial "T",familyName "Smith"},
//		children {
//			{ name {givenName "Ralph",initial "T",familyName "Smith"},
//			  dateOfBirth "19571111"
//			},
//			{ name {givenName "Susan",initial "B",familyName "Jones"},
//			  dateOfBirth "19590717"
//			}
//		}
//	}

// Representation of this record value
//
// 60 8185
//		61 10 	1A 04 'John'			// name
//				1A 01 'P'
//				1A 05 'Smith'
//		A0 0A	1A 08 'Director' 		// title
//		42 01	33						// number
//		A1 0A	43 08 '19710917'		// dateOfHire
//		A2 12	61 10 	1A 	04 'Mary' 	// nameOfSpouse
//						1A	01	'T'
//						1A	05	'Smith'
//		A3 42	31 1F	61	11	1A 05 'Ralph'	=> 52 61 6c 70 68 // children
//								1A 01 'T'  		=> 54
//								1A 05 'Smith'	=> 53 6d 69 74 68
//						A0	0A	43 08 '19571111' => 31 39 35 37 31 31 31 31
//				31 1F	61	11	1A 05 'Susan'	=> 53 75 73 61 6e
//								1A 01 'B'		=> 42
//								1A 05 'Jones'	=> 4a 6f 6e 65 73
//						A0	0A	43 08 '19590717' => 31 39 35 39 30 37 31 37

fn main() {
	// We detailed every pieces of element

	// PersonnelRecord.name
	pr_name := Name.new(NameEntry{
		given_name:  asn1.VisibleString.new('John')!
		initial:     asn1.VisibleString.new('P')!
		family_name: asn1.VisibleString.new('Smith')!
	})!
	//		61 10 	1A 04 'John'	=> 4a 6f 68 6e			// name
	//				1A 01 'P'	 	=> 50	
	//				1A 05 'Smith'	=> 53 6d 69 74 68
	pr_name_bytes := [u8(0x61), 0x10, 0x1A, 0x04, 0x4a, 0x6f, 0x68, 0x6e, 0x1A, 0x01, 0x50, 0x1A,
		0x05, 0x53, 0x6d, 0x69, 0x74, 0x68]

	assert asn1.encode(pr_name)! == pr_name_bytes

	// PersonnelRecord.title
	//		A0 0A	1A 08 'Director' => 44 69 72 65 63 74 6f 72	// title
	title := asn1.VisibleString.new('Director')!
	title_bytes := [u8(0xA0), 0x0A, 0x1A, 0x08, u8(0x44), 0x69, 0x72, 0x65, 0x63, 0x74, 0x6f, 0x72]
	assert asn1.encode_with_options(title, 'context_specific:0;explicit;inner:26')! == title_bytes

	// PersonnelRecord.EmployeeNumber
	//		42 01	33						// number
	emp_num := EmployeeNumber.new(asn1.Integer.from_int(51))!
	emp_bytes := [u8(0x42), 0x01, 0x33]
	assert asn1.encode(emp_num)! == emp_bytes

	// PersonnelRecord.dateOfHire
	//		A1 0A	43 08 '19710917'	=> 31 39 37 31 30 39 31 37	// dateOfHire
	// dateOfHire      [1] Date,
	doh := Date.new(asn1.VisibleString.new('19710917')!)!
	doh_bytes := [u8(0xA1), 0x0A, 0x43, 0x08, 0x31, 0x39, 0x37, 0x31, 0x30, 0x39, 0x31, 0x37]
	assert asn1.encode_with_options(doh, 'context_specific: 1; explicit; inner:application,false,3')! == doh_bytes

	// PersonnelRecord.nameOfSpouse
	//		A2 12	61 10 	1A 	04 'Mary'	=> 4d 61 72 79 	// nameOfSpouse
	//						1A	01	'T'		=> 54
	//						1A	05	'Smith'	=> 53 6d 69 74 68
	nosp := Name.new(NameEntry{
		given_name:  asn1.VisibleString.new('Mary')!
		initial:     asn1.VisibleString.new('T')!
		family_name: asn1.VisibleString.new('Smith')!
	})!
	nosp_bytes := [u8(0xA2), 0x12, 0x61, 0x10, 0x1A, 0x04, 0x4d, 0x61, 0x72, 0x79, 0x1A, 0x01,
		0x54, 0x1A, 0x05, 0x53, 0x6d, 0x69, 0x74, 0x68]
	//      nameOfSpouse    [2] Name,
	assert asn1.encode_with_options(nosp, 'context_specific: 2; explicit; inner:application,true,1')! == nosp_bytes

	// { name {givenName "Ralph",initial "T",familyName "Smith"},
	//			  dateOfBirth "19571111"
	//			},
	childinfo0 := ChildInformation{
		name:          Name.new(NameEntry{
			given_name:  asn1.VisibleString.new('Ralph')!
			initial:     asn1.VisibleString.new('T')!
			family_name: asn1.VisibleString.new('Smith')!
		})!
		date_of_birth: Date.new(asn1.VisibleString.new('19571111')!)!
	}
	// 31 1F	61	11	1A 05 'Ralph'	=> 52 61 6c 70 68
	//					1A 01 'T'  		=> 54
	//					1A 05 'Smith'	=> 53 6d 69 74 68
	//			A0	0A	43 08 '19571111' => 31 39 35 37 31 31 31 31
	ch0_bytes := [u8(0x31), 0x1F, 0x61, 0x11, 0x1A, 0x05, 0x52, 0x61, 0x6c, 0x70, 0x68, 0x1A, 0x01,
		0x54, 0x1A, 0x05, 0x53, 0x6d, 0x69, 0x74, 0x68, 0xA0, 0x0A, 0x43, 0x08, 0x31, 0x39, 0x35,
		0x37, 0x31, 0x31, 0x31, 0x31]
	assert asn1.encode(childinfo0)! == ch0_bytes

	childinfo1 := ChildInformation{
		name:          Name.new(NameEntry{
			given_name:  asn1.VisibleString.new('Susan')!
			initial:     asn1.VisibleString.new('B')!
			family_name: asn1.VisibleString.new('Jones')!
		})!
		date_of_birth: Date.new(asn1.VisibleString.new('19590717')!)!
	}
	//				31 1F	61	11	1A 05 'Susan'	=> 53 75 73 61 6e
	//								1A 01 'B'		=> 42
	//								1A 05 'Jones'	=> 4a 6f 6e 65 73
	//						A0	0A	43 08 '19590717' => 31 39 35 39 30 37 31 37
	ch1_bytes := [u8(0x31), 0x1F, 0x61, 0x11, 0x1A, 0x05, 0x53, 0x75, 0x73, 0x61, 0x6e, 0x1A, 0x01,
		0x42, 0x1A, 0x05, 0x4a, 0x6f, 0x6e, 0x65, 0x73, 0xA0, 0x0A, 0x43, 0x08, 0x31, 0x39, 0x35,
		0x39, 0x30, 0x37, 0x31, 0x37]
	assert asn1.encode(childinfo1)! == ch1_bytes

	// PersonnelRecord.children
	children := asn1.SequenceOf.from_list[ChildInformation]([childinfo0, childinfo1])!
	//		A3 42	31 1F	61	11	1A 05 'Ralph'	=> 52 61 6c 70 68 // children
	//								1A 01 'T'  		=> 54
	//								1A 05 'Smith'	=> 53 6d 69 74 68
	//						A0	0A	43 08 '19571111' => 31 39 35 37 31 31 31 31
	//				31 1F	61	11	1A 05 'Susan'	=> 53 75 73 61 6e
	//								1A 01 'B'		=> 42
	//								1A 05 'Jones'	=> 4a 6f 6e 65 73
	//						A0	0A	43 08 '19590717' => 31 39 35 39 30 37 31 37
	children_bytes := [u8(0xA3), 0x42, u8(0x31), 0x1F, 0x61, 0x11, 0x1A, 0x05, 0x52, 0x61, 0x6c,
		0x70, 0x68, 0x1A, 0x01, 0x54, 0x1A, 0x05, 0x53, 0x6d, 0x69, 0x74, 0x68, 0xA0, 0x0A, 0x43,
		0x08, 0x31, 0x39, 0x35, 0x37, 0x31, 0x31, 0x31, 0x31, u8(0x31), 0x1F, 0x61, 0x11, 0x1A,
		0x05, 0x53, 0x75, 0x73, 0x61, 0x6e, 0x1A, 0x01, 0x42, 0x1A, 0x05, 0x4a, 0x6f, 0x6e, 0x65,
		0x73, 0xA0, 0x0A, 0x43, 0x08, 0x31, 0x39, 0x35, 0x39, 0x30, 0x37, 0x31, 0x37]
	assert asn1.encode_with_options(children, 'context_specific: 3; implicit; inner:16')! == children_bytes

	// PersonnelRecord entries
	pr := PersonnelRecord{
		name:           pr_name
		title:          title
		number:         emp_num
		date_of_hire:   doh
		name_of_spouse: nosp
		children:       children
	}

	mut expected_record_bytes := [u8(0x60), 0x81, 0x85]
	expected_record_bytes << pr_name_bytes
	expected_record_bytes << title_bytes
	expected_record_bytes << emp_bytes
	expected_record_bytes << doh_bytes
	expected_record_bytes << nosp_bytes
	expected_record_bytes << children_bytes

	// PersonnelRecord ::= [APPLICATION 0] IMPLICIT SET {
	pr_record_output := asn1.encode_with_options(pr, 'application:0;implicit;inner:17')!
	assert pr_record_output == expected_record_bytes

	pr_record := PersonnelRecord.decode(pr_record_output)!
	pr_record_encoded_back := asn1.encode_with_options(pr_record, 'application:0;implicit;inner:17')!

	dump(pr_record_encoded_back == expected_record_bytes) // true
}
