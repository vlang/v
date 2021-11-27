module bcrypt

import encoding.base64
import crypto.rand
import crypto.blowfish

pub const (
	min_cost              = 4
	max_cost              = 31
	default_cost          = 10
	solt_length           = 16
	max_crypted_hash_size = 23
	encoded_salt_size     = 22
	encoded_hash_size     = 31
	min_hash_size         = 59

	major_version         = '2'
	minor_version         = 'a'
)

pub struct Hashed {
mut:
	hash  []byte
	salt  []byte
	cost  int
	major string
	minor string
}

const magic_cipher_data = [byte(0x4f727068), 0x65616e42, 0x65686f6c, 0x64657253, 0x63727944,
	0x6f756274]

pub fn generate_from_password(password []byte, cost int) ?string {
	mut p := new_from_password(password, cost) or { return error('Error: $err') }

	return string(p.hash_byte())
}

pub fn compare_hash_and_password(password []byte, hashed_password []byte) ? {
	mut p := new_from_hash(hashed_password) or { return error('Error: $err') }
	p.salt << '=='.bytes()
	other_hash := bcrypt(password, p.cost, p.salt) or { return error('err') }

	mut other_p := Hashed{
		hash: other_hash
		salt: p.salt
		cost: p.cost
		major: p.major
		minor: p.minor
	}

	if string(p.hash_byte()) != string(other_p.hash_byte()) {
		return error('mismatched hash and password')
	}
}

pub fn generate_salt() string {
	randbytes := rand.read(bcrypt.solt_length) or { panic(err) }
	return randbytes.bytestr()
}

fn new_from_password(password []byte, cost int) ?&Hashed {
	mut cost_ := cost
	if cost < bcrypt.min_cost {
		cost_ = bcrypt.default_cost
	}
	mut p := Hashed{}
	p.major = bcrypt.major_version
	p.minor = bcrypt.minor_version

	if cost_ < bcrypt.min_cost || cost_ > bcrypt.max_cost {
		return error('invalid cost')
	}
	p.cost = cost_

	p.salt = base64.encode(generate_salt().bytes()).bytes()
	hash := bcrypt(password, p.cost, p.salt) or { return err }
	p.hash = hash
	return &p
}

fn new_from_hash(hashed_secret []byte) ?&Hashed {
	mut tmp := hashed_secret.clone()
	if tmp.len < bcrypt.min_hash_size {
		return error('hash to short')
	}

	mut p := Hashed{}
	mut n := p.decode_version(tmp) or { return err }
	tmp = tmp[n..]

	n = p.decode_cost(tmp) or { return err }
	tmp = tmp[n..]

	p.salt = tmp[..bcrypt.encoded_salt_size].clone()
	p.hash = tmp[bcrypt.encoded_salt_size..].clone()

	return &p
}

fn bcrypt(password []byte, cost int, salt []byte) ?[]byte {
	mut cipher_data := []byte{len: 72 - bcrypt.magic_cipher_data.len, init: 0}
	cipher_data << bcrypt.magic_cipher_data

	mut bf := expensive_blowfish_setup(password, u32(cost), salt) or { return err }

	for i := 0; i < 24; i += 8 {
		for j := 0; j < 64; j++ {
			bf.encrypt(mut cipher_data[i..i + 8], cipher_data[i..i + 8])
		}
	}

	hsh := base64.encode(cipher_data[..bcrypt.max_crypted_hash_size])
	return hsh.bytes()
}

fn expensive_blowfish_setup(key []byte, cost u32, salt []byte) ?&blowfish.Blowfish {
	csalt := base64.decode(salt.bytestr())

	mut bf := blowfish.new_salted_cipher(key, csalt) or { return err }

	mut i := u64(0)
	mut rounds := u64(0)
	rounds = 1 << cost
	for i = 0; i < rounds; i++ {
		blowfish.expand_key(key, mut bf)
		blowfish.expand_key(csalt, mut bf)
	}

	return &bf
}

fn (mut h Hashed) hash_byte() []byte {
	mut arr := []byte{len: 60, init: 0}
	arr[0] = '$'.bytes()[0]
	arr[1] = h.major.bytes()[0]
	mut n := 2
	if h.minor != '0' {
		arr[2] = h.minor.bytes()[0]
		n = 3
	}
	arr[n] = '$'.bytes()[0]
	n++
	copy(arr[n..], '${int(h.cost):02}'.bytes())
	n += 2
	arr[n] = '$'.bytes()[0]
	n++
	copy(arr[n..], h.salt)
	n += bcrypt.encoded_salt_size
	copy(arr[n..], h.hash)
	n += bcrypt.encoded_hash_size
	return arr[..n]
}

fn (mut h Hashed) decode_version(sbytes []byte) ?int {
	if sbytes[0] != '$'.bytes()[0] {
		return error("bcrypt hashes must start with '$'")
	}
	if sbytes[1] != bcrypt.major_version.bytes()[0] {
		return error('bcrypt algorithm version $bcrypt.major_version')
	}
	h.major = sbytes[1].ascii_str()
	mut n := 3
	if sbytes[2] != '$'.bytes()[0] {
		h.minor = sbytes[2].ascii_str()
		n++
	}
	return n
}

fn (mut h Hashed) decode_cost(sbytes []byte) ?int {
	cost := string(sbytes[0..2]).int()
	check_cost(cost) or { return err }
	h.cost = cost
	return 3
}

fn check_cost(cost int) ? {
	if cost < bcrypt.min_cost || cost > bcrypt.max_cost {
		return error('invalid cost')
	}
}
