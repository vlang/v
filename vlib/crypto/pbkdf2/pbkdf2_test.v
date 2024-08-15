
import crypto.sha512
import crypto.sha256
import vlib.crypto.pbkdf2

const data = 'test'
const password = '123456'


fn test_sha512() {
	// vfmt off
	assert pbkdf2.key(data.bytes(), password.bytes(), 1000, 64, sha512.new())! == [u8(149) 155 168 16 77 243 26 192 128 222 29 139 38 173 131 82 73 152 197 253 66 64 11 103 32 110 95 116 143 4 104 70 176 24 99 48 224 77 47 184 193 59 98 191 18 172 4 119 83 93 198 101 118 131 223 150 215 172 170 166 205 187 247 160]
}


fn test_sha256() {
	// vfmt off
	assert pbkdf2.key(data.bytes(), password.bytes(), 1000, 32, sha256.new())! == [u8(110) 95 68 212 254 34 114 21 43 19 155 141 36 158 236 51 16 244 85 107 245 172 219 25 128 109 111 18 25 14 9 149]
}
