// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
import net.openssl
import crypto.ecdsa

fn test_openssl_and_crypto_ecdsa_are_compatible() {
	pbkey, pvkey := ecdsa.generate_key()!
	message_tobe_signed := 'Hello ecdsa'.bytes()
	signature := pvkey.sign(message_tobe_signed)!
	verified := pbkey.verify(message_tobe_signed, signature)!
	assert verified
	pbkey.free()
	pvkey.free()
	c := openssl.SSLConn{}
	assert c.str().contains('in_memory_verification: false')
}
