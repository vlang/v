import crypto.sha1

fn test_hash_crc32() {	 
	assert sha1.hash.hex([]byte('This is a sha1 hash.')) == '6FF5FA4D5166D5C2576FE56ED1EC2D5AB0FDF936'
}