import crypto.sha1

fn test_hash_crc32() {	 
	assert sha1.sum('This is a sha1 hash.'.bytes()).hex() == '6FF5FA4D5166D5C2576FE56ED1EC2D5AB0FDF936'
}