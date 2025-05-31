// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA512_224, the test vectors come from:
//     SHA512_224Monte.rsp
import crypto.sha512
import encoding.hex

const seed = '2e325bf8c98c0be54493d04c329e706343aebe4968fdd33b37da9c0a'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: '9ee006873962aa0842d636c759646a4ef4b65bcbebcc35430b20f7f4'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: '87726eda4570734b396f4c253146ecb9770b8591739240f02a4f2a02'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: '7be0871653db5fa514b4ec1a0363df004657155575b0383bc9fdec35'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: '7a794a3a1ae255e67ffbf688a05b6aba7f231cebec64b4fc75092d49'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'aaf5d4ecaf9426149821b15821b41c49e3900c0fc91664fb294216ea'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '60f6ff2fbffc9151f67c7e9325e27706f9904d1d55311bb587c183c7'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: 'c6e46e63fd0b37a7fd460f724315796a6be5dcfd047907acc0512278'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '1c9dd92b35f00ea3cd13bcdef3c99e73fd604fe167d4a23ba1aec8eb'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: '3ebec6bbf913b0eee0cc8aec7f9611648432674551fc3f1e01520dd8'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: 'b09556d256b3de7b122096a7a2a8c1d2ffcb23a9bfc38bb1d919739a'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: '756dd96518eff331dc48e2af06c6f2a92831d49bef28af687d0c3912'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: '08ef4f1cfe5b707ff57218240f5c02e75cc875e6585fc17fd60d3af7'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: '6f956e415d7c29d8073d6e5f89d9288215988db029b86183af622de5'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: 'a39947509d4a45d39837f785b2205a2fb4ab184fc40c88ec8dd7fe7b'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: 'c7b53227a32fc6e03f300ec56bc4505f03bad3d66a39940e1670b62f'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: 'b61a60ea4173d714cbcccff0dfef932f9d719175d5747ebca2b55b09'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: 'ee2c5fc1649e4033884ddbf3c2d93fe3ec9e8e8171df0cd94b438613'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: 'a333fbe4ea8dc682f8dc5e96cc2365ec2510b96d8e6bffdec1324b41'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '5ae36caae04f3426a6b8da72d16b261b79ddef7e94a5ac2c601a40cd'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: '0d13a738747e8a15dc02e26338ad47a556afb4af033874de50e31a39'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: '4fd90174f249e11b99e7eb63ebca5e06f8f1f78efee855b71545ef5d'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '02a47cd22538f44db8e2b0f769cf8fe2c0528dac2f47e53740fa7384'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: '75ceb64019f21eecef34741b96e2dd57eb261b24d65fcd1a409210ce'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: '4ffe012bc77387dd23c521ece6686e67c8584f308f5f62adc46715c8'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: 'c1b12961b27395bd2d309b5e6a20451411bd3986d992bb83b30b92d5'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: '0547c463bf3aeac722005c48d45e008f09c33551f1cfdd85b8d67dec'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: '8704119b4a6f7fd8ac25bc1ad72c1d573106a0af5dad3c8b7563c742'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: '877f5024aa6fa131e4660a8c7dd19bc80ce77c784b01ad79a5528fbb'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: '15750a559a4aed2925d280671b75d34e3a5b080259143f7b671e0e5e'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: 'deb48a5c402f539d0a0a62d81928debf78519cbeaebce1a29f203b06'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: '19af4d392fecc6a03f443f8aeb8e59a5496ca3b75481771b3efe45f9'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: 'd7e0d727fea61dae8f2c92fe884f2cd939ab2737f6bbf4545aa27e67'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: 'c8b6e303d2c654a5f330209502ac89adf9840a6c832a356095c98b70'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: 'bbc6d0112d60da2975d2028cf3c6b9509aed404f378540235199456c'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: 'b8bec3ad04591295081ef9484df499d4659ee1cfbd74b11033fa3d27'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: 'e6f5a285cdc65b24496c1b5040fd354b7abbc930128e6c750fe72ef8'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: 'af77e2ccd33403b22954796a071da760f7c27fd998e9aaa38126172a'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: 'de0e347e717e3331a73fc27c4d0a6f4adae82de2c70c701a6d5ab2b3'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: '00397993b6b0ab272be84263167167270c45df00483b01fe9b1ea365'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '9a95e0f3e52e822552ff16c701d3541674de4cf183b6f30c43154bcd'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: '6a9d0c77cc592b625be5896b41e5dfc10a28ebbfdf688c84b525e0ca'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '39b8102119ff29ab458d92c691bad39cad3e3a9bd08a6d051b9e8603'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: '49aad2c03b7c4d3dcf8356325fe6c43705731341697290c824f9eacf'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: 'fe4e83590811ce54caa352e52829268532a20b1a38d069ad99fa9a41'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: 'eeba13d3177da1b2b6877cdbb8ba32e0886b94bae893a9a062af4d4d'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: 'edd2d2365300c6a904526ad7106ee74d75bcbb9634c97f3fec57fc48'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: '46bd3b5289ff2278b2d1d652c9294f31415dda38694fce83fdd25f15'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '1e1059e7089fa33e23fc1572b08131c7600ac8a1bbe583b6a4c6e09e'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: 'cd70a5e3d6ba8281c5ecc468dd7e96bb3d7723754d2cdc19e8a6063d'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: 'cb46f0380d1db1b1bec9b70d51c29d1300c5c814a4b360a0db548363'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: '76b467577856973eab224e2ecd0689601446c7915f6805e93276a4bc'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '3f4d4db5e47d20c7458a87c5029eb340c4b639ebd3b6f1fca9c20427'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: 'f8fff1c83b456dd8ce96aa977a7a1b968c1a5bd4f2cb6e0ff80aa923'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: 'f88800640deb5328d0a6985474a041be43c7248609fa97ea0b9236cb'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: 'a071d9e4da86be8c17bbbbcf47a863ceea303bf02bb872565f829bdf'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: '19e399fc939c03fd3d016f69f77345580c1f8869b42c8231aea37027'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: '87f1884b3f78b9c962beef4e024e235fc9ca25c8667426fdcf18ee34'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: '6172fb88775686115c5d62e7b32affb353b40a41fe1ebfc603917274'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: 'df9e73edd801f9f457192f7d3aae4261dd89515200b028b27684d554'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: '3c5c668e1b085b7efb28c5413657b921c7db8cec6eb91177bc5fdb25'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: '2273fe0a482b701a54dc397ea93bbf73f7a8ce0fd891cf4bd7d0b9e0'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: 'bdfd8d50df97a02af4ba997e237f99d17503321f67cc5af8c3aadf2f'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: '24f6e6a0d54767dc5b33a46ca073d368cb3ec1b36fecc2672838a194'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: 'ea7672b36987befb16352c13231fb7668c75673a932bf880181765e4'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: 'e45da9a4f5c0dc38a2f7d2a30b0c8fe153584a56d80ce69e929a2784'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: '86983ba278254438244b1bf5ea59492ec36504a078530b6a7812f832'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: 'ad9c1161761135fff88c17963e86cdbc74ff1b1cfc4433fbc8d9efc1'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '9af064cf4fa078b50835f283cee8ecb6ada36012ece1a8426a62342c'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: '046810fe8fb6826cc5b6b15eaa5e52f6b3679359ccdd773d6d3b638c'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: 'a5ecb35e74f484c8dfbf759ab4c0ab432aa2a9d7d75fdce80e556fb0'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: '5e1de1624a8111e59cb1955a4c2c4a2b54aa4df035679c0bdbaafc2d'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '9b449c13d9633869261dfa842c63c82ee9ac71b392b1ab268d536917'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: '502270faa8397007684f76f331365a793e7a6cc8298dd9798229a515'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: '7e1fb43c7ffd08d08ca9a0cc5d5414f8e2b20cc3458a63ac00ab388d'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: 'b8694aa799471f785e33c6f8d5729f6c0db59f46d652cbacc923dd70'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: '6b431252c1f23628d7ec11656d52e8d6724100a6204e136bfc7ccd92'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: '357f7779dc289dd46231a46bdeabaf899b167984864d232410736505'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: '28cd0f7181f891e138c3cc43f14b7e826eff9dea465cdfe948d31d88'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: 'd3eefb1a85d7ade8278706aec0c0c2b889004ca386278fe466605d2d'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: 'bbcf4c4b31af3a6ea82d23907fe71bdaf5ae4db1446fe28fabf01145'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: '1f8354c43cb18715125c504ea1bff73e4e95c64fbc19e850468924a3'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: 'd3a57114d5572427096478d26162d7cebbbe6b99cb06234327b21ef9'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: '45b964bcdbde997f8b8de4a7617152a2f80b0333aeac6aa3e53901fd'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: '2fad09cb0e93263548120115972110979eec7ef94e303c462223005d'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: '6680932f0332b4c0ccb708d4bf351af23bb05b6fb8c36b458efa46f3'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '1076a4637cc36c9f5c2fff3700362a9d62cda3596be7b5e7f244fff1'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: '0a819369c413d8d20195fa16660b320988357d63c1a94602eb117377'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: '397fcc30577bc5327a8cf9284cc64a02700f1b2f2d1ca8a1506b9916'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '217be1548b8d99bb1080ea0f3995be3c6494c91235fbf3a6d854a08e'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: 'ddcc841fd03e9d942f886657b0feb522682979e836307f68f4f19843'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: '4a33efa590ee444c75b6a4530a0fedd9987661199ccded49e40bc8b0'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: '970a4a3334796b06cb0ba8014eca67bbc471dbcea0efbcb22c666b2e'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: 'a7630df9688bbc66ceac6ae4a3a549513e427f5dc0f55ad2696b021a'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: 'd63c202d5feab56898d8eb793115a8ac216ea1b163a967eb119367fc'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '0dec4d35fa7737c2d93d901a0556ce3cb357224d3caa25dec8095a05'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: 'c40b609646eef457dea98eb32f45ee1f0af31658d742680bdb784f53'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: '1b2a12a3bc4403a1bebfc1358cb2844c56215ae413ed6df10fe2f8d3'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: '8342cbc0b0e8c270134a7907448037e07201150f0891b20dcf20867c'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: 'a09ed66100b982070edecb5af45ac354759778134098ca3c0bf67b05'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '5d8fc89761e82efe7188596a52eb43efd9492038bbc47bc0df5e9843'
	},
]

fn test_monte_messages() {
	mut msg_seed := hex.decode(seed)!

	for c in monte_cases {
		mut md0 := msg_seed.clone()
		mut md1 := msg_seed.clone()
		mut md2 := msg_seed.clone()
		mut md3 := msg_seed.clone()

		mut mi := []u8{len: 0, cap: 64}

		for _ in 0 .. 1000 {
			mi.clear()
			mi << md0
			mi << md1
			mi << md2

			md3 = sha512.sum512_224(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
