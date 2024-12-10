// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA224, the test vectors come from:
//     SHA224Monte.rsp
import crypto.sha256
import encoding.hex

const seed = 'ed2b70d575d9d0b4196ae84a03eed940057ea89cdd729b95b7d4e6a5'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: 'cd94d7da13c030208b2d0d78fcfe9ea22fa8906df66aa9a1f42afa70'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: '555846e884633639565d5e0c01dd93ba58edb01ee18e68ccca28f7b8'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: '44d5f4a179b33231f24cc209ed2542ddb931391f2a2d604f80ed460b'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: '18678e3c151f05f92a89fc5b2ec56bfc6fafa66d73ffc1937fcab4d0'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'b285f829b0499ff45f8454eda2d4e0997b3f438c2728f1a25cfbb05a'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '206d442c6605be0e675b0efc76243c2f18f2260a93375fb36e469631'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: '1cd8ea34d8483b6a513c52a74e416bac2f322bbaeee02c6b0b05a781'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '00cee48001fe8442ef39c3433ed05473179f34205d337940d4bfd3cd'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: 'ead3ad27819401912bc9abfdb50037672a3aed0e94fbaa1cc0560621'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: '8f4dd5aef9cea829d8802ffcced2e8ed6b48ac23bbfbb0fae2fad0fd'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: '03aeb918feab459e39af29ff3aaf406088bf06d793338bbd563641a2'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: 'de80c312b153fbd7241c8bc432d1ed253d26dcc6f458b953ac2d9259'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: '3eb8d347cc2565ddd71f7fc21cff7eb3a2cf8e85c5e1d4c751f69f26'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: 'dbfd7033a4f884ecf7053f07b4c51f3efb1c09084cc7bbe667196a3e'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: '5323fc60310fe29900eb7a500f29897001c37945c5f8849674725553'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: 'c3d9416549bebebf679c0122a9c5bb86c0b514c6a4e9eda1e9782040'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '749fc9c8c21957ddcaf5eff69c297284d722c79be1fc6c910495a586'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: 'aa307d91c4037372ff0ca60eb17ec8f1faba862601b95754783ea808'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '071e361909c38791e941d995b0b25a3294bdf39456cc012806ada3c5'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: '18751a765f3b06fc2c9a1888d4bb78b2d2226799a54dba72b5429f25'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: '54b39c96f6377e3fc2ae0ba4ec89049a6c04808da3fa0415c9053ce4'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '58c1eda7eab2fc4046ae153ee95de5df036dbba25b9bb5c5428ea882'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: '3a02eaf55d04b6052b7d79b96d1e316f90f5dbbb3217dbfaea55faef'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: 'c8b5eac17f450458c60c075a8f75a24a1dbc58247fcd0ccfaf03e446'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: '53084cacfebbc4d1ff2db614b42714c18ddde36c6b7c2fbc3b1a8706'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: '528b867aa481d42fc4931a47d24c3c905aaafa8f6dd5820c67d3579c'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: 'f0a3cd3f53eb72df80ab67d264a973b6bb2f58bde8f636d9100e8864'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: '7912f20299d803ba917f408a5a59822d147bcd1008ad5c7b678e2390'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: '6f0e49505c15669302133d66e45d192e0c6ad02fc5b9aa128aa5517a'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: 'd06aed0f18e6c377174fd00cc74a92020b3df218c376eac0501a094a'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: 'cb1bbf7cc5dad591d32534c570e5bca93b8952832779dd6e0ccdc91c'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: '4775bc11834930118654a3e66e5b7f51871d6f5068f4305dc2845574'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: 'ce3b5703ed9f946ec4af62fade6e69c2751474ab8da570064ecd2ef6'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: '49a9e1aa84700874ac27eee43f193df69ed6718b131c4854f729a32e'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: 'd2c6592251a27cae7d819ac7b476c8a2ff608e57b018f79e0cf19b87'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: 'a861be4fe188858b913aad179ba575cec91bed54c1ca27608daa27dc'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: 'bf7975e63aa7f1bef84e7446f1c4a00a75c13285fd7c4a7a8318b1cf'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: '5d125b14e966c9e431bdc5592d3e6305fae678dc5d6dd064fa1345f9'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: '8c0fbb471404371145dbb66e8b1c6fc428e6dcfa263e3f5ddb47b30d'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '7148b69b04457296fca18e6f7b7f2145d9af9e6bc8f48b8571af0e53'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: '0bd47a04fc42fb3d9643496e5b0719c22262f741e862adfcef91d61c'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '3dbb14133351e5d4bc885d9594c07f7f0f99d4a7c745eff3e7aadc2c'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: '94f6ba7f94ba9903f7e9bde9b131353fce560c7de7159458f1a39cfa'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: '65fc03fabbf57904f572358c20f004aa16e0b5ae6530fa961ea10b9d'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: 'e46532e3e4bd0a0cb389abfba5d679e916a7c193324f1bac498ee1ee'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: '131d096c684345311cff958706eded139676a74d97a1beb861998485'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: 'fe3e03637088ac8ee2e035bfc1e7f4e944304663c832c26089e85f9f'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: 'd570c2b7040fc721b41f2d213f6ee87ac1e37f2b86526cf46c699aa7'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '82ede72ad163b914be7c22c085cd99438b6d5557ddd3b752f0a9fb7b'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: '343c21a0cbde3cccdbbd66eee32c50f5a54b0ac267ec3f41ec07a67f'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: '94ad254f3b4a76f6140d0dd3775bd75eb3c081085fcb76c91b4cca92'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '65fa84f358bc32caaff799129bc2cad883636826415703a2dd1a3cbe'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: '1c2f47c532856198d03dd85275357dce085c8f6c5a871aac4ff4ea28'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: '1d51c1019131b41a076cc603f4a8e56b2f4ee70dba326af30d926827'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: '9789daba3a8e9702d2d0b319878f88b08ebc5876dd5dff6414bf1922'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: '1968789785f1ef61f849bcb29fbc1491c006021f729718e72f29b80d'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: '62dca9550461f8a85e1abca4192a8a55a6e6663ebcda9ba6fb95f10c'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'ee190aa251c1a2ae0a376b4c6b6ab3bb09f743fa01eafaab68d170e3'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: '02273be94aaaf4a1d22496821e8abda8c418d3a4c278947c27d6c912'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: '3998a213e392978a38016545a59bd435180da66d2b3da373088f406a'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: '7308f2145d345bdb01c38a9993a0ec81ed5164ed0c6caabfa3b23fea'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: '3ccde61f4734978995b7489bad8c1e6bafe03f7c3886e9b4ef0f1aa0'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: 'cca9745f59f3ae2bbb8d65d31c171aa33960c8c6fa4689bb7e6d2152'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: 'c976de72db46c1a254293af6093c563ce43232077c249584c016ff6f'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: 'f1448af3cfe317aff1470f0a3de7bf533d77dc7f55e8dd790fd57727'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: 'd4be0ccfe4913851c9636ed036c625524e72891c5c0627aae50288ce'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: '6bd99c53693d4e2467ba6094710a6d2f48cc2ae907c4ae28604586a1'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '2eb4fa0872ede2a378386e40002cb00b4d1c2fca3413b944ed210915'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: '48b624151c9d3a1cc8e9d6665d42d4e640ac91abcd3556a31ff0250c'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: '31159840b7cb040d819ebee1ed0e52d09f5805be523cccc22eeacba9'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: 'f9a67791dcae0aea00f77f8536ddba439e9fcf7e5b1ed827f83818a8'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: 'a5913105fba645ba0df942da96d271a1d5efb923a4f61eb463450ea9'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: '6ece291f81eceaa9eb5a5e228c9924f165b8b10e2cf0e143dd5fe601'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: 'e07ab143f09eb8ad0d381b888adb22229c2e2a8b067e0fd012ef67a7'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: '88a33980be5bc911c1713d5c2bd2e5ecca7fc87879501aefa9722c89'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: 'a709d188da8ee187d91bd17069f785ebb379df013d78844a45b2bfe5'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: '306fa7bd696b3e9841f84d1c861712acba0febddd7a952499b96579e'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: '61341dee2e2869112bba2e1077fb409375f755dcafc1457bf49e0e8e'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: '0959a6e3b727c6213119b9e8411132b5819eb848bec6ebda0b75578a'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: '11cef0312aaedb9d0b26de64656406c8f4c358e6d3db459d364481de'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: '5de71b191eec70e591c22ebe3a5d2973aa3172f1c272e926cc0d4873'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: 'ab218bf4268aec9b41dd2db80622e4c0319cc0de12a60e06d80414ea'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: '5c83874afe6da0443abfbbbf8ceef38f9400b63593ee7a29d467b4f5'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: 'ba0fd01f699a0d00a0dff4c63f6ad19e9530a7ad11fec504e6481816'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: '835ec2c57424baaeed09a7c0c0b6e8bf9d1cec83de4c719846c990fb'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '64d95f2c92343d8fca6f6914fba8814478850b5d4c2eb227f4ae6fa1'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: 'f8dd5355827ac4bd040fc05ed6cb2914d013f126487a6d5f2c22f767'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: 'be5bbf68d6b99749edefa6b113638ca5cf5fddfd8fcd4d719aeb54b5'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '3434d03e98d0af69281e7a7ff8301369c5bc6166cd29b83397ad3fd7'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '747b2a7cfb8c4fef7de0a08499f8b19f37e9161b855a84bd50ef84c5'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: 'c1c1fa2bbb10c5672b040ed0c33d4d93e0fd210d1373fc7fd2312c0c'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: '671f67380b7676ee7c9fbfe71f3807e3575745ec3ae3128420a141fd'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: 'e88394adf710b9764a448abc6d62928b0268c6b119306f3c93d7b6d2'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: '711cc90bfdeed121bd5a8629a9cba6df7bf8df89184ec64ee918cc67'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '3f63432484eaa1f389d27947a84e256618f9bc81015993cac386887a'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: 'e00e0bf2a32227461230a065bbd2eeb5364277e83a850c53ef5c92e9'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: '1977311cea23a681c83dc58a6863e362bf6c02e30f4c9c4c8439ab4e'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: '2853a64f56c6282de53e30eba9418dd62eccb8c9a662c313b4768265'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: 'ca9d0a2eeb484b9809c3850f166362d893f951f5e93cc7a3c72522e0'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '27033d2d89329ba9d2a39c0292552a5f1f945c115d5abf2064e93754'
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

			md3 = sha256.sum224(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
