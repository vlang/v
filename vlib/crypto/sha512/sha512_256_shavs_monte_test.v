// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA512_256, the test vectors come from:
//     SHA512_256Monte.rsp
import crypto.sha512
import encoding.hex

const seed = 'f41ece2613e4573915696b5adcd51ca328be3bf566a9ca99c9ceb0279c1cb0a7'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: 'b1d97a6536896aa01098fb2b9e15d8692621c84077051fc1f70a8a48baa6dfaf'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: 'a008d2c5adce31a95b30397ac691d8606c6769a47b801441ba3afb7f727c8a9c'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: '8eb896cb2b309db019121eb72564b89c1a59f74d4e2f2f6773c87b98c1997d77'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: 'ac71b694438cc300dde0f6f9f548d2304e2bdb6ea45e2b305af5fb3e4ec27761'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'd47cca4ae027778fc285bc78fb2a9c1cc7cde498267c35157e86b05fc58e698d'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '8bc1516ecea32fa3cd4f9f12ac13a2af6763090226e7c1484b2d85757fc9e82a'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: '1abe44379539744d7806f5d3ef7c2202d0b08f575d1899b7a3df737b18e2a2a6'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '324a7606ee9fc9cb26abd881581e70a06fedf33456f385b370973fcee8d01b7a'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: '06d1ccb978ca4b455ed380bea2e8c3afe24d0b935f5740677927f6f1a96fa111'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: 'cc78adb572c8cff2cc84020dfb61c521657baf3bcd8aea9593437cd2cdb94266'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: '935a43189f019f06a630f7601e1d40d51fc2c2ea602fea2c0da8404730c5a475'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: 'a99225bcdbbac4a87d8ed9e8e06889e823fc993a9661cedf1af02c6d38505989'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: 'e5e769874da74a143cd49f0a2f7a6cd3ad4c28f55d8719c85063dfdf0e1efbbf'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: '43f7ff7effc58dead5f13a47f8ea7e19f83dbcb409b10b3f985c78346817448f'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: 'c1acce072812dfc53682f0c4794b1cc4ae21fca882f4046e6d2c945f6d29dc24'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: 'a78313758117cf3319d90decbe3b46bf7ba20c079879f485b3290fbead662ac9'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '8eca6994ac6093c1f7701bec60770293d754e9eb95372f9ab10ca58fe8864bd0'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: '05457ef20a508c205225b9a836f11964febd60d9dd31943c49b91578741dd7fc'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '8ee3ca17f5ad34649a41b3cb6f4c60d24229d3c39b9afdce11725779296ce412'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: 'c9873b3341592ea56d0321a6653c350ebab65beb5e37f0a1554e9c07d93924a4'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: 'eba87758a57c5f9e3caa8da51b1acdbb2e69be17379f81715a134f5f09c4662f'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '8f70a95573734147b50866372b11673d97a8cd0754013f7f6fb35f96b2911353'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: '19ec8b72cf00b934d5bde820566eadc516e0038cb54bfc59349c68b36d079a62'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: '22f7a748d30425980a9f83298b0b053ff8ac156f304c87aa5d10a01e2e05c397'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: '3429d8880a8862eb6973b245c7d62f2f8078b7defe8eb117e8c787b30df520bf'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: '373806e57129e849aa91a208a1f855f9bedfcd9f85b259a70fe481a35f0266ad'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: '6fa0be65b46bfc853be64470cc9cfe46e2001d1345b3eb4ce3e2f881a2c7a56d'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: 'b32b578b078f6e3c8c48ca07de123fc7db6b6abe8ec09f6b91263e0353d2c584'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: '8cb0edd50a485250433188a66758fc8167e07d665aeb1553f53e08169d1842b5'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: '92150694ff1674bf073856b6df9a6cd015087e9bd366e7387e8c19084e0cc598'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: '3ec487fd88c8e1321e9279f8d5a82026efee762d686968cd2d801ab7f6b3d912'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: '894c78e468398414affcf1e9ab8fb14426e1fae964379615a212b744e61b12e9'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: '7f6a9d2c8d476ad79c845f786da4bd4a8de9a4af05ad0cb12af1375077714cb4'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: '092ae9d7a22cff2d6f28cb2ed47c082ef279fc20e2246b638c6c0c941238a075'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: '553d007076a97c6c6566600693cff33fcba2d021c1fb690427823d65076e4249'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: '17a8b074390ff2aca2b9cb7bfa17310a293f522b6bfe9b6b6f1429d709f8be7b'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: '6d903e776f8edadc19c680fab311dfec6aebf7333dd924dadc21fd0b624ea856'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: '1a340486e0703cfddf87dbedb73c93b499ed6018ed8a50ae148ec9cb0be1a608'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: 'd694d39fadf623aaf0144abd69d5cb580d050fc4efe49f98f0d910cba5ca3531'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '128d31156899fec03b0251cd739f2ee6b5041e9323a5aa31389f67ddf0c5a981'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: '1b3df866d64c5db4f55a4137e1d88da5b6e8a107a6fbe0022cde5e5c293a6a35'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '31feabe0b78b6a9ab56929570e48afc949e8150e5e1dd11ad7a8c6b43f1db943'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: 'c7c08d91944704e9de672577acc1a0cc832cc109e26742a2812822c55dd432ff'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: '76b41e08876b881dcf0007746a1a1e275e512b38c0f949cadd0521cc4bb43a2a'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: 'eb015a3eee8b130cba1f120e41b83eb032124d366f8a40eedb0569124b013c34'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: '537d4b84f4c9ce3c30723db6ea2dfc3a8df1d2a4fb090c651dee04ebc019145a'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: '9000799092b166e5042f996f9dc4273daee4c96b80716e321ea17f50e3e74e18'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '20c9c91bf7bfd79ed144cfbf5ea7226f5513ceed5944834c53326dad1a6a77a4'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '5d3b9b8125e1c55ee5dd70548d38ebf9bc706536b6db98f85c48b3072d27cb2d'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: '53e469865c36cddde9a53b29a3ed6b8bcc5e38f2c0f6dd3b356f1354be481a62'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: '4df8eb71a1f46ab35861dfcaccb723d372dfe58af9c036e5079f70661a419c85'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '503fd9149292f8e65f5a59db63dc8305e527f7e70e9cdc282b9d5075f23f92c9'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: '904c87a50b6383843b48cf9ce2a17c945b45c516fb0efa9eed88015fd10bb0dc'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: '888062fa782842258cadec93b1bd624bfe392500f5e6d6ef9e71b2963b73bb3a'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: '90c2a13524c81890be3f184cddb2feb44053de85335fb7933c6af8e3ee5d4ac4'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: '1efd7a3e6331c66b21c40416ce4f27d6aba8099359ad950b752e5491a156d9c8'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: '3f2a1f7bdda96ae3abe859e7767a0831ee69971b15d09d9385fac858643b0207'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'e021e5b9b812248cf5ea9563b84d753f5230cb1b442b04895871e3d8a7c6733e'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: 'a54312fe62b278b87bf440ff26703e280a13046071cfed2e23922e23ed7f8b17'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: '2b9906372367f7704556d4345602337c581917c3d91711466f4d27c8597c1628'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: 'd13e8e889226f72a1cfbb6d77a8cba9238900d148b4d0395e17415efc026a45f'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: '0977ad675ee02c46cc7523a4d617e5c8e69894efe0ad2d159b616eac65fdb68d'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: 'd34cd5081453b756bba9a455e0cabe97281fab2c6b5952a69171f8ed60bfb8d6'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: '720dc85f63580df35589802b1ee71cfa0522e92022382d875fd862bca3e64a19'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: '933991fa66485b36788447d2f0c8fd0f4d3d815f95a64fb197a1f66e7655ec3e'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: 'f80e89277c950a465d4b8a857efb1200dd7acf6b2dc4842bd6d3edeb1be232bd'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: 'c180da36f5e94ebc6a41747500728394cf2fbb06c003a14c9c977e20a6d98deb'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '2d3e75cc7f37a6dee320001adc7e0d8811b87f532e126cbd99f13874cd149a80'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: '3edd7afe868f3876cfce54aeb22fb55475aa4df87015c1defed2fb41f13ee786'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: '7548c1cc9b96a65a918b37b4817825f32841bbc575ea8e6d12fd9d0c66ce8cd6'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: 'a0ea397e82b8c7639d3594199c517b8f42f5c3dc40d104b51a42b46eb891d1ac'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '7fce4436abbf5da053cad07e8cf721c24442f926d4cc20cb8ac0a68b9d63b8b7'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: '48f93612a3689b8123fa234251e59a9784dc865dda43186e02a3fa10eac57732'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: 'f0c29f8e390435a2e233afa958ae208eb5d8eeb38e448df53a6d2e7a8e3a8266'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: 'fa43fb8ffa7d953a4da11627e11c51ba4f1cf0a62464b0e9d34cb9f1ca4a2339'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: '18a1c86b7c266b55b253f509e6457df10b48e6705bc0c78b638f9ee4eb937633'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: 'f5cd2b6b4a6bd0070942f81d012bdc85cf55c095075578a0404f2273908c20d2'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: 'b7825bba2320209852668a29f8b0eca4a3a0ad77e0e33cd2a7185761da5061b1'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: 'e7688883bbc0dbc767dd8d46ce58a0463f29d0781dfe1bde55a83ba9ca54b1b5'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: '54ba60a63f82f501435dd8a51d772404e5cfbddf226bcbc47836cf717fadb4e8'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: 'c533fff054d8dabfd37c4e860761d215954c7bf8f37ab6d6ef7ff9ec4713ea65'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: '71f36f26cb92c129f7450d76849a7f2f4bb0554e9ade0a9d5182f435541ca1fd'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: 'c39f5bfe6c241d0f64c13b05e5fb6725ffda3789ef9946c8af1dfcb424ba3d75'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: '27401de7cc0d86a8f07e276a9fbf4fe5cdf7b3d73802a5c7edd27b686f6f7039'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: 'bc56733b8682bab456b8c0819e1c9d6d3bb864b2427ec2a2003a871c44dc0246'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: 'e3843383a52a8c3d6315d255d1708467f5de61a5372308978b295460220d99aa'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: 'e03de5004325a1dc96eb709c849bb6ecadc5db9c6152aba480ca10c8fe2573e1'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: 'df09f2a9bceab29b5c149ec7db23b1c8b1cc849ff2612c4ad9d66e98cc1f1faa'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '123426906e20a02fd517c4cee4e5f5273f894c56ae456ca466c7a0a2d1b0676c'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '14a9e07e6280a8c7cf824cd1896ad974c310859eb1bb4bef59b715b25cfaaa30'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: 'ae7febea114679a52c9f7351aa0a207dc40ba8c8394585cc5a0b9bfc7f67d76a'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: '4966b9ba04ef0590cf0f335798c9b3e15ad32d5ebd2719a0c914c8faeed65326'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: '8ea5a43cdaf50149c3aef14cb2ff2db42279ca36267355a0e023f3ec2ea13375'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: '14ca6c157502aa86c4900086c373c5d2b8789e5cc9863a384938e498fdaa52e0'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '8471a7d5a2cdc61a8bc885c1b0b79491a54d6610ab19a3cffbe0c83479628cb3'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: '51749feaa3f2ea9c89b38d77814ea2796a3265e4f6ee7a2c08a1804c6fbfe042'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: '8163861f1ca72bbc19cb8ce59bcee8a3d6d28c8ce17e33cea67f04277a68360a'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: '6c00f3fae370e7d14504c48cd7199064b3b4828163905af7b7a0593d2b5d06fd'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: '5fe007f269a854e2394874f1a95ef7ef3844cf06248deda3af561cfacaed1e35'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '1822ede971ca1407fbcb1dff487ea9d93d71d756cdf32945703de673f4b3d816'
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

			md3 = sha512.sum512_256(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
