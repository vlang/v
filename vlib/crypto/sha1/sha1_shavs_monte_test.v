import crypto.sha1
import encoding.hex

const seed = 'dd4df644eaf3d85bace2b21accaa22b28821f5cd'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: '11f5c38b4479d4ad55cb69fadf62de0b036d5163'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: '5c26de848c21586bec36995809cb02d3677423d9'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: '453b5fcf263d01c891d7897d4013990f7c1fb0ab'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: '36d0273ae363f992bbc313aa4ff602e95c207be3'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'd1c65e9ac55727fbf30eaf5f00cc22b9bab81a2c'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '2c477cd77e5749da7fc4e5ca7eed77166e8ceae6'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: '60b11211137f46863501a32a435976eabd4532f3'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '0894f4f012a1e5344044e0ecfa6f078382064602'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: '06b6222855cae9bed77e9e3050d164a98286ea5f'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: 'e2872694d3d23a68a24419c35bd9ac9006248a8f'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: 'ea43595eb1cff3a7e045c5868d0775b4409b14a3'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: '05a9e94fdc792a61aa60bcd37592acee1f983280'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: '7d11aa9413cd89a387a5c0f9aa5ce541be2aa6e8'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: '37297d053aaa4a845cc9ce0c0165644ab8d0e00b'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: 'd9dcde396d69748c1fe357f8b662a27ce89082c8'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: '737a484499b6858b14e656c328979e8aa56b0a43'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '4e9c8b3bce910432ac2ad17d51e6b9ec4f92c1ad'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: '62325b9a7cebcc6da3bfe781d84eb53a6eb7b019'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '4710670e071609d470f7d628d8ea978dfb9234ac'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: '23baee80eee052f3263ac26dd12ea6504a5bd234'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: '9451efb9c9586a403747acfa3ec74d359bb9d7ff'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '37e9d7c81b79f090c8e05848050936c64a1bd662'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: 'a6489ff37141f7a86dd978f685fdd4789d1993dc'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: '39650d32501dfcee212d0de10af9db47e4e5af65'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: 'cd4ea3474e046b281da5a4bf69fd873ef8d568d6'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: '0d7b518c07c6da877eee35301a99c7563f1840df'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: '68a70ae466532f7f61af138889c0d3f9670f3590'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: 'c0222aae5fd2b9eff143ac93c4493abe5c8806af'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: 'd2efc5aa0b29db15f3e5de82aaa0a8ce888ffb2f'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: 'eec4f55d02c627dcee36b5b5606603bdc9a94a26'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: '0e706fb1a1fa26aab74efcef57ab6a49c07ca7bd'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: '2ea392ca8043686424f7e9500edfb9e9297943f7'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: '74737ef257b32a4cb9428c866b65bee62ccbe653'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: 'df3e86e49a0429fa81f553b04b9fc003510e9a51'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: '79c3049944fbf8b80dadadc7f5174e5cfdf996de'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: 'f25e2eca4cfb6da8e8b7b62f581672fab80754fa'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: '76509239d9fd6c6f050c0d9b3777b5645e4d4c70'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: 'cf4bb3e1f330c862e239d9b010bd842f302bd227'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: '4eeac7ab2ac9e4c81ed1a93a300b2af75beddb08'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '46443ba72a64fff4b5252fbac9ef93c2949f8585'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: '5e9c42482343a54aadb11ab00c2e00cbe25ec91a'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '93acee1977128f2a4218678b32e2844f23eb526b'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: '226065d299b2d6c582d386897b93f2adf14de00b'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: '672fed0d90c21d4ec0111a7284bcf1bbd72af9bd'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: '90d642f12f28cb3dad7daad84cf0f94ded1137ae'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: '4a2815b58ffc858e5e7e9e6106765458d2af4ec3'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: '29fa3679032421b78b7a08c54766c1592f6739c1'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '19f4e30393eb66c6e200744fa8999d224e6df173'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '30650026be77212088ab50438e04b4b8e3761977'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: '993d0e135bcd598fa673c6f19251bcbde18b7b34'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: 'c9eaf20b473219a70efe85940620426c6ff6f4a4'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '6325d0b83c308bd42854ce69446e85ba36348d7d'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: '2fb354f8a68030efb747f78812060a9c05e92164'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: 'a7e33bd16f770c17e8818ad5a5fc4fee673eae56'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: 'ff23e7105bc9f4dad0fb9c6519d1eae16439a5d6'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: 'a31aca821e163213cd2ae84cf56c1134daa4a621'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: '94ab9cfd4cf9bf2e4409dbcdc9ef2c8b611cc69d'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'c0194064ce48dde771b7871efa86a4a6e87eec76'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: 'f1a9065e3e7f98753c6f833f5ffe74133f6b887f'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: 'b8b3cd6ca1d5b5610e43212f8df75211aaddcf96'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: '33c3a8d739cc2f83be597aa11c43e2ad6f0d2436'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: '4f5c67e5110f3663b7aa88759dbba6fa82f2d705'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: 'b1ebc87c7b2b8fe73e7a882d3f4f0492946e0d7c'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: '01566616fe4a8c7cf22f21031ac6ea7fb7ce15db'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: '5650f3517a393792781d23b4c9d360bf8bd31d65'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: 'a4fdbd24cb4a328b898b804b103caa98baedd3fa'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: '0cf01eecec4b85aa39f40aa9b4dce208d68eb17b'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: 'ae9ac147bab7c10609abe6e931a5ab087a41dc5a'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: 'c0328145ce63fb0aceeb414e791d2be92009b1ec'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: '60343e5fb7eee00d31ea507b820ddbb7ef405dc7'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: 'e0b97cd9149ff9955b6a35b3a79ecb3bdbd2a5a5'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '4e4fdcd382ae0f3f4fbda5fd934eee0d6ad37df5'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: '9d97dd237d193482cf3ab862a38843762e69077f'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: '2bc927d17ff2f8a844f6f36a944a64d73d431192'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: 'b91200306b769aab18e5e411b5bd5e7bce1cc80e'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: 'c47493a666085e1b7a75618761a80c402f46546d'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: '31355869b80ff84fac239db694ada07d3be26b15'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: '1a2022f6330bf96f025cb7d8f0201a7d70b3b58e'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: '0f60d7c5ad49efce939c3a27da9973f7f1747848'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: 'ceada087801616fc6c08cfa469658f3dc5239ca7'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: '4ad0cf9181122b06df714397bd5366aa90bfc9fa'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: 'ac6404e6b9d5c0fa17fa77fd39850f22b76ecd83'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: 'f0658218adffb9ee9328577854b6387393957a3a'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: '6fe9992747897389957b9a91467a4ec983829ab6'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: '74320b3ddde6dbfbdad3ad29a7695f5a275b2105'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '2009ea5d6452f51d12477740e374e0e313134779'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: '7dbf33d7125709f101fea4ec03436ab95a900c28'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: '0c05b78e324cb265bd6adc7452249eaa85bccb3f'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '10c1b9b2de8a9050fb6f4b10a99f7e1e47159f25'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '20072c1f691142d9b83a090dd01f446b4e325a1c'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: 'ffcb6a1525f20803cfc79deb40addfd3e7b2f05c'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: 'bdcbb4ed636e244bb0fe6af4bc53998936df4ebc'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: 'f58ccbc65a2ffa5b35274dd0ceb4ea70eb73c26a'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: 'fbe95ac75e4b9cccd1a5debf757fa1a502d07944'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: 'a8babac55950dba4993601d35adff874a2b9bb2a'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: '594db79de71c7651e9eef2f08bb7be3d26b6ee99'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: '63377d45d0e2d0c987bebe8086c76a5e8b63a14b'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: 'cd1e7a192130866aa87fd1c8b43e9b7a0eab7615'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: 'b3c69ad5dbdd34b7b45b2a89dad72f4cf1d8fd73'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '01b7be5b70ef64843a03fdbb3b247a6278d2cbe1'
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

			md3 = sha1.sum(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
