// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA256, the test vectors come from:
//     SHA256Monte.rsp
import crypto.sha256
import encoding.hex

const seed = '6d1e72ad03ddeb5de891e572e2396f8da015d899ef0e79503152d6010a3fe691'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: 'e93c330ae5447738c8aa85d71a6c80f2a58381d05872d26bdd39f1fcd4f2b788'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: '2e78f8c8772ea7c9331d41ed3f9cdf27d8f514a99342ee766ee3b8b0d0b121c0'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: 'd6a23dff1b7f2eddc1a212f8a218397523a799b07386a30692fd6fe9d2bf0944'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: 'fb0099a964fad5a88cf12952f2991ce256a4ac3049f3d389c3b9e6c00e585db4'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'f9eba2a4cf6263826beaf6150057849eb975a9513c0b76ecad0f1c19ebbad89b'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '3ddf05ba8dfec982451a3e9a97695ea9cdb7098c877d0c2cd2c64e58a87754d9'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: '2cc3fe501e3b2e33e60407b0a27025735dd04fd7623bb4fceeebae5cad67ad4b'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: 'c534802a459b40c792e1fa68e54ceab69e333fbeeecad65fb124d2f3cc1f1fc1'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: '8986e95d85e64822287c78cb7a714339431332182107109d57827776c6cc930e'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: '72361401c670d07f1151a95e2ee914665c2bdb1228581833c7dc53b89c01c927'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: '124c443bad9d955e084a3961b079c43c59b5e0d666af38f2f37846e85369a618'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: '81914b78674a2a6204eef78ff51369526bf0c2e121cd364eb40a8435479dda14'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: '8eac9d963b44021b70a527ea07420b03f51a998d0d6cb73ad4cb7fc688b4d174'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: '0427263b4dd3ebfcb7871939dbaca5ca94e794f748c02920c9759dfa554ea534'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: '3e9d754f2ec273b0056c2fcad2e891aaf9616fe74005d36cbf5ccba2e037b5b3'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: '986b6594ed96a819e49edb9f65db2ea52168973d7e18ae9e0b8869a8b5dd29a0'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '117578126a35176a00f8c0cf999442df0890737be1880f06e6a7270959c114c6'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: 'fd7f5574788d8ef64b83333ffb62e4cd3311e638db0c514071c19b84e9117afe'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '19db7ba6e3488a9e935af33ffb912d60c9d3b98a0be1d78e0b374dcb5274a7fb'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: '52519e6319505df7a9aa83778618ec10b78c5771bac50e8d3f59bc815dabfb1f'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: '434d7795fc7510af04b613e120f7f48e6d613ec056ae9fbc7c869b87c1dce63e'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '020324de7f6763be57bc4a6a0960258ea401ffe40d68f854e82ccfa9e0612ff7'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: 'b87c7fd0ec4cd35fab077b64d00917ad06aaccb095bbe4603466644ce6cbce18'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: '01abbd12b2b476b2d540d0c47edcb56263ea658a8080a8f08dbb313942562f00'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: 'ce95bb2bf2d5c91402e13ed5271615607f39e0678aae776d18a78351b90b5838'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: 'b81af264b0bb485f6656be91478f7b96c324fe262fcc366d9ce3edd44ccb85d0'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: '9e2ad901200ca524c91373f7b5eda9cda142353e763862e350314f793a0b700d'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: 'dbfabc7124338d6845f083cb1bbdf7b4060274d8e0e98d08bb7ca3779059b45b'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: 'd93c2cd61f5476ea08d85f741720ab2ce5c4e38cd8254758238155fd68ea7723'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: '232d9c3b583e297439c859150738e1b1d530812d63a9a2c1cb8e40cb50a2f27b'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: '8b9c858bd135138d9023a0b5fcf3f12ebbc3b7f721ee0b44be1871187f21f506'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: '05cedbd568ce9adcf5022999b8f3a28995a910c572375186da5febd775d62b79'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: '24282cba8f5dfce7e423a103488a9a924080d549853c699159d27816dbdbe5d9'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: 'ba6e3c38128f93f288e781af8a13e7ce5120c2a43a6d1c0d4edc831247350079'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: '706fffec5b69f5ef5465b6a8663c302143af743c6b7cd5fec9f3fa9bf9b2e285'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: '6d32c55c005eea65dacdf0e90f436943d0d0acec3c2355c36e2df1a86d1a11a7'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: 'b353f425293db464ad814177ea9689f43054bcdbaf75675e918b78a82ca97a50'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: 'c3fa9993130b3c95d9aed30243ba902035933d18adf5e21d2567674769062e81'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: '1e77e07988ebd618740c2f89a7bcf0ae2542279ea8895b39aa70ba8bc37ee00f'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '063927892a0b095be7d21987ff8157cd4c674c1cd01ab9f0834824e8efbcf938'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: 'f43054c280f05371cfbac776d43d6001f71350d898677f035aa8f7e5bd7b3fa3'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '2427934b28c7a9c2b18a5b7e996351aa567523744f60d54dc35bbb61f56f6fd4'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: '3633976d174279161e13b49e5866c144ce8c1d17ec1901ad56a02c900273fe11'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: '5f9788660d82c80155a7fea91896be3be2eb6a7b2ce963f3804cd09da5ac0c8f'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: '097ef57de6df98c29346e67e7f676569ad402f7a1c88d1cf39ce2d44fd706f72'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: 'fedcc810c74706a27fc0b6663ab2f9de0761089682dff1279fcd91312af1b8e3'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: 'bd5d61fea8d23089f3f30266b1daa636a352e49476526e71cc0735cbd17054fe'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '5ead027c03d7a55c17f0c783b6d77670cdb8942772077d09dff9a46ecd527bec'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '7a06eeea07ca9eb94a98a5e9f00b7efd8de9843b6aa888822c3dccf803637732'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: '44b6a895058ed3f31a5549407af8f788631f8a6eb8c0a5f2e15facc9190b5672'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: 'f8a58bff4b54aaebe18fc3f0bb1d24974a125530756dd4a0f15628c35c02ea1c'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '3bf2ae5408399aba59f42e5bed35a00d038fada16013ffa5da9e8b7207f6012c'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: '31d33c0275986b06f6dccf570d1064c7b36e1574cc4371d4bba2e55321d75397'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: 'bda59cbd65e87a57df3f03c89e4d9511de71da05e2eee0560948696b37615f8f'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: 'f431cc1817569e92c8ba11ec4741e6dd2e361156575af7b482587ed78e9fb7fe'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: '1b3b3789a32165f725167da6f5ef89d95de5992783961440fce67b66c3351ea6'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: 'c9873a09c079ca7f477b5601519ce51896c2a35a28fe05fe8b13e990813c6634'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'fb16cc865ddcf513be298c7d514033ab3fae7a80b285d2b43e82363342e498f4'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: 'ebaebc261b327f8be24026e32099a6b15927c54dbe390b72756f3f6362ea3b3a'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: 'ae5a4fdc779d808ba898966c8c14a6c9894107ef3e1d680f6ae37e95cb7e1b67'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: '5a4a67451c197b038c540878b6e7bc6fce3eea9c95795d611359703d6cc7ca02'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: 'efb075aa051070a6b2303e026f81a5262a6e64eabb270ec5e13fc6efa3529f6f'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: '8ff3df1a5cd0840bce61520f1e5645ce272a37b884c1750c69a957134c1a20d2'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: '8fbd86567c20dc3ea9948dd5ea6f5204028c4ba258c35052994e7c86de2d7701'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: '670559572a74e9af0513a3f9243bfbfd5805b837705faedc3c480d67a92bc124'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: 'ef2ad8656fac9c593d301fcfac77a7815d50b42526d3a44e1573316a25b05904'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: 'a3484a7a6cb5c941e15346a3ac4e09e99a5189cc96a87104d196af3c43cf995e'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '966851a0ef41f8d8ff970f4340a8dae8eec4f1999f5fd4f6cbcfa372fbf85495'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: '8e1559cd4431febfa15662a2ccf2cac82f5401b2657551480bb0e3dd2111032c'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: '5f535e2e7351cb8caf0070166218238a843c17472cea2f5911008be5d7fd6ba2'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: '86ac4ea15f10c264b158058f5c13a36a87ac72f840071bbc45399b36823a5709'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '5c0d3fe289b2aac7d1bbaf57f4154b8d10875cffc9d8bd2402255ed1615f1d5f'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: 'd7d808366d0c8b76ce3e7ab80ea11b4e2f8758f9ff404a3aafbf5b0cc191adcb'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: 'e0768536856d1d7399667d6fd2c32f72416eeea1c40a313ee6edc910a5c3b786'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: 'd670923731b3e598f5c4db4c7e57fe2275cc6c49b4bf67cb91d520846aec256e'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: '2cb0bdcc305ef3b3d6b7265ab62bee555c524102679da122424713a9a01d69f6'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: '5acdc323fe067a4b915ee521ac8eb81bcff4e205d53e4e7f9a69d436035cc5ad'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: 'e634c43558d12c2a8710f2d6f10a86411cfad5a014e6b6cc159733c8ccece283'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: '4a05f4bc3fcaf50e6d0916d7e7024b0ed22e9a3c413ff4bbcc0922d2326dcf6e'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: '17c9d6029e15d3fd84e6809c5ef8a279a040f49ada91601a3ba4572cef7c08bd'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: '1f21e137da2427536758409f3fbf5842589c5f587f0b9d2d10430f840faaaf45'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: 'e3d38cff8a8d7fc00693dca5e37b03e7b10dafe4926023e26d937106ddac6a78'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: 'cd749eb05c67038fe837910310b3b4cdda190f6235fa970602f865bec1b61a1b'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: 'd596ccddea01b4ae29b68b0e8a191007f0c89a1016c380b49786f2d4fac4c43d'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: 'cbccb1ff23e33c59dc4c858093c9e215c3759acfe6bc84ff75940b59b25a4e40'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '7214c134e9a963d6c43969d3ef44ece825dd9cf35bda5fcce92a6b9d0d3fd1b8'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: 'aceaf5b775779621319f9ab5d4d370a3359cd6553ed2328cdc9dbab5b68840fa'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: 'e8123acb0a2fb62978d3811b31676975542993932108ab14d487ad7875ddef72'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '660202a436fb05c3d59be699734e77c9750c906c8597ca213d064853ecf8c9f3'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '4752b0a5ec3f1fb295d5bfa98fa63a0ba38a02a4c1e1f73b0c4d4e88a07e0317'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: '1e24f1467c36b051af3241fcf8c2c868b86dcb8e4669931878018e9914129b42'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: 'd1c3efc99d9487e147282d811ab932d4a24362d09ac909f4854e783887068891'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: '7dc455cf6f8b2042b6f0f368c44f18a080e5d3912ce3cdaf7142bd61ae50d02e'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: '4b991c15789084eb1d6c1d7ce8f0928df4d3931c0c22c571f375849b9a6c2b71'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '8b78f95a007cfb0bd054a1f5d962cd8d927665f79a5ce9e0fc31105e57b8460b'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: 'bf305423849cf773fc54206d8ae3c000c3e8b359cba8364581d1f91b0a201032'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: '47006af96cff3843d3ed53bdedb167490d7bfefd93ae3e9ef473cb53aa840fc0'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: 'c53cf5026162021fd2345dbad7c53d3a3df47b5bdff8cd34a0ccfee06dbb7328'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: '3326899b575f93cdaff757f8ab7c3996a2fe930450d5002d4575f4e4cc4b4360'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '6a912ba4188391a78e6f13d88ed2d14e13afce9db6f7dcbf4a48c24f3db02778'
	},
]

fn test_monte_messages() {
	mut msg_seed := hex.decode(seed)!

	for c in monte_cases {
		mut md0 := msg_seed.clone()
		mut md1 := msg_seed.clone()
		mut md2 := msg_seed.clone()
		mut md3 := msg_seed.clone()

		mut mi := []u8{len: 0, cap: 128}

		for _ in 0 .. 1000 {
			mi.clear()
			mi << md0
			mi << md1
			mi << md2

			md3 = sha256.sum(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
