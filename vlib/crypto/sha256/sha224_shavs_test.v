// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA224, the test vectors come from:
//     SHA224ShortMsg.rsp
//     SHA224LongMsg.rsp
import crypto.sha256
import encoding.hex

// This structure deals with both the short message and long message tests
struct SHA224TestCase {
	name    string
	message string
	digest  string
}

// short message test cases
const short_cases = [
	SHA224TestCase{
		name:    'test case 0'
		message: ''
		digest:  'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f'
	},
	SHA224TestCase{
		name:    'test case 1'
		message: '84'
		digest:  '3cd36921df5d6963e73739cf4d20211e2d8877c19cff087ade9d0e3a'
	},
	SHA224TestCase{
		name:    'test case 2'
		message: '5c7b'
		digest:  'daff9bce685eb831f97fc1225b03c275a6c112e2d6e76f5faf7a36e6'
	},
	SHA224TestCase{
		name:    'test case 3'
		message: '51ca3d'
		digest:  '2c8959023515476e38388abb43599a29876b4b33d56adc06032de3a2'
	},
	SHA224TestCase{
		name:    'test case 4'
		message: '6084347e'
		digest:  'ae57c0a6d49739ba338adfa53bdae063e5c09122b77604780a8eeaa3'
	},
	SHA224TestCase{
		name:    'test case 5'
		message: '493e14623c'
		digest:  '7f631f295e024e74552083245ca8f988a3fb65680ae97c3040d2e65c'
	},
	SHA224TestCase{
		name:    'test case 6'
		message: 'd729d8cd1631'
		digest:  '342e8e6b23c1c6a54910631f098e08e836259c57e49c1b1d023d166d'
	},
	SHA224TestCase{
		name:    'test case 7'
		message: 'cbf2061e10faa5'
		digest:  '3aa702b1b66dc57d7aec3ccdbdfbd88592d7520f843ba5d0fa481168'
	},
	SHA224TestCase{
		name:    'test case 8'
		message: '5f77b3664823c33e'
		digest:  'bdf21ff325f754157ccf417f4855360a72e8fd117d28c8fe7da3ea38'
	},
	SHA224TestCase{
		name:    'test case 9'
		message: '10713b894de4a734c0'
		digest:  '03842600c86f5cd60c3a2147a067cb962a05303c3488b05cb45327bd'
	},
	SHA224TestCase{
		name:    'test case 10'
		message: '006470d57dad9893dc03'
		digest:  'c90026cda5ad24115059c62ae9add57793ade445d4742273288bbce7'
	},
	SHA224TestCase{
		name:    'test case 11'
		message: '6f29ca274190400720bba2'
		digest:  'ac53157947aa4b2a19089182382a4363d182dd8e4ca79cd8571390be'
	},
	SHA224TestCase{
		name:    'test case 12'
		message: '17e8556176fcca2addbdde29'
		digest:  'cc6ad0488db0222066f740557b5758a19b30372b302332295d8c3aff'
	},
	SHA224TestCase{
		name:    'test case 13'
		message: 'dbf163601db9a122a4026824de'
		digest:  '9849845f4e47e1ece9a1c1e01a0d896ffea61c6c8894a75a11ce5f49'
	},
	SHA224TestCase{
		name:    'test case 14'
		message: '5e1ef2ad86ceaf5439fe87d2ec9b'
		digest:  '223c5d5d4a0116b32cea044f9af0fe44babea1c5ab201502591bcd5f'
	},
	SHA224TestCase{
		name:    'test case 15'
		message: '65f3b9866fb8002b53cfaf806f702f'
		digest:  'b1e0806a218d593821fde8e9eacc44ab5287c32209a94f011ab66b75'
	},
	SHA224TestCase{
		name:    'test case 16'
		message: 'b776708ffb91b3515ac46598ab9fa796'
		digest:  '427311b1d7ab2488791c4deeb4251d783fe5f9806bfdfb5188c5443d'
	},
	SHA224TestCase{
		name:    'test case 17'
		message: 'a4bc10b1a62c96d459fbaf3a5aa3face73'
		digest:  'd7e6634723ac25cb1879bdb1508da05313530419013fe255967a39e1'
	},
	SHA224TestCase{
		name:    'test case 18'
		message: '9e8f3c6645c1749b55c50d2018ce40dc2427'
		digest:  '2f5a583bf588c8988a572d128a95bea5ef1b66780a7d4be9c29efc31'
	},
	SHA224TestCase{
		name:    'test case 19'
		message: '2db6d207c0b7d9117f24d78ee59abf2f316978'
		digest:  '35681fce28307cae19522c23cbd4a77969347f7d8ee4a3088ba90ada'
	},
	SHA224TestCase{
		name:    'test case 20'
		message: '3df5e7f399f6dd61a12a9d4e9464fc4997c1f37b'
		digest:  'a3e68076e30751085a843a6cbfbf0f3dee63d9c4219c914372e50b28'
	},
	SHA224TestCase{
		name:    'test case 21'
		message: '65781d018f27ca0c72a9fa9ab4648ed369646dd3ce'
		digest:  'd15ef0d872d02da6427b8d0349dea2f204e67133b7365b4b150efc3c'
	},
	SHA224TestCase{
		name:    'test case 22'
		message: 'af48eeddd93fee69d1bd7de428a63986011d10945eaf'
		digest:  'b89d428ee42e397cf11029ecbb27baddd036c8938f51c8ab56b875ac'
	},
	SHA224TestCase{
		name:    'test case 23'
		message: 'df2bf0d5f9c994ac69d78baa0d512eceb74d8a047531c1'
		digest:  'db8e1ce68c8c6b84d6db755c2b8bf54f3c4b081a881efcddaf303294'
	},
	SHA224TestCase{
		name:    'test case 24'
		message: '48d2f20955ea2d13433c20bc0404eb2e6ad79ed28f7cb4c0'
		digest:  '3617cc3179f8b59adce181eebeed5e2763f62650949224a67e53694b'
	},
	SHA224TestCase{
		name:    'test case 25'
		message: '218f74a42d3a47ef3b806601fba024b078cbff4e4b85772e0e'
		digest:  'b5f40b95dcc363b97e9d00b67c5d7c37f17ab563297d2d67a4df20c9'
	},
	SHA224TestCase{
		name:    'test case 26'
		message: 'ef55b1e797000b04fcdb9b3021b09327e3b4e269d20cabdf418f'
		digest:  '827b223d51240c2e3271c534c19c5637b6fe10083e85bcf06761ef21'
	},
	SHA224TestCase{
		name:    'test case 27'
		message: '96df4387dc2c40297043bea36483f65e4eb1e07e93359cb7e68610'
		digest:  '98e430a63fcdedafc9419010f7f59a4d816a45b4f973beb62530ff8c'
	},
	SHA224TestCase{
		name:    'test case 28'
		message: '3ec0aa8d30d5ed825b77dc7095f421b1e608158797a377ff8bed641b'
		digest:  '3108321eb7ff857f6aae69101b937f32a51ea279a6c14ba5232ac8c1'
	},
	SHA224TestCase{
		name:    'test case 29'
		message: '8b0239712039f077ce323b35f4e306787b9b35270096e57735cff45d84'
		digest:  'a5c740d3ce46bb2e0a048488f2b0605c6d0ca0ea2f382d043d13db97'
	},
	SHA224TestCase{
		name:    'test case 30'
		message: '044be30167a9758c46c727921dc4eb4e0dcb965623423e6fdd44e7a4ea52'
		digest:  '6eb78313c743ea8769d8340f284dda6ded64a1db64392f21abb82c5c'
	},
	SHA224TestCase{
		name:    'test case 31'
		message: '57f6118bacce47ecc31ce8b0c083d3c9219e0dbe9e4fbea154537c41231acc'
		digest:  '0dbb53c866d63af44c222c76c825df0e379dcedfb958db03b6fd29a5'
	},
	SHA224TestCase{
		name:    'test case 32'
		message: 'fe1f0fb02c9011f4c8c5905934ed15136771737ce31c5859e67f235fe594f5f6'
		digest:  'bbeaacc632c2a3db2a9b47f157ab54aa27776c6e74cf0bcaa91b06d5'
	},
	SHA224TestCase{
		name:    'test case 33'
		message: '14fb01ae9d6015ecb3e56d6ecdfa4bc0533186adf8457f5e4a5c57c687895f3db3'
		digest:  '178272c7d7cc71b15074c27e3b7997d4a3ba99626986a1a16cf30030'
	},
	SHA224TestCase{
		name:    'test case 34'
		message: 'ff6c49712f044f4063c14125c0cdfba18ed8b7138453768a45dfa2d82a05f1e84227'
		digest:  '403284c888a7280bc8bfc25f0c34182cd378306a21a1404d4e1c40cf'
	},
	SHA224TestCase{
		name:    'test case 35'
		message: 'f900bd7e0117247f97c8fc7a665c76a35f571c3366571d6c4a3ee5d7fb93f1d1f726e2'
		digest:  '48235b9820d66d8885faabf6a9ede63ba2a21b6177e987a33242373e'
	},
	SHA224TestCase{
		name:    'test case 36'
		message: '42d38188ac49440cfefb77db975e083e6b22348c4c67f0f8692e88ad140d861dc828d595'
		digest:  '615344f890e5bcf71b5efe39de1fc942ba1fe30dd9e9146adb6a41bf'
	},
	SHA224TestCase{
		name:    'test case 37'
		message: '74fdd7d958b8ae7c2c3c5cff4266dfb2b3b842c9f59ecbbcaff575edcbcda08ccd6e08b764'
		digest:  '66d7d6c54fc7775a0ba845ba3e11719fa535b9289f20b098c5f7a342'
	},
	SHA224TestCase{
		name:    'test case 38'
		message: '934416dd0581e22f2bfbece7bb64afe820451fa21342df7e6f9fb37c4103381a1f7cd379bcc4'
		digest:  'fae8f1aa22def4dbaa814c5b0babdec43394951792c937050d2963a6'
	},
	SHA224TestCase{
		name:    'test case 39'
		message: '102401c84a716ae72579c6ae79c359ea309ffd95abffae4c61884c03c9e99df77b6c92e492cacb'
		digest:  '8f34812d57a16ef8a51ad987660c5f8623e0fa9d89846e28d46d14d9'
	},
	SHA224TestCase{
		name:    'test case 40'
		message: '79bc8fb60f85d15a2386566e3e7314df284533085add1c7bb6ead3ff760c86d5633a66404761b544'
		digest:  '65c54014cfa30f0bc27d1c6efa96ae8481f4c2505bff272956eab0df'
	},
	SHA224TestCase{
		name:    'test case 41'
		message: 'db3121ea71294983b185207a9d8de3e484a66c0431bf07c962eb82977c4f834b7c3f1e7931a4a7f7a9'
		digest:  '9316d2f021c2913d63a7e66924c87c161c3cfde0ea7ba07f54772862'
	},
	SHA224TestCase{
		name:    'test case 42'
		message: '0dd51aa660c5cb4b7f78c46852c1db8707ab451c1367b6187388c8bb3873a1aa4210d0414cc6792a29a7'
		digest:  '31989e7a62a5132a5070d77250d8904bb82d457dc63469d06b50185e'
	},
	SHA224TestCase{
		name:    'test case 43'
		message: '487fd2e5b694b7071d3789a258a51e8604dc0d3e8f5d62f39131968e602abe1ddf6b0278962a512408b553'
		digest:  'e798683438284626d710877d9eea3a0e02f349fc43acb7f9f8f9e81c'
	},
	SHA224TestCase{
		name:    'test case 44'
		message: '11183bdebfef58e4da5b1cb73be0d30b20da304d8659d921da2e270fd14626799537e4d12119e809ee97004a'
		digest:  '96870657d6cb668be3995aa8bd31df77840d1d1915d72482e83b6b2c'
	},
	SHA224TestCase{
		name:    'test case 45'
		message: 'a239de5c8e2644e8f030d94d98f1a30664e6fd961dc2977a9c08be5c31d8de89450945a53d79299ea2a1edde7f'
		digest:  'e99743d4fd26c8800c36a67b6762247c29da6b62794123c59de06dc0'
	},
	SHA224TestCase{
		name:    'test case 46'
		message: '917c4577aa6b0f9df49999fc1c958cb09b7fd5fc80be949670f03545eb27dcaed052076b24f96f5e0f2e2f4527c0'
		digest:  '7ecd693d4d9cf43929464698efa0bac33c2e1424f816edc769260978'
	},
	SHA224TestCase{
		name:    'test case 47'
		message: 'c3f1e735a6741aa481ad577a98dbac1f03cc80ea0dae1b94db2369ed4e93facd29c64e4e77b25038279120bdfa3715'
		digest:  '86f0d89d8e14fd8b6606412d71a7a54a347b304ea5d49c208f2266ab'
	},
	SHA224TestCase{
		name:    'test case 48'
		message: 'de4fbfd553cdf37019f25afa82dc6b9970f4bb1ebbc37f80d3084c88a70722cdc523a9e3c2afbad0dc0221bfdec9a2f9'
		digest:  '4c5262acb4a2a44eaa9bc6757024fb202ef4d5a7a16fa37252a422b5'
	},
	SHA224TestCase{
		name:    'test case 49'
		message: 'db2e2eb636610cf42e9b33433acce1b3b925949f297dd83199f45d2861d64cd910c2db74a60b2089045e22cba0a536137d'
		digest:  '16bf4e45bcdc60447c68dcb30e6b08f55ce9f4124a29cf1f9a9d065d'
	},
	SHA224TestCase{
		name:    'test case 50'
		message: 'a8e729d336d5d6ac50e1e22f0b193b66e26042fc6459214129875e740ab2b142918c138aaf941863ad3b7e6065450613b273'
		digest:  '452bf2e5ebfc4e451cc434bc09e2a10032eed0b7627cf55e7e5ed0e2'
	},
	SHA224TestCase{
		name:    'test case 51'
		message: 'd05317d4b535f9d10f739d0c2dedf3ffb090c1ad9d205089b1346693f58273c4925c0face57ba45ad6fc687c66a88fc78878be'
		digest:  '4f03c439e097b51b00e314f675937c4d911505859fb7ab16adc65e44'
	},
	SHA224TestCase{
		name:    'test case 52'
		message: '26bb4ed4f0424c60fe4212ff8c955e89e2f553a7d7701be59416d2089af59fa1074724e214e919b1e30f33fb78374b4b055bbc9b'
		digest:  'e7c899e27009d4dc77c2d300f191b757e52c9e7eac4b023bfab2b52a'
	},
	SHA224TestCase{
		name:    'test case 53'
		message: 'f015ec83944f03292463c4345fdb1c26d1ea07645facbc9520ae244b6eb191e53dabadb4ac0fb15cda4ed77dfb9e1193abfafb1b81'
		digest:  '459e40b3fbd612912f0217c60099379ce077cd02505871b0c9c14e7a'
	},
	SHA224TestCase{
		name:    'test case 54'
		message: '0786706f680c27b792d054faa63f499a8e6b5ddb90502946235bf74c022d772c809cb4171bfa4791539aca1abd91900e53ba93ca0efd'
		digest:  'fadebab7c3d0fb8e97e429b79083087735e4ab385a789521260ef3ad'
	},
	SHA224TestCase{
		name:    'test case 55'
		message: '445e8698eeb8accbaac4ffa7d934fffd16014a430ef70f3a9174c6cfe96d1e3f6ab1377f4a7212dbb30146dd17d9f470c4dffc45b8e871'
		digest:  '4c7ae028c0fe61f2a9cada61fae30685b77f04c6442576e912af9fa6'
	},
	SHA224TestCase{
		name:    'test case 56'
		message: '52839f2f0853a30df14ec897a1914c685c1ac21470d00654c8c37663bfb65fa732dbb694d9dd09ced723b48d8f545846ba168988b61cc724'
		digest:  '2f755a57674b49d5c25cb37348f35b6fd2de2552c749f2645ba63d20'
	},
	SHA224TestCase{
		name:    'test case 57'
		message: '5fe8c2072d8900287ccaf07f3f66b0c22acd3e0bb91d9573754e19e373ac35271d8b43443436ac0c162850ef3d7f281409ad29a9bf716c77d1'
		digest:  '42909757f6e229f69f04cc7a863c4e70e48c7c3575057b455c959775'
	},
	SHA224TestCase{
		name:    'test case 58'
		message: 'e8064d83f3d643af8718c87e3ccd6a9733685eac61d572a22ab943f232fcb04f70858e8984449db14a76bb7eaf2458efc3ed2a32100622c52b7f'
		digest:  '1a1d8ed54cb45c97bc970754b43eb93d9eabde4c7b07f76ad82d8ede'
	},
	SHA224TestCase{
		name:    'test case 59'
		message: '87c9a517e28d1bb54ad20fca76460efd894d7786e68ee8d746b2f68208682157c8ad06cc324ad7a3189e09c6c39d4c768719c0a49a41669f2767d5'
		digest:  '605977cf87b9b309bbddaaa64e528ace66b04df9f72c0e7ec88be1da'
	},
	SHA224TestCase{
		name:    'test case 60'
		message: '59fdac3b6b32039291801c7d6f46ede8d26dc5b7a192e007116739b617569f2523680b3c0b6631af453e55805aa760c6970833ac06963bbc9dbd455e'
		digest:  'e9f0cb1dc8337e906385892f2348a8ba4412318ecad9b96e3711531f'
	},
	SHA224TestCase{
		name:    'test case 61'
		message: '30350a4df0b58ff49c0fa09e426fcd7007b290c760c825c1855d9b0023b82caa51e3cab4c60cfa61492be50568e5ac0f6db0fd468e39e4536403e3809f'
		digest:  '776cc6636c02408fbf65ace73ae80017108b917c16c5a912fd860241'
	},
	SHA224TestCase{
		name:    'test case 62'
		message: 'ef797a0d43c30b4fe1014bdb9420879c2ff845d27e73d55a7df22930c8ece73253d8bb265b4ef2ff9c69455cc56ff25229b4126bb7bb26ee2c9ff36187b1'
		digest:  'f5b9ffb102affac352a4a535a00f89b06c268cf4881d712668906025'
	},
	SHA224TestCase{
		name:    'test case 63'
		message: '716944de41710c29b659be10480bb25a351a39e577ee30e8f422d57cf62ad95bda39b6e70c61426e33fd84aca84cc7912d5eee45dc34076a5d2323a15c7964'
		digest:  '61645ac748db567ac862796b8d06a47afebfa2e1783d5c5f3bcd81e2'
	},
	SHA224TestCase{
		name:    'test case 64'
		message: 'a3310ba064be2e14ad32276e18cd0310c933a6e650c3c754d0243c6c61207865b4b65248f66a08edf6e0832689a9dc3a2e5d2095eeea50bd862bac88c8bd318d'
		digest:  'b2a5586d9cbf0baa999157b4af06d88ae08d7c9faab4bc1a96829d65'
	},
]

// long message test cases
const long_cases = [
	SHA224TestCase{
		name:    'test case 163'
		message: 'f149e41d848f59276cfddd743bafa9a90e1ee4a263a118142b33e3702176ef0a59f8237a1cb51b42f3ded6b202d9af0997898fdd03cf60bda951c514547a0850cec25444ae2f24cb711bfbafcc3956c941d3de69f155e3f8b10f06db5f37359b772ddd43e1035a0a0d3db33242d5843033833b0dd43b870c6bf60e8deab55f317cc3273f5e3ba747f0cb65050cb7228796210d9254873643008d45f29cfd6c5b060c9a'
		digest:  '9db6dc3a23abd7b6c3d72c38f4843c7de48a71d0ba91a86b18393e5f'
	},
	SHA224TestCase{
		name:    'test case 262'
		message: 'c39147fad02c6ed5876eb3257837c3f456008694fac94932aa521bae9c8e77abb65524e298595abbdc5b261e9c9f0f86359dfa584bf94b740eb54d09bba6d4ad652421adf50605a170ce4a4478204e831679f1d4b61db1c9735114e595cb47ae58670403f01bb8b0c92de64013a3c6137efc33b0421dc45b638e38eb33e617d61884968e8f80bb1071e1c3b97611c15cf78e8793f8e1c95265d480c29ce08d4c2ce59ff3dc1e56e8f053a958a75085890897b625de520bd6028bb512d89ff1391cf0e7dddfdbd160ccea5fc5b04a5ac03a7f890fff56d10dc01b9f85f00d8bc4710f35f29fa073a28f0dafa798e58a2913aec5f960d662222aed06c1eb11a216b2a952da2456'
		digest:  '2b05b170d4976409f23ce885a0a2c0a943226688d4f5bbaf35fabb46'
	},
]

fn test_short_messages() {
	for c in short_cases {
		message := hex.decode(c.message)!
		expected_result := hex.decode(c.digest)!

		actual_result := sha256.sum224(message)

		assert actual_result == expected_result, 'failed ${c.name}'
	}
}

fn test_long_messages() {
	for c in long_cases {
		message := hex.decode(c.message)!
		expected_result := hex.decode(c.digest)!

		actual_result := sha256.sum224(message)

		assert actual_result == expected_result, 'failed ${c.name}'
	}
}
