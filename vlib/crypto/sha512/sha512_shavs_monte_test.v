// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA512, the test vectors come from:
//     SHA512Monte.rsp
import crypto.sha512
import encoding.hex

const seed = '5c337de5caf35d18ed90b5cddfce001ca1b8ee8602f367e7c24ccca6f893802fb1aca7a3dae32dcd60800a59959bc540d63237876b799229ae71a2526fbc52cd'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: 'ada69add0071b794463c8806a177326735fa624b68ab7bcab2388b9276c036e4eaaff87333e83c81c0bca0359d4aeebcbcfd314c0630e0c2af68c1fb19cc470e'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: 'ef219b37c24ae507a2b2b26d1add51b31fb5327eb8c3b19b882fe38049433dbeccd63b3d5b99ba2398920bcefb8aca98cd28a1ee5d2aaf139ce58a15d71b06b4'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: 'c3d5087a62db0e5c6f5755c417f69037308cbce0e54519ea5be8171496cc6d18023ba15768153cfd74c7e7dc103227e9eed4b0f82233362b2a7b1a2cbcda9daf'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: 'bb3a58f71148116e377505461d65d6c89906481fedfbcfe481b7aa8ceb977d252b3fe21bfff6e7fbf7575ceecf5936bd635e1cf52698c36ef6908ddbd5b6ae05'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'b68f0cd2d63566b3934a50666dec6d62ca1db98e49d7733084c1f86d91a8a08c756fa7ece815e20930dd7cb66351bad8c087c2f94e8757cb98e7f4b86b21a8a8'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: '937d7856a82a84c163c79417d0540c47daaf9ffe662c843737dbbcbe5f865bf6f47a9d2bd10129a4f498073094653c324a2519a1c71ac1279b1623ff7d24647a'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: 'f8fbc058c2b9f84131c9decfa543a35ade41581f670398efd61b3abfced9c1cfcb5324f2370487f9c59a65bc668ea596c8d22ce8a33014dfad28357fa7d05f04'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '4ab0c9484ff5c30fa64ae6e81510c5fea566eafb88f175f8bc19109f40fe80014c8b77fff10b8750778429bf3c5497e4cb92d9b30014f4cb975dff2a45244c28'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: '685179397554d276513d630234a03419808c698abf2600d7490aabb8e455c6ab6ea412c7729dc140a79dff66533c6946cbe90f9da9ed16e2e629db1651bea870'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: '335e6e941ab7dadfecdb74ea6cb4e8584b6e3408841a33a6cf7fd6a63294b1930a60983240311672acac3840a90e64cc366ce75081b2252627e9c31197ebad03'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: 'e3217f6af6e279e9445dc3738cbf9ba0e9edba0455844a73648139777afdea2c4d8032e214f541bf92675fb23f24df8e4fe98e0003aadfb6d8f9cc2cd799bbf7'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: 'ee2fdfb3ae630613b7d890977cf2515deac272a37f27e4a01961ecf103d4ff5b45cc8aef53b635dd75aa51aabf71c0642555ccd3281e0388f8ca09d83258cf30'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: '6a30d97cc98af6a25b673dce7aeab8d762bf2e55ea0c6dc899179281f84dd02a2896f77e9c106b472f55f7adbef7b1157be567ee1236ebdac2a3c5d8cb133eb5'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: 'ac1176abdc5f71170183d92ae55856221b0d95590af11d9d72ba605ec026bbec52d6974bc43a1efb125ff2b161fbdc616fda00f04193a0bc26aacdfa052a5741'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: '59fa909480620ecc08d34531a6da1b55158b74fc93ddf68e1d242615b6f3843a7952e63e798c6445cde1b07e0be09d0d711cb7b42a0e7760a593b08acfceb63d'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: '9eb253319efa61b864f27bd334d7dd78b38d3265fb544e0c8edee950a547e1d8db921a285774ab94d66beae933298d20f2a5aa87c62fe1e383cc3b18e7af18ac'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '81735324005671f7bdad9e685ee8257f5e0622b9fcb5d38dbdfb2df27258c3e1d46d76e24c0c92c744e1b50a2b4b0d31525b3af83cc80a75722d921bdeef59c4'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: '17498cdff4323bb8021e44eca6559e05d8ff9a0ef2ee9d4ba0ac6e73f83972a0dfbb6d47728fa70311d7c82e154966e1b7678263b0f65133e9116969193d429b'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '228c4574d7c45eb9ba9240722133fce74abe00c7328ab30b4bde373dc79afdd6e0569d36268cd5eaa2f27205fc00512577bcbb6699e1d66ed85eafaba7548afb'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: '3d40ccd9cc445bbecca9227c67fe455d89e0b7c1c858d32f30e2b544ca9a5a606535aea2e59fec6ec4d1ba898cc4338c6eadef9c0884bcf56aca2f481a2d7d3e'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: 'e1e577aeac92e3a2b7f8a262bf2ac9c037d2274ca6618fbe4cc21db7c699e9946b6671ae45ea433a1e392a5bc9eec96fd641ba8f4a047f022a04a337227004df'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: '5e4424c0bcb2f0f7a2428821a9d5840a82401f4440ae6bed25c53cd9e71cf9d39904d6a375bd721f4332ab0202529c91feb9c094c3e6d34ca4f66649ee6fa212'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: '56b199d63ca37189d5ca0d40006ac7bcb9f39cbdc00ef7b8a5697caa7d81d05b645a146995b1151d01958f1589337e14afc6e7dd10a815170e527a398e6ce8c3'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: 'd2d498ff93fb03013a64f295b5bc68e57d2fb5600da578aa011d43ff432eae3e0c800f9e2a53155e56fdbf5e068fe2b4beb3e42b2585531b8b16c4d8ca3356c6'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: '3d3875489903710f17cf4247b5842ace6f017b1a3b99e9ee5fbc04fc7898e78b12693879878028ca40c63cd0f6925fb7d0ca0412e4f06619e3ace223690f03b8'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: 'a013e21cd1234483c95c2ea2757be949bc79401ba39b09c316a1612d594642be65ca106e12695ac3808c57c6f2980e895fd1fe188946562afc238414e1e43649'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: 'c5f6367d7195489e16242f912fbe0d8002e947de3a7e9c53f77b1e5e90e05bd7ca395e787e34cb5f500c02da59c9d83de35601de7ae80dae74a0d6b4a292d43b'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: '7c28c44c6aaba83c122f24d68273e28a5afd65b4071d02b7ea3300478d5118971e1356ae57cbc70d2a177ea464a1c2c50d4297b933e789c63b1481797ae8f08c'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: 'af7cb42b1c70a85ac1ae1c2991b25b657c19f4fcf83af7f7dc0ae1028c1452a6a17dc98929634fe6ed3855b70b96bc2caa93d82037b94ebeddc77e4c1a7cc563'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: 'bd56ad4c0cbd162706053da929d667253aadcf417affb483fff4f2699bf406d128cfdf5196dfbb05bb89ccbf04c5147bd2ebb3156b0bc1768ca6faa171c91c01'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: '004d7b0fff9bcddf4b3913ae190a76728705a3d23874d92a8b7ff246c8fcad46623cb04723c8aded0cba4968d1a8cc1375b99005786c1bcb7ae4bf13325c3ae0'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: '8299a5bf5ed64f525c4eebbeca969fc1b91a81adb58c584bdd2d7676386a31fa546643a3cf505007584f02fb712d708cab645bf078a1b9339f5a76aee985d017'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: 'ce7100f3455db1a9776a9f40d562ea998afca1f9fee7e0d81c8db34cf68ad23a8bfa6fc04774703e1e56d5196b66966158fcf2a8335a58c6ba7ba1af756ba1dc'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: '90aaabcb655ee921b8350229efe6064a60051cf0cac858fa3d43afd5b97cc82301bd1b8cc1f874022e5af948185638783a13ca1bbd5049ace7fbf4f6d90c201f'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: '3cf0a25b33ded3e0806dfe603b9987f1d6f2b3fdcb1ec7f8566828c00e17e8f59e38b3bca302396c7525ca194e6cc8501369059e2e34ae21e3141215876847c4'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: 'bdc5266aee339a1ff13fcf5229773cd3d14b47101e83076927c160bb71bf7445590525a2012d52af008e118e16df1b6bfcaf8f22b4e45f9e749f3c20625a2bc8'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: 'ef8d2ba885381ab97756d59dbbbf53a1ea35d152b2d8f82c3518430aa34e708359194ea43950d032e151f576d343a5c3cfe6b71d4ed0ead9d3a107402589bad0'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: '194ea5324c4179998dd7057755f255fdea04dadf533f7851e3e9718b610948e32fd28323077d9421142ac808978adfa325b668c8599a2e01c757a5a14ed2dd37'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: '106984d2f0087e621dae760552bc6279072267883c204079481af6034354f1a2b77c17e6c039a1063e479342aa3ccd90330dd3fb5a7d5e976619497e2d3326cd'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: 'a1347216f1a6db47b90c4ded3c5c75440f54c22c87d538314d1340f86f88acba01378acb933ddad0adc6b75d55bfb7e8efc9c4a531b2a410610b7515b6dac66a'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: 'b76e4db147e0eaa4f04880654088b9d0fce518c8c377d92c846345604dc6b2b18d377fdb8e30f06d9bcfe6d7dacc07d6adff73d98d49f8f132b80f3084390830'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: 'acd4e527763dfd4513f0def0b1edf8ea12dc78d336b7b796f3dcc32e1068725443a2f55ab4f666b27d6bf2ab39669c98293f0a9108051fd3144d31a1ed171ddd'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: '10128c15494bc87a87374f676ef9fe2df20b36ffcca41a80bd40b216637b3de710efd070e277827820a7bba3cceb7b21f8fe7f9775d6c4df4d3da5349434ec49'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: '2632dd5c188c6ed3a4610405fdda704add752f5424d9de65a51400fe478e26cd0412e5f91ca4b744c34f4954f40a3a4254431d21954623208b527b7b4daa687e'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: '45707f5b6fc5ccd1f78d77f177d10fb8b462c74cc821518cd5cfa4b5d6b40b418044900693c37abbb82367d340fec67f800d74072935da1706b4d90ae26099c7'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: '56c37f31220b5b3040373d91b2c5e42fe9e601a12f7f8dc4534459bf28e484b8713db243c5782c031e674003a3c14c42fd152e7188789065e82795e10f87d54b'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: '5da94c899d48bd8299fee3d81662f8d6c5f8f8bc54d18cb0368b13cebaee7ad71e74ea80f34974ad166f04f9a0602809166fe4085a475a8ca86cade12b6754c4'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '0664363f97ba910760b0922e31ca880ca97469506cb007e3108c36c3ce3ce1801fb4197609479339e8820632b6a38bffffee05a9adc11cc544b9aa6f5b95cc6f'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '732c41a1edaa727c04f627ff158aaff67c18efd667216132b99ab84d108996a10bb008b5d803b22ed1aa78bb0d10f8a762fd34777d7dccce8e84827ba88d4193'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: 'fc9c21d67e393a2b05a23a17d8db630cbaebaa3def211181749f1bcad181560627fb60ee20fae2e5980cbf50fce0a19dce807e7fb75c4da0ef008bc75d413a65'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: '0453b765afc1edffa595efe345177f5805ed3abc1297ceab757ae7161723a6144cb543299f418049276d16b7896662631634fab9549127c10f27505b7dee8665'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '3853f3bf024e0668e8d1ea53733a97537f97d9307c5f3a19864ab4eeb1654710693bb961a344dec8a758f5e64b26fcb6dd423419c4a114fa749211a9de06c281'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: '240137f0dd57beb3f7fc283bb3ead423c67883fd46f4e27471d7be57ad469a49bad03a3658418bd55614678f3a463bceff85291314b90ef43ccbcb028f0a7a07'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: 'f9050a5271edbe4cfdb9520ec05bbdc3cbcb9bce36fd212338d3e7028a39b9ab30793e561d75a2e424193264c7f0775e65599ef0c94e0ad24dbfe18252364267'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: '47caa7a5862fad837aaa409a4a9df2575e645528c35159115911b7c4e2f08ae49d68de97249b31b83ce2c163f649cad4559dc6e6a7191f2922d79a5fd6af167b'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: '13f5825c41fa49edf6104e3e35c9c224eba93e37374f730004c39c54e7391e4a847fd61865235a3fe32224c96fbe86f7e14c3d5df496e83ec989a71b4f293a44'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: 'e5b55e05efe1ca6b9a96a57e3a1523d610d70f837e93b31fa98c2736d3e114d238d46ec6b6e3d19e774b253f6b0c7a2ebe69b7e60fc0874444806b2a2278df45'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'f14a586ac30f0af255f597a9aef9abba5e99c04d17b01f24427c4ee2c196b52acb1ceefc9b15cb822b3ecffdc2f7c49e11d3fc0769acee33361537d379c62e0c'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: '7e2d3398807195c48e6ec52d20710bbf8b21ea8de4d1abc197897ccc58aeff40259edc67270cdae0edcc686c0d0dccc5760c1495ab1cf48482dc2000ae2d42ad'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: '2f3d5c5f990bf615d5e8b396ccbd0337da39fad09b059f955a431db76a9dc720dffc4e02c0be397c7e0463799cd75fd6ab7c52bec66c8df5ef0d47e14a4c5927'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: '483a1764d308cc494a2b543d29ba616483aefdf91c7769fd084eedaac1add1891df95d317a47430b2bf73e4081f86597020e28afe2d34a22b77ea62b6112d09a'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: 'bfa88691ec951511651c6f14af100eeb26d87729e18ac3ef49a80d73ffeaeea53e97c4a7277a7ee9f2fba070b1c9720d6cdba407dd82267019e3f0f5662b2f2b'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: '4c17c8e2e7132dbf82afebc40efc77926d16f4d2c082d846dac28733aa767e2840ebf04f2563df75933466a36e11968d342e4157827605d04d9627ce9b5216c8'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: '70bbfc29a2a765220af84e7bb10d759a3152ad4b5643ef6b89966950ec7ef9503d57bc0a28c4ee789a60bf9dcac59139e15241d73b990410cf92eff213da9eca'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: '8d1d56f37fc19b84984a6fa33aa9c2dbdbf79a29c04ad0b4cf20333e6bec943447be2416242f8cd2f9732e79bb925cc5a61a80c5fc9c079961243fd1c1f5900e'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: '492fd0171f4dcd5d20ea6c0d34b5576c8894664ae5955e6737f5e3b711c2804d99ccca065b7ec18c82da98b18a3029b765c51ebc7c433b36492e0ed6b8511bb6'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: '7f49e8e54db7e5b4323cae2db71f3e8b8eba172dcad3602e9b7b058007a5589358732d5afffa56072a46e89b1ea27ef8d556deb86b569c635d394f15d99d8a15'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '56884a6a9210d5f371e25823efb2511a9c410c26a441e07c1bdffe8605084267d49c315baf6a692d7d97844b2714b4930877a5d7f52cf6fa151700fcb6980546'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: '6aaef8284eef221ecb17ea3c9596f075b5155fe7b925d737ed3c6543c761c28c7cd9d9d4b5e2a37b2f183a2a367bbd34b633497bc7a1737d61c8c1f3ef295062'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: '38ef178f5688e59d47c375252db7b39f40c0c84169878ee7ba5086e4b25fea81076b9c37847e9e6bf24ae0b343689c265ec5ca7469e619acd61b0276721efb1b'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: 'e3fe1aabad120777cf24eaae289b486632ca46ceb89afae73dbae5fa87c767879369355a9cc5c21ca604ed91d0f2f58c466573f3e6d88e52c62c0d3cb188e141'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '82f5bd920457bb2763a0da031a7fed47b236951b1ea420c20fd2b6de1dbfbb9c4600ea7092788493e2d4be6ee24b6dba04e57af3e8f2f14d9837295420ac7631'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: '6d0b26208ba9b1615067bb3ff97b292fe67e4c02d240d649c32370e0a4cd22d03bdf864be4d24a3f5f51aeccfd1afd5191e590edeb5f7bec323b0506c3104b89'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: 'd081083158054d08371ec84f4d3aa5aa761734ac6091a30330a861fda056f835c750bf4f7981af1693ff28545366bd05cec47bccd77a7d237befb0135c534138'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: '6ba8b52780b8a07a2a2015dd8f0c5e7437b8e024c4ee428f7ba91dfea118cb72a939872550983317132b841b7cbc29a22b8f1cfea0c55203cafc69b55ed6244a'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: '312692b0a51f002b7f06d05b39d15a5637dbddd2f4f1a73e6c88a4c841cdba5cd8e69c0939ab39bb1a9c54fa35402143c97edb9704a0e9e1a98701710f6a5dad'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: 'aaee960de201a8dcccff95b834fccf0dafc03fe6cffc0429162bf4aff01165ab07a0c9435e9cb412121b7ba010657ccc3152118602b665072136317d92fd4262'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: '21fdff552e08c86c07f080cefacaaaf31846eb893bfe2e4f88c3c3cd8cbf592a84500942695a5e5ae971ab343ce2695dd1baeb1f94dd4b53d678e14265e421ae'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: 'ca8f1a5b2172f6adb474da53b35e3f73ffd88263d3eecde72e48b16e1a0658015b555ee319005a1d82802e91431ee777610f9b1028d819921e1044ad426b0270'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: 'ce5ab25eff9c1ddc569a1eaaa66b689109ee269db7066e0b02d39b3564fd14ca6249987b7791e203d3d7c2ebf18558d2f23f94c03dd1d03aa63849e4d2889a76'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: 'a6f8b0561000dd4ae8b828c5f676e8c1a6474c4a042a645f1815bd52e9ff53c97dc36d5d8997f8ce332185feead76267f5b2e63f597fb3345ca0046e58fc0f24'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: 'fec86794bad4106c5ad1c1a2d9a1b7aae480396ec231eb5cac21c4077d17a0b652da0037363399a5a1dababa4a40e4c54b9124167580dee9108c4dbb24c57512'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: '594f5dd3f4c87bdc0d81309386e9163a9718e34c7b0dcb4613f8487aa786f9d211cfb61bb247fa9f5ecef042e710f192850f5571807294bfd8a54397850e5773'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: 'd81ad866f25ef6a0a6431d267114da564513e5ebdcf48db7e95db8cf32a89f0ab107874d796035db97420ffcf1db5f04dc1a52ddbbb960fc63b7f3f835cc8be6'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: '431d537e098e9949f6a68108d55d20952e3bfcdeb7273bac3917e37790a84fa5db04c33a79c113a06cf333e831d7702a00853a93fd0aa5146d934f4f71242a6a'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '4ed95636c6885ae4e63d042e82f4da830c702dbf3b9746d64770a64dd666b33208315f3a947c4dff790771ef283788a9c74da83e22b97f750286a820ee46698c'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: 'a9bcb60b4d7724cdddddbc232b4ac70b94d0d7e9f0724b1222d918930cbb9bdbb04b3ad43e3c8caf3bf8b004ee4aec6bd527ff8eb6189b44827f7ba7057f6a90'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: 'd6d5e44d5bb07fc4144ab6ab309f048968f73f7992beb326047e9e2cd7af6240bc8abf46703c32fdb58fb2a8672594a660ef855be74f24cec09d4fb00219de82'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: 'dfda9ac0c7147530da97715ccf47814182255f2f2cf40287db97a4c63b43fcd39e6d41e560921492badb253a7dea0aba863c7c33b912bb59d1ff4de03a4f03bb'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '0395faaaf2e907f27779d6f1cc9c9db68ec390a38fbb0702c6475b46f7a399498d46fd8014f834b131e1e83abba0359b1f16d8fc0a393580615def2ad0caba73'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: '41cb98f09029abe85d24a0f131f116c7f69f54f7e91c250642606512bf3da4ca89ba70a4714a5f66d9ae81ff09317dadaff12a02057074c970f0f02a52bfafd2'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: '8e8f161d48e306c5533ed614b8ef3a1979df6db7e13d0780a73c4a3980ddf0a95f93941d412c93683e39915a660c3fbec0dbb1bb6beea2e2099cd968011535c0'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: '789593f0b8fb83ef9b3ec50ab8f6e1e47344f763d4f7ceab5600989e7b6fd5fef6ee5e487975f64474af6cd71ae4d9ecce8f009edea0227c7ebe73080b8f961b'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: 'f37e1449e0b313d9537a6177f7a31158d353e5b79c781facf02526ec94e0c6cfda37105bac67098b194ea82efb307c2929a9ab8aca0e76c53e829e3f901cd245'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '2e74e745caaf2d449ab3b031dd214b48616853a512cf2e95c40cb8e7594fe5e4879ac8a26d02eb35b3b96a5c9e7dcae3e15fd050a0bcc1fb3b9cb9c4df0fad3e'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: '6eac7069c26082e52574ca6a58abb9b1b9faf452e8cca9f1c7023679ce192ca554892f30e38104d39088a24df35612444a0fc90084af7535fd9344fa51dded84'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: 'ada6caf30c4f6e3644d952366e01519af6771b406e2c447552f0c597b8dd10e9e9b4e699c9a835de03f422be8980538d9786172dfd2fe511db272a1543d5aa35'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: '4d4b0086b2cb05d713f2805caa7e6605c8f7dbbb2e0f92aa159aebdcd63060305f47b748f1bca6e0b6e11cf8f9697fcccb6584b878c4b54a699290728a40aa1b'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: '97420b8a0ad102aeb92139da2c052d2748dd7d2dbb93a9ea79dc15b520d0ca7cab8cb7a00f5b5aebcb49d7e7f52a27180935ce617aeecdecba04064c668edd37'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: '4aa7dad74eb51d09a6ae7735c4b795b078f51c314f14f42a0d63071e13bdc5fd9f51612e77b36d44567502a3b5eb66c609ec017e51d8df93e58d1a44f3c1e375'
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

			md3 = sha512.sum512(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
