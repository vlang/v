// These tests are derived from the Secure Hash Algorithm Validation System
// test vectors contained in:
// https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/shs/shabytetestvectors.zip
//
// For SHA384, the test vectors come from:
//     SHA384Monte.rsp
import crypto.sha512
import encoding.hex

const seed = 'edff07255c71b54a9beae52cdfa083569a08be89949cbba73ddc8acf429359ca5e5be7a673633ca0d9709848f522a9df'

struct MonteTestCase {
	name   string
	count  int
	digest string
}

const monte_cases = [
	MonteTestCase{
		name:   'checkpoint 0'
		count:  0
		digest: 'e81b86c49a38feddfd185f71ca7da6732a053ed4a2640d52d27f53f9f76422650b0e93645301ac99f8295d6f820f1035'
	},
	MonteTestCase{
		name:   'checkpoint 1'
		count:  1
		digest: '1d6bd21713bffd50946a10c39a7742d740e8f271f0c8f643d4c95375094fd9bf29d89ee61a76053f22e44a4b058a64ed'
	},
	MonteTestCase{
		name:   'checkpoint 2'
		count:  2
		digest: '425167b66ae965bd7d68515b54ebfa16f33d2bdb2147a4eac515a75224cd19cea564d692017d2a1c41c1a3f68bb5a209'
	},
	MonteTestCase{
		name:   'checkpoint 3'
		count:  3
		digest: '9e7477ffd4baad1fcca035f4687b35ed47a57832fb27d131eb8018fcb41edf4d5e25874466d2e2d61ae3accdfc7aa364'
	},
	MonteTestCase{
		name:   'checkpoint 4'
		count:  4
		digest: 'd7b4d4e779ca70c8d065630db1f9128ee43b4bde08a81bce13d48659b6ef47b6cfc802af6d8756f6cd43c709bb445bab'
	},
	MonteTestCase{
		name:   'checkpoint 5'
		count:  5
		digest: 'a038eaa91772d458a7339612f6a9214e6550f78d5870d91fa7a3dc1c2321e511759c159a371475dda7ad1fd7f51ac82b'
	},
	MonteTestCase{
		name:   'checkpoint 6'
		count:  6
		digest: '8552b4ea2da9f855fcc7230ac88ecb1c3cb4841cc28e0495bf1ab97d100d4e4c60a1c51949d26723415f560da2951bb0'
	},
	MonteTestCase{
		name:   'checkpoint 7'
		count:  7
		digest: '668338b2e661d9e25b799d7329e55629b62416ee4d42d2e55245ffe7092e7ae4a3da6903944d532c2a88e4632f9a4b36'
	},
	MonteTestCase{
		name:   'checkpoint 8'
		count:  8
		digest: 'c40cd19cb770b1fe81135fcb9223ffb9ef50340d13660658ba60656a88f4ea08ee3b572680e2b4abbaf4392486ea589a'
	},
	MonteTestCase{
		name:   'checkpoint 9'
		count:  9
		digest: '540c154e82b57914abc0452990abf988e46a2f65b40f5e9ef80a38c7431293d5efa31c7c83df5345b079ad4e31b2c8b2'
	},
	MonteTestCase{
		name:   'checkpoint 10'
		count:  10
		digest: 'e112f9359983e7df9b266f53ec950ed2b951b6d832c8fc1ffed22b4be3eae3d015e0771e96d344672000d4959d2829ab'
	},
	MonteTestCase{
		name:   'checkpoint 11'
		count:  11
		digest: '186e69c28033e11b152542c86a94f59a25ec4edb7ab5b735faff16bdbacc682b2ae79b5f5a68ece9fee5ea4530b8a8f4'
	},
	MonteTestCase{
		name:   'checkpoint 12'
		count:  12
		digest: 'a37c790e641f911ef2929bab0ccbb890627c7e38e1a2fbbd71cd4215de919763c2819c35a0e93984b7d85d0e8858573f'
	},
	MonteTestCase{
		name:   'checkpoint 13'
		count:  13
		digest: 'f1b4bdcfe0cce284ca26e0b7e01f26de21bf4a7a409ee99fee1019cc8d23e775bc134ec7987a6575fa64160b5c7f849e'
	},
	MonteTestCase{
		name:   'checkpoint 14'
		count:  14
		digest: '45aabfb784a29d6be30104ae9f3c5daa58e48a9c94f61fed94dccd2ec06b527ed564643573d588ce10d3847361a57961'
	},
	MonteTestCase{
		name:   'checkpoint 15'
		count:  15
		digest: 'be3a49139a48dafba418bb9e0e8e2bd195e0d239eec77c43d1f0cee844b65f65c30cddfc05b644f9a792459695883a2e'
	},
	MonteTestCase{
		name:   'checkpoint 16'
		count:  16
		digest: '3b263277a0d11de4cb4616591d4623e0821da4f111dae33938329cb8cc47f46742c70c09463edb9ff4a0171c4604b243'
	},
	MonteTestCase{
		name:   'checkpoint 17'
		count:  17
		digest: '2be323079381bd13a02cca824c2f3cd18cbc89b0c3319afbf33c6f06f2abf2fa3af35ea9752ffe153d1775a95e7accbd'
	},
	MonteTestCase{
		name:   'checkpoint 18'
		count:  18
		digest: '14c8c3ea3f5aea2e74a2f138863accf1b5f563d96194c0dcf08ca816e1ac9156c3f5d15aa5701d93c188c2c1f7237518'
	},
	MonteTestCase{
		name:   'checkpoint 19'
		count:  19
		digest: 'b9158943803c47678fefafa91c98966aa3dc1fd96f4e86cfdde7ca879dbf9fa9f54b1988a53376f7005df7fd87b1396b'
	},
	MonteTestCase{
		name:   'checkpoint 20'
		count:  20
		digest: 'f4395f88c4dc24b6edca5868fcebd24991ced2e3826c04ec00e4630d7e1f70ee56124a90657a795f446f7db94d8b439e'
	},
	MonteTestCase{
		name:   'checkpoint 21'
		count:  21
		digest: 'bb56b4ed4683fce0d6d412ef84b7985ccc5fe30306679a994c8221c1212ff7f1492512095b7ddd250411008ce9d54020'
	},
	MonteTestCase{
		name:   'checkpoint 22'
		count:  22
		digest: 'cacaeb081a62a4e384e0082f8b994dcd721f38b3c3e4b5836ddcaaab74bcf2700fdc8b6faf3f4df1ec175a0628728a4c'
	},
	MonteTestCase{
		name:   'checkpoint 23'
		count:  23
		digest: '231796f44eba3c8c6ad064b0e350093fcae026c3c03d669c28e5f36befa5f35489bf595a0312c38b9b73b7bb4dad5b96'
	},
	MonteTestCase{
		name:   'checkpoint 24'
		count:  24
		digest: 'd54559e7eb20534025f8f265bdbdab0e7dd9e2459305288d3ca84190b6d2c6e8ae2cd6f3e51ccb54551d94e9e40b31ac'
	},
	MonteTestCase{
		name:   'checkpoint 25'
		count:  25
		digest: 'd6231b809b70ed19478cdf7e05534e92ea01e74e970fe17e92ec9a67b1b7977c4c485cfa74787224fe326b1b8d1ede87'
	},
	MonteTestCase{
		name:   'checkpoint 26'
		count:  26
		digest: '8ca1d462cc1b16b734c0e4f1a6b7f7d9d0ba9ce8074addc7755243b05fe269afab6618f07fe73089d9379bcbdce6c728'
	},
	MonteTestCase{
		name:   'checkpoint 27'
		count:  27
		digest: '60fa725bbf21ee56545ab24e6f26ec5ec749c4971bf761923c23415cbcb0c02e6a474deb50144abad2b0f16ddbe850a9'
	},
	MonteTestCase{
		name:   'checkpoint 28'
		count:  28
		digest: '8b60fe287f5bc133ff842a246bf161a58b4b8ead5be073c702552d664653e384e28e70d28624aa1da951ed33dcdfc7fd'
	},
	MonteTestCase{
		name:   'checkpoint 29'
		count:  29
		digest: 'd18a080af3ae7e6318dc69298bd7b13644b7ab00adac18b446c1e9a9366e68dade87687b9129aefae9a98b531309f1c7'
	},
	MonteTestCase{
		name:   'checkpoint 30'
		count:  30
		digest: 'faad832a7ce865cf3183e51e07f08aad6570795f8d0274fae52fc5cb637ce4b7cdb8f8092e2ac4a7c96beb70cb288b69'
	},
	MonteTestCase{
		name:   'checkpoint 31'
		count:  31
		digest: 'b7bea61748495351409fcf1bd45a94af34e7dd899933ed06d5759fe728f6933781c21f1986b99906c6910d976a0d9c4b'
	},
	MonteTestCase{
		name:   'checkpoint 32'
		count:  32
		digest: 'b35027199cf6467302a88e5d0106d01953b41dfa172234b3e11d3ccad33bda9a836e44f43ae94451e2bea28f0dca7989'
	},
	MonteTestCase{
		name:   'checkpoint 33'
		count:  33
		digest: '8571acd0d5fdd20909e9cd74e125cb9cd65a2e74056eab3f85f6f13e31a5fc1580aa588997d31a0a3ba1a16dea5528d3'
	},
	MonteTestCase{
		name:   'checkpoint 34'
		count:  34
		digest: '3dcf49cb8d79d4ca3f6e06e8a58bb92a7a42d7915b9e710a29d37ece6c1c32eb89a897e9935354db3cbe384a1149964a'
	},
	MonteTestCase{
		name:   'checkpoint 35'
		count:  35
		digest: '7b2258c5fbf26780f55054b4aa462a607eecbf7382af941efd75b9b4cadd5b97936a762b9c03e133d7cfb65de501e6df'
	},
	MonteTestCase{
		name:   'checkpoint 36'
		count:  36
		digest: 'c54cfebd2381621fed5cf6b82b3dea25965ec99365ce415b184ded71b949eb8009d91c85c02b30b7465fdcf18be885ca'
	},
	MonteTestCase{
		name:   'checkpoint 37'
		count:  37
		digest: '39bed6de0e885dfd51569f3c5dc967f7f551065e87b6e7108d15ef5ea407419f936f5ae2288aeecf98777249f384fdfa'
	},
	MonteTestCase{
		name:   'checkpoint 38'
		count:  38
		digest: 'b1c341b1428e5df6f4bcec533f1b5ca57e02102f5647b82be0986e523fad24b7f27aa29336a3f333817e8a5336a4a3b4'
	},
	MonteTestCase{
		name:   'checkpoint 39'
		count:  39
		digest: '2e1e67546b9424a2f0bd8931082f9fb8951b9fe57a2b61683a5e197017ebcd96592dc47a75d2ae4ab8f436edd5e5bb4e'
	},
	MonteTestCase{
		name:   'checkpoint 40'
		count:  40
		digest: '6c84a39bc94af5960715818bf9b4694548dd1f0af8f6344d56b0dc7f86b181d5249172c82572ec8748ff35b6c0a2abd5'
	},
	MonteTestCase{
		name:   'checkpoint 41'
		count:  41
		digest: '576705bec035d07e31ebb091f180cd68c3873ea306708c5259f50491463c68d912080ba9f11bcc983a4b849ca19df008'
	},
	MonteTestCase{
		name:   'checkpoint 42'
		count:  42
		digest: 'd45c7e9080b6223a2185c490363764f9fb0634f3865d57a15bea438fb243e98fccad4176bf24c4cb7247dd2c5728b761'
	},
	MonteTestCase{
		name:   'checkpoint 43'
		count:  43
		digest: 'b3021cedd2ba38b69348867729cfe2ab172e4f1643eb4971cde2db002413458a566ea884d651a9c010b1a6b869168497'
	},
	MonteTestCase{
		name:   'checkpoint 44'
		count:  44
		digest: '1cb9c05e35029fe6b114c85a457091b7d9aaf7c95f32447f3f20cd034bc54f87ae85c4013d18fe2a94de8ecf9c6b9f05'
	},
	MonteTestCase{
		name:   'checkpoint 45'
		count:  45
		digest: '1e8f037fb920b836b8f36a1ed4875cf7d61390f68d4843e420b2c1ca702a104524c1187c8eec7bb4b174a252e1ae1462'
	},
	MonteTestCase{
		name:   'checkpoint 46'
		count:  46
		digest: '07c790c3d4948347ad5fad9992d8a0f6603a2133d138f1ff5cbbdc04c39277fb67d45b2e2c8e6c51fdaa6c5883e3a69c'
	},
	MonteTestCase{
		name:   'checkpoint 47'
		count:  47
		digest: '98ef381d9b6b4e26ebf2bc293743e1e07943a3663b17f1be52d12ef8d19621263efb8525506ef6b95f746567a43577c6'
	},
	MonteTestCase{
		name:   'checkpoint 48'
		count:  48
		digest: '8beefea2f858f8902928dae6060b10cf6d4a3cd1f91cf1ee5ddef0ee5fc25a8269367c114c1c5c5ab5287c48edc59274'
	},
	MonteTestCase{
		name:   'checkpoint 49'
		count:  49
		digest: '03a6509ad6eb7f009931e596f3dab586de3bd6549afab4f218eba4fe47daf37c6faa360afdd931c5a95544f1a028195b'
	},
	MonteTestCase{
		name:   'checkpoint 50'
		count:  50
		digest: 'bdf7e8d538e7ef418c808bf2dec1242b716326f83bf0a53db81f4d63aba37a2412f6b2bf00957ad6faf8404e4e2067a8'
	},
	MonteTestCase{
		name:   'checkpoint 51'
		count:  51
		digest: '71ad43312ed6e403a5e174480fb14b3c2a3a60bba36611e1c99adf8013d243fe945b947b362b6dc51a3dd96235472f5e'
	},
	MonteTestCase{
		name:   'checkpoint 52'
		count:  52
		digest: 'bec45a229217e5fe28d6d1675dc7440ccb5616c0f02eb5d4c814921fc82fdddb04592ce7af192fee6c61b1a08f6ab6aa'
	},
	MonteTestCase{
		name:   'checkpoint 53'
		count:  53
		digest: '5d6bc2d2cd0d8f5e41656f73a473bef1eff212fe98b184386cdecade24c5c5e7b92cf76ea16f582b9951634881f85585'
	},
	MonteTestCase{
		name:   'checkpoint 54'
		count:  54
		digest: '96e3568d30a1f7810404f3be8d2d26e5606da6c3fc064d0ef62298ca327476d587a1e3ef0d6554f4ad529053b7a651fd'
	},
	MonteTestCase{
		name:   'checkpoint 55'
		count:  55
		digest: 'e4df3b7028f5e68753c1f21a556d8468a5d80fb048f6b92d405e519ea9ce44c6c95fb362119c553f5921dc9616dd3937'
	},
	MonteTestCase{
		name:   'checkpoint 56'
		count:  56
		digest: '7881e36eef708df4d9c06e37bfa7af5ce7d41d31a2e4332d23922a518de3e2e6fd4b9f27ee64379afae7ca2570d24748'
	},
	MonteTestCase{
		name:   'checkpoint 57'
		count:  57
		digest: 'aa12548b63d3a58f6914744111292cfc1f1358f717f1b19fab1a4ecac6292f1ff4b4c67b8a260b048f00ddc83b42453a'
	},
	MonteTestCase{
		name:   'checkpoint 58'
		count:  58
		digest: 'b421b6941b5d7748765a4090c224dbbd98e85dcff9a65a77db0c2a83b92f4cad961b5b8ff76b5513d4a7af45ec4d4550'
	},
	MonteTestCase{
		name:   'checkpoint 59'
		count:  59
		digest: '14e1b1733b16899c4046a604f8e1e777d55649c5357d7d9e3d7a1c395b6275aecf733a598de1d0bfd7eeaa9ecbd7d1e7'
	},
	MonteTestCase{
		name:   'checkpoint 60'
		count:  60
		digest: 'bd05a1f9fa5b77371005a8073f0f3bcc4cb4e08fde3335dd3688921cf9cb5e97cf1b3052ff74bed8a359d170d2bea48f'
	},
	MonteTestCase{
		name:   'checkpoint 61'
		count:  61
		digest: 'a319d9b3eeee6da494940ffb08903bcd588b18733a64ed435eadc5d9add6ab4f0c35fc050958bed81303409f388a065e'
	},
	MonteTestCase{
		name:   'checkpoint 62'
		count:  62
		digest: '6f19824ec874b55e88fe4b1387433dab85415148870bf4a0612aa9c1cbcd9627925616fcdb66d68760c50fb308f628b0'
	},
	MonteTestCase{
		name:   'checkpoint 63'
		count:  63
		digest: 'bacb435a1ff538d0ba3e3d0ab04b5e8868bc1f84e964409229d7eada4b846b813c0e30d8e962786aa83aac2dacf02d19'
	},
	MonteTestCase{
		name:   'checkpoint 64'
		count:  64
		digest: 'e8f013470eafd5af84f63d51d51af2ca884789d03d79f8c3089810254b95a6f54fb86c08202cae94681ad702ea29451a'
	},
	MonteTestCase{
		name:   'checkpoint 65'
		count:  65
		digest: '374b07621c018cc3935374c2f2f098e661ca0656181f67f55fb80ac36e23da379c4f6c8a3683c2621f874afa1241b918'
	},
	MonteTestCase{
		name:   'checkpoint 66'
		count:  66
		digest: '5878f1ebcbe60aa62a7b149bd181167e5898d08a3627a08c589436f007bfb82c040b26ea9944c6f0c9c4079b9b0e1ecf'
	},
	MonteTestCase{
		name:   'checkpoint 67'
		count:  67
		digest: '05961b57507c99d0cb7dc24ae34eddde94ac484129de621edac5b001ac5c0b974d09d24f75504f3be1a3cd635c44bf71'
	},
	MonteTestCase{
		name:   'checkpoint 68'
		count:  68
		digest: 'd961eb883eecbc083533fa5128695c8d28281fbac23308dd2f504eb079d2d311b973f1a52b45aa6275550e14477a8876'
	},
	MonteTestCase{
		name:   'checkpoint 69'
		count:  69
		digest: 'a4557f990f4ccce585ba33453090f66af576f0a501d26667031f48f19538b820b84f870579efb554e7550f9f53fea5ea'
	},
	MonteTestCase{
		name:   'checkpoint 70'
		count:  70
		digest: '82194c49f24084249567f0e8963c5f72a23bc20a8f522a6108f12abf95b7437ad93673860a953264838a09bb3968d0a9'
	},
	MonteTestCase{
		name:   'checkpoint 71'
		count:  71
		digest: '371dc5573b145f2136eb854591ece253efebf8732d3898bea063fdc3889d07953ee646e533b214f8c2dd66f1355b03cf'
	},
	MonteTestCase{
		name:   'checkpoint 72'
		count:  72
		digest: 'cfc4dcecc6103027232029dd9a19850a6f79b9004be7d70054d0af11c692affa44c537f7cd749f2b6317cafe1fa52fe4'
	},
	MonteTestCase{
		name:   'checkpoint 73'
		count:  73
		digest: '9920b835400795bd3ed8ae0bc12417d58be8c5ffd6eed151ed738c3031e624c74fb85488953ac81c75f395cab74f1679'
	},
	MonteTestCase{
		name:   'checkpoint 74'
		count:  74
		digest: '4a1b040fa38b5cee63f5d308b55502d2a017b349ead5172c288289f42ba9874d0d11c9ac43255580c428a99067495782'
	},
	MonteTestCase{
		name:   'checkpoint 75'
		count:  75
		digest: '93c1cb94d0689301728165299057edd78ef48a6dc7654931ae2bf7ea5bc733f3b724f4c3081bc93ed61e7d739c38e137'
	},
	MonteTestCase{
		name:   'checkpoint 76'
		count:  76
		digest: '8f2e6c868b224b3cd5ac80669da0ba1d7e799e85a124c9e81c6865ebab1c0481e4ed4957a8989902ec565169ac53b7b4'
	},
	MonteTestCase{
		name:   'checkpoint 77'
		count:  77
		digest: 'df045c9302fba73f9f27ceb0fb70e6ca3897f410e81a2b8392489e40aa17f15ac59cf8d6893ab10bacd8b59704eab22b'
	},
	MonteTestCase{
		name:   'checkpoint 78'
		count:  78
		digest: '8ab095f49aa7ebfcb8b1410f42c38fb1755a0560e3638b8b82c7a852e8bce8f4b780015e051dda0d2cbd6d6cb08e30c0'
	},
	MonteTestCase{
		name:   'checkpoint 79'
		count:  79
		digest: 'd77fdff2f768188efa63a7e29d73b8ade14c1aeb12e77866a57ea12c81bf0b3e1421d1af57fccf91b2098ba02ffb4118'
	},
	MonteTestCase{
		name:   'checkpoint 80'
		count:  80
		digest: 'fac5ee7450b3fd1ae2152f5d020680137f553a2c210c57290d058f330d11407593d74c9d3d9ac88bf4af44e023345168'
	},
	MonteTestCase{
		name:   'checkpoint 81'
		count:  81
		digest: '39d0ee95db114925ed7ff1577a22eb3dedb8658ce31504bd0f9f8a8f11f90825587203f26c432d216918156ca931fc82'
	},
	MonteTestCase{
		name:   'checkpoint 82'
		count:  82
		digest: '17d22080e8a9f589a80a5ca8291b0479c41351008dffff79ff522779c35ba0b09acc2dedde936b07e260451d35ce86a9'
	},
	MonteTestCase{
		name:   'checkpoint 83'
		count:  83
		digest: '9d75befac42e6d4d544e70477f7581264b5f8dda988da0dc40ef32f85c31b709284aef5f4f0246d20a855eee9175948e'
	},
	MonteTestCase{
		name:   'checkpoint 84'
		count:  84
		digest: '3985f0cbca4c25f624850580516184e3d75996d77f138839c7570b4539b90fae8e751d1cea642816abd7f9ebf9d86c8f'
	},
	MonteTestCase{
		name:   'checkpoint 85'
		count:  85
		digest: '93cfd14ade34e50deeee23aa75a63a017b6974e23051117e7e6b56b4ddb88f917a5d88d3af2af27da8e63fe130502f8e'
	},
	MonteTestCase{
		name:   'checkpoint 86'
		count:  86
		digest: '7ae513480491a9500fa9afb8f64b8914ffdbeece3b3103048e91f6510b64cdccc8273257e275e5b34ec14c4c4aff4405'
	},
	MonteTestCase{
		name:   'checkpoint 87'
		count:  87
		digest: '2585bab1cd98d4b51a1475fbfbe3bca43da2a7be842c5667c98d3b62a9f05918108be94198d96c67388f83c2abebe498'
	},
	MonteTestCase{
		name:   'checkpoint 88'
		count:  88
		digest: '32f67419616e11fea79e3baeee4524c58d09f0cfb42049cea70f9a4a74e0096df841a0cf5177e402dd5803f4b51c602c'
	},
	MonteTestCase{
		name:   'checkpoint 89'
		count:  89
		digest: '9f4486d93c599e68e7463d07d5cc9d589ab3a7c3d4d3c2b2d1f81f65b5c85068331f4142215f337c3621d096eb36aa91'
	},
	MonteTestCase{
		name:   'checkpoint 90'
		count:  90
		digest: 'e3399ed2ac93c6a4a6c88c11bd89655aac3e573493483c81631fd67dba3bb237d46f9e8ddab3a9fd78236296d00dfd79'
	},
	MonteTestCase{
		name:   'checkpoint 91'
		count:  91
		digest: 'bd9d1de114afa5ffacfbeb488d4846d012aa6ef66ce09725ae7b15e680d719fc2447f308eeb8247ae8e91e34b5a21ea2'
	},
	MonteTestCase{
		name:   'checkpoint 92'
		count:  92
		digest: 'e1c3511ed2ed26f770bf5212c7ec245ab2ba49e1c09edae2abad6a3ee41c9e25445f5e5317cf7c9c3c3f702ecd6778a5'
	},
	MonteTestCase{
		name:   'checkpoint 93'
		count:  93
		digest: 'c363234d1a6272d081f351cd68ac90abea09d3eae3a4d64fae7fab251a252591cb34dc63fb10abcbc5460129464c868b'
	},
	MonteTestCase{
		name:   'checkpoint 94'
		count:  94
		digest: '6e5f1531eb282a2911a64b72b043cfe43b527d4d557abb9a31a9a632cdf5b5e055317ecb72a517a025eb4286d6f00433'
	},
	MonteTestCase{
		name:   'checkpoint 95'
		count:  95
		digest: '19c85253b3c703fee80a70bb2ac2ef836bf8e14464d2a17f35bd5e4f2b0b3a059a27891410950a9ce07197f5b306ae3f'
	},
	MonteTestCase{
		name:   'checkpoint 96'
		count:  96
		digest: 'eec713a44cb778811795609610f2f9bfc9bba479e415746efe0dc530b6de66d73cb85b8698a8c0c2ef9344a2043b7a31'
	},
	MonteTestCase{
		name:   'checkpoint 97'
		count:  97
		digest: 'b799577aab5e8898326ed88eb96de38a27e76280ce44c3f16e70a1f96543ee7020bc29913ea0b9a9b9d92ae201143e0b'
	},
	MonteTestCase{
		name:   'checkpoint 98'
		count:  98
		digest: 'e4dcabf1e22134076a21ea7cf9be6e03b099be40efc2b3080b9ec358cb021623ad27d33129bc68fce3eaec6b25aa2329'
	},
	MonteTestCase{
		name:   'checkpoint 99'
		count:  99
		digest: 'ccde4359f23e64579c5c0380df837ee950928aa82937a2d2ed33d216e707c46d847efa5ca52dcbda551145e164fbd594'
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

			md3 = sha512.sum384(mi)

			md0 = md1.clone()
			md1 = md2.clone()
			md2 = md3.clone()
		}

		msg_seed = md3.clone()

		expected_result := hex.decode(c.digest)!

		assert md3 == expected_result, 'failed ${c.name}'
	}
}
