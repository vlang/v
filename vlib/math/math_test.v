module math

struct Fi {
	f f64
	i int
}

const (
	vf_          = [f64(4.9790119248836735e+00), 7.7388724745781045e+00, -2.7688005719200159e-01,
		-5.0106036182710749e+00, 9.6362937071984173e+00, 2.9263772392439646e+00,
		5.2290834314593066e+00, 2.7279399104360102e+00, 1.8253080916808550e+00,
		-8.6859247685756013e+00]
	// The expected results below were computed by the high precision calculators
	// at https://keisan.casio.com/.  More exact input values (array vf_[], above)
	// were obtained by printing them with "%.26f".  The answers were calculated
	// to 26 digits (by using the "Digit number" drop-down control of each
	// calculator).
	acos_        = [f64(1.0496193546107222142571536e+00), 6.8584012813664425171660692e-01,
		1.5984878714577160325521819e+00, 2.0956199361475859327461799e+00,
		2.7053008467824138592616927e-01, 1.2738121680361776018155625e+00,
		1.0205369421140629186287407e+00, 1.2945003481781246062157835e+00,
		1.3872364345374451433846657e+00, 2.6231510803970463967294145e+00]
	acosh_       = [f64(2.4743347004159012494457618e+00), 2.8576385344292769649802701e+00,
		7.2796961502981066190593175e-01, 2.4796794418831451156471977e+00,
		3.0552020742306061857212962e+00, 2.044238592688586588942468e+00,
		2.5158701513104513595766636e+00, 1.99050839282411638174299e+00,
		1.6988625798424034227205445e+00, 2.9611454842470387925531875e+00]
	asin_        = [f64(5.2117697218417440497416805e-01), 8.8495619865825236751471477e-01,
		-2.769154466281941332086016e-02, -5.2482360935268931351485822e-01,
		1.3002662421166552333051524e+00, 2.9698415875871901741575922e-01,
		5.5025938468083370060258102e-01, 2.7629597861677201301553823e-01,
		1.83559892257451475846656e-01, -1.0523547536021497774980928e+00]
	asinh_       = [f64(2.3083139124923523427628243e+00), 2.743551594301593620039021e+00,
		-2.7345908534880091229413487e-01, -2.3145157644718338650499085e+00,
		2.9613652154015058521951083e+00, 1.7949041616585821933067568e+00,
		2.3564032905983506405561554e+00, 1.7287118790768438878045346e+00,
		1.3626658083714826013073193e+00, -2.8581483626513914445234004e+00]
	atan_        = [f64(1.372590262129621651920085e+00), 1.442290609645298083020664e+00,
		-2.7011324359471758245192595e-01, -1.3738077684543379452781531e+00,
		1.4673921193587666049154681e+00, 1.2415173565870168649117764e+00,
		1.3818396865615168979966498e+00, 1.2194305844639670701091426e+00,
		1.0696031952318783760193244e+00, -1.4561721938838084990898679e+00]
	atanh_       = [f64(5.4651163712251938116878204e-01), 1.0299474112843111224914709e+00,
		-2.7695084420740135145234906e-02, -5.5072096119207195480202529e-01,
		1.9943940993171843235906642e+00, 3.01448604578089708203017e-01,
		5.8033427206942188834370595e-01, 2.7987997499441511013958297e-01,
		1.8459947964298794318714228e-01, -1.3273186910532645867272502e+00]
	atan2_       = [f64(1.1088291730037004444527075e+00), 9.1218183188715804018797795e-01,
		1.5984772603216203736068915e+00, 2.0352918654092086637227327e+00,
		8.0391819139044720267356014e-01, 1.2861075249894661588866752e+00,
		1.0889904479131695712182587e+00, 1.3044821793397925293797357e+00,
		1.3902530903455392306872261e+00, 2.2859857424479142655411058e+00]
	ceil_        = [f64(5.0000000000000000e+00), 8.0000000000000000e+00, copysign(0, -1),
		-5.0000000000000000e+00, 1.0000000000000000e+01, 3.0000000000000000e+00,
		6.0000000000000000e+00, 3.0000000000000000e+00, 2.0000000000000000e+00,
		-8.0000000000000000e+00]
	cos_         = [f64(2.634752140995199110787593e-01), 1.148551260848219865642039e-01,
		9.6191297325640768154550453e-01, 2.938141150061714816890637e-01,
		-9.777138189897924126294461e-01, -9.7693041344303219127199518e-01,
		4.940088096948647263961162e-01, -9.1565869021018925545016502e-01,
		-2.517729313893103197176091e-01, -7.39241351595676573201918e-01]
	// Results for 100000 * pi + vf_[i]
	cos_large_   = [f64(2.634752141185559426744e-01), 1.14855126055543100712e-01,
		9.61912973266488928113e-01, 2.9381411499556122552e-01, -9.777138189880161924641e-01,
		-9.76930413445147608049e-01, 4.940088097314976789841e-01, -9.15658690217517835002e-01,
		-2.51772931436786954751e-01, -7.3924135157173099849e-01]
	cosh_        = [f64(7.2668796942212842775517446e+01), 1.1479413465659254502011135e+03,
		1.0385767908766418550935495e+00, 7.5000957789658051428857788e+01,
		7.655246669605357888468613e+03, 9.3567491758321272072888257e+00,
		9.331351599270605471131735e+01, 7.6833430994624643209296404e+00,
		3.1829371625150718153881164e+00, 2.9595059261916188501640911e+03]
	exp_         = [f64(1.4533071302642137507696589e+02), 2.2958822575694449002537581e+03,
		7.5814542574851666582042306e-01, 6.6668778421791005061482264e-03,
		1.5310493273896033740861206e+04, 1.8659907517999328638667732e+01,
		1.8662167355098714543942057e+02, 1.5301332413189378961665788e+01,
		6.2047063430646876349125085e+00, 1.6894712385826521111610438e-04]
	expm1_       = [f64(5.105047796122957327384770212e-02), 8.046199708567344080562675439e-02,
		-2.764970978891639815187418703e-03, -4.8871434888875355394330300273e-02,
		1.0115864277221467777117227494e-01, 2.969616407795910726014621657e-02,
		5.368214487944892300914037972e-02, 2.765488851131274068067445335e-02,
		1.842068661871398836913874273e-02, -8.3193870863553801814961137573e-02]
	expm1_large_ = [f64(4.2031418113550844e+21), 4.0690789717473863e+33, -0.9372627915981363e+00,
		-1.0, 7.077694784145933e+41, 5.117936223839153e+12, 5.124137759001189e+22,
		7.03546003972584e+11, 8.456921800389698e+07, -1.0]
	exp2_        = [f64(3.1537839463286288034313104e+01), 2.1361549283756232296144849e+02,
		8.2537402562185562902577219e-01, 3.1021158628740294833424229e-02,
		7.9581744110252191462569661e+02, 7.6019905892596359262696423e+00,
		3.7506882048388096973183084e+01, 6.6250893439173561733216375e+00,
		3.5438267900243941544605339e+00, 2.4281533133513300984289196e-03]
	fabs_        = [f64(4.9790119248836735e+00), 7.7388724745781045e+00, 2.7688005719200159e-01,
		5.0106036182710749e+00, 9.6362937071984173e+00, 2.9263772392439646e+00,
		5.2290834314593066e+00, 2.7279399104360102e+00, 1.8253080916808550e+00,
		8.6859247685756013e+00]
	floor_       = [f64(4.0000000000000000e+00), 7.0000000000000000e+00, -1.0000000000000000e+00,
		-6.0000000000000000e+00, 9.0000000000000000e+00, 2.0000000000000000e+00,
		5.0000000000000000e+00, 2.0000000000000000e+00, 1.0000000000000000e+00,
		-9.0000000000000000e+00]
	fmod_        = [f64(4.197615023265299782906368e-02), 2.261127525421895434476482e+00,
		3.231794108794261433104108e-02, 4.989396381728925078391512e+00,
		3.637062928015826201999516e-01, 1.220868282268106064236690e+00,
		4.770916568540693347699744e+00, 1.816180268691969246219742e+00,
		8.734595415957246977711748e-01, 1.314075231424398637614104e+00]
	frexp_       = [Fi{6.2237649061045918750e-01, 3}, Fi{9.6735905932226306250e-01, 3},
		Fi{-5.5376011438400318000e-01, -1}, Fi{-6.2632545228388436250e-01, 3},
		Fi{6.02268356699901081250e-01, 4}, Fi{7.3159430981099115000e-01, 2},
		Fi{6.5363542893241332500e-01, 3}, Fi{6.8198497760900255000e-01, 2},
		Fi{9.1265404584042750000e-01, 1}, Fi{-5.4287029803597508250e-01, 4}]
	gamma_       = [f64(2.3254348370739963835386613898e+01), 2.991153837155317076427529816e+03,
		-4.561154336726758060575129109e+00, 7.719403468842639065959210984e-01,
		1.6111876618855418534325755566e+05, 1.8706575145216421164173224946e+00,
		3.4082787447257502836734201635e+01, 1.579733951448952054898583387e+00,
		9.3834586598354592860187267089e-01, -2.093995902923148389186189429e-05]
	log_gamma_   = [Fi{3.146492141244545774319734e+00, 1}, Fi{8.003414490659126375852113e+00, 1},
		Fi{1.517575735509779707488106e+00, -1}, Fi{-2.588480028182145853558748e-01, 1},
		Fi{1.1989897050205555002007985e+01, 1}, Fi{6.262899811091257519386906e-01, 1},
		Fi{3.5287924899091566764846037e+00, 1}, Fi{4.5725644770161182299423372e-01, 1},
		Fi{-6.363667087767961257654854e-02, 1}, Fi{-1.077385130910300066425564e+01, -1}]
	log_         = [f64(1.605231462693062999102599e+00), 2.0462560018708770653153909e+00,
		-1.2841708730962657801275038e+00, 1.6115563905281545116286206e+00,
		2.2655365644872016636317461e+00, 1.0737652208918379856272735e+00,
		1.6542360106073546632707956e+00, 1.0035467127723465801264487e+00,
		6.0174879014578057187016475e-01, 2.161703872847352815363655e+00]
	logb_        = [f64(2.0000000000000000e+00), 2.0000000000000000e+00, -2.0000000000000000e+00,
		2.0000000000000000e+00, 3.0000000000000000e+00, 1.0000000000000000e+00,
		2.0000000000000000e+00, 1.0000000000000000e+00, 0.0000000000000000e+00,
		3.0000000000000000e+00]
	log10_       = [f64(6.9714316642508290997617083e-01), 8.886776901739320576279124e-01,
		-5.5770832400658929815908236e-01, 6.998900476822994346229723e-01,
		9.8391002850684232013281033e-01, 4.6633031029295153334285302e-01,
		7.1842557117242328821552533e-01, 4.3583479968917773161304553e-01,
		2.6133617905227038228626834e-01, 9.3881606348649405716214241e-01]
	log1p_       = [f64(4.8590257759797794104158205e-02), 7.4540265965225865330849141e-02,
		-2.7726407903942672823234024e-03, -5.1404917651627649094953380e-02,
		9.1998280672258624681335010e-02, 2.8843762576593352865894824e-02,
		5.0969534581863707268992645e-02, 2.6913947602193238458458594e-02,
		1.8088493239630770262045333e-02, -9.0865245631588989681559268e-02]
	log2_        = [f64(2.3158594707062190618898251e+00), 2.9521233862883917703341018e+00,
		-1.8526669502700329984917062e+00, 2.3249844127278861543568029e+00,
		3.268478366538305087466309e+00, 1.5491157592596970278166492e+00,
		2.3865580889631732407886495e+00, 1.447811865817085365540347e+00,
		8.6813999540425116282815557e-01, 3.118679457227342224364709e+00]
	modf_        = [[f64(4.0000000000000000e+00), 9.7901192488367350108546816e-01],
		[f64(7.0000000000000000e+00), 7.3887247457810456552351752e-01],
		[f64(-0.0), -2.7688005719200159404635997e-01],
		[f64(-5.0000000000000000e+00),
			-1.060361827107492160848778e-02],
		[f64(9.0000000000000000e+00), 6.3629370719841737980004837e-01],
		[f64(2.0000000000000000e+00), 9.2637723924396464525443662e-01],
		[f64(5.0000000000000000e+00), 2.2908343145930665230025625e-01],
		[f64(2.0000000000000000e+00), 7.2793991043601025126008608e-01],
		[f64(1.0000000000000000e+00), 8.2530809168085506044576505e-01],
		[f64(-8.0000000000000000e+00), -6.8592476857560136238589621e-01]]
	nextafter32_ = [4.979012489318848e+00, 7.738873004913330e+00, -2.768800258636475e-01,
		-5.010602951049805e+00, 9.636294364929199e+00, 2.926377534866333e+00, 5.229084014892578e+00,
		2.727940082550049e+00, 1.825308203697205e+00, -8.685923576354980e+00]
	nextafter64_ = [f64(4.97901192488367438926388786e+00), 7.73887247457810545370193722e+00,
		-2.7688005719200153853520874e-01, -5.01060361827107403343006808e+00,
		9.63629370719841915615688777e+00, 2.92637723924396508934364647e+00,
		5.22908343145930754047867595e+00, 2.72793991043601069534929593e+00,
		1.82530809168085528249036997e+00, -8.68592476857559958602905681e+00]
	pow_         = [f64(9.5282232631648411840742957e+04), 5.4811599352999901232411871e+07,
		5.2859121715894396531132279e-01, 9.7587991957286474464259698e-06,
		4.328064329346044846740467e+09, 8.4406761805034547437659092e+02,
		1.6946633276191194947742146e+05, 5.3449040147551939075312879e+02,
		6.688182138451414936380374e+01, 2.0609869004248742886827439e-09]
	remainder_   = [f64(4.197615023265299782906368e-02), 2.261127525421895434476482e+00,
		3.231794108794261433104108e-02, -2.120723654214984321697556e-02,
		3.637062928015826201999516e-01, 1.220868282268106064236690e+00,
		-4.581668629186133046005125e-01, -9.117596417440410050403443e-01,
		8.734595415957246977711748e-01, 1.314075231424398637614104e+00]
	round_       = [f64(5), 8, copysign(0, -1), -5, 10, 3, 5, 3, 2, -9]
	signbit_     = [false, false, true, true, false, false, false, false, false, true]
	sin_         = [f64(-9.6466616586009283766724726e-01), 9.9338225271646545763467022e-01,
		-2.7335587039794393342449301e-01, 9.5586257685042792878173752e-01,
		-2.099421066779969164496634e-01, 2.135578780799860532750616e-01,
		-8.694568971167362743327708e-01, 4.019566681155577786649878e-01,
		9.6778633541687993721617774e-01, -6.734405869050344734943028e-01]
	// Results for 100000 * pi + vf_[i]
	sin_large_   = [f64(-9.646661658548936063912e-01), 9.933822527198506903752e-01,
		-2.7335587036246899796e-01, 9.55862576853689321268e-01, -2.099421066862688873691e-01,
		2.13557878070308981163e-01, -8.694568970959221300497e-01, 4.01956668098863248917e-01,
		9.67786335404528727927e-01, -6.7344058693131973066e-01]
	sinh_        = [f64(7.2661916084208532301448439e+01), 1.1479409110035194500526446e+03,
		-2.8043136512812518927312641e-01, -7.499429091181587232835164e+01,
		7.6552466042906758523925934e+03, 9.3031583421672014313789064e+00,
		9.330815755828109072810322e+01, 7.6179893137269146407361477e+00,
		3.021769180549615819524392e+00, -2.95950575724449499189888e+03]
	sqrt_        = [f64(2.2313699659365484748756904e+00), 2.7818829009464263511285458e+00,
		5.2619393496314796848143251e-01, 2.2384377628763938724244104e+00,
		3.1042380236055381099288487e+00, 1.7106657298385224403917771e+00,
		2.286718922705479046148059e+00, 1.6516476350711159636222979e+00,
		1.3510396336454586262419247e+00, 2.9471892997524949215723329e+00]
	tan_         = [f64(-3.661316565040227801781974e+00), 8.64900232648597589369854e+00,
		-2.8417941955033612725238097e-01, 3.253290185974728640827156e+00,
		2.147275640380293804770778e-01, -2.18600910711067004921551e-01,
		-1.760002817872367935518928e+00, -4.389808914752818126249079e-01,
		-3.843885560201130679995041e+00, 9.10988793377685105753416e-01]
	// Results for 100000 * pi + vf_[i]
	tan_large_   = [f64(-3.66131656475596512705e+00), 8.6490023287202547927e+00,
		-2.841794195104782406e-01, 3.2532901861033120983e+00, 2.14727564046880001365e-01,
		-2.18600910700688062874e-01, -1.760002817699722747043e+00, -4.38980891453536115952e-01,
		-3.84388555942723509071e+00, 9.1098879344275101051e-01]
	tanh_        = [f64(9.9990531206936338549262119e-01), 9.9999962057085294197613294e-01,
		-2.7001505097318677233756845e-01, -9.9991110943061718603541401e-01,
		9.9999999146798465745022007e-01, 9.9427249436125236705001048e-01,
		9.9994257600983138572705076e-01, 9.9149409509772875982054701e-01,
		9.4936501296239685514466577e-01, -9.9999994291374030946055701e-01]
	trunc_       = [f64(4.0000000000000000e+00), 7.0000000000000000e+00, copysign(0, -1),
		-5.0000000000000000e+00, 9.0000000000000000e+00, 2.0000000000000000e+00,
		5.0000000000000000e+00, 2.0000000000000000e+00, 1.0000000000000000e+00,
		-8.0000000000000000e+00]
)

fn soclose(a f64, b f64, e_ f64) bool {
	return tolerance(a, b, e_)
}

fn test_nan() {
	nan_f64 := nan()
	assert nan_f64 != nan_f64
	nan_f32 := f32(nan_f64)
	assert nan_f32 != nan_f32
}

fn test_angle_diff() {
	for pair in [
		[pi, pi_2, -pi_2],
		[pi_2 * 3.0, pi_2, -pi],
		[pi / 6.0, two_thirds * pi, pi_2],
	] {
		assert angle_diff(pair[0], pair[1]) == pair[2]
	}
}

fn test_acos() {
	for i := 0; i < math.vf_.len; i++ {
		a := math.vf_[i] / 10
		f := acos(a)
		assert soclose(math.acos_[i], f, 1e-7)
	}
	vfacos_sc_ := [-pi, 1, pi, nan()]
	acos_sc_ := [nan(), 0, nan(), nan()]
	for i := 0; i < vfacos_sc_.len; i++ {
		f := acos(vfacos_sc_[i])
		assert alike(acos_sc_[i], f)
	}
}

fn test_acosh() {
	for i := 0; i < math.vf_.len; i++ {
		a := 1.0 + abs(math.vf_[i])
		f := acosh(a)
		assert veryclose(math.acosh_[i], f)
	}
	vfacosh_sc_ := [inf(-1), 0.5, 1, inf(1), nan()]
	acosh_sc_ := [nan(), nan(), 0, inf(1), nan()]
	for i := 0; i < vfacosh_sc_.len; i++ {
		f := acosh(vfacosh_sc_[i])
		assert alike(acosh_sc_[i], f)
	}
}

fn test_asin() {
	for i := 0; i < math.vf_.len; i++ {
		a := math.vf_[i] / 10
		f := asin(a)
		assert veryclose(math.asin_[i], f)
	}
	vfasin_sc_ := [-pi, copysign(0, -1), 0, pi, nan()]
	asin_sc_ := [nan(), copysign(0, -1), 0, nan(), nan()]
	for i := 0; i < vfasin_sc_.len; i++ {
		f := asin(vfasin_sc_[i])
		assert alike(asin_sc_[i], f)
	}
}

fn test_asinh() {
	for i := 0; i < math.vf_.len; i++ {
		f := asinh(math.vf_[i])
		assert veryclose(math.asinh_[i], f)
	}
	vfasinh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	asinh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfasinh_sc_.len; i++ {
		f := asinh(vfasinh_sc_[i])
		assert alike(asinh_sc_[i], f)
	}
}

fn test_atan() {
	for i := 0; i < math.vf_.len; i++ {
		f := atan(math.vf_[i])
		assert veryclose(math.atan_[i], f)
	}
	vfatan_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	atan_sc_ := [f64(-pi / 2), copysign(0, -1), 0, pi / 2, nan()]
	for i := 0; i < vfatan_sc_.len; i++ {
		f := atan(vfatan_sc_[i])
		assert alike(atan_sc_[i], f)
	}
}

fn test_atanh() {
	for i := 0; i < math.vf_.len; i++ {
		a := math.vf_[i] / 10
		f := atanh(a)
		assert veryclose(math.atanh_[i], f)
	}
	vfatanh_sc_ := [inf(-1), -pi, -1, copysign(0, -1), 0, 1, pi, inf(1),
		nan()]
	atanh_sc_ := [nan(), nan(), inf(-1), copysign(0, -1), 0, inf(1),
		nan(), nan(), nan()]
	for i := 0; i < vfatanh_sc_.len; i++ {
		f := atanh(vfatanh_sc_[i])
		assert alike(atanh_sc_[i], f)
	}
}

fn test_atan2() {
	for i := 0; i < math.vf_.len; i++ {
		f := atan2(10, math.vf_[i])
		assert veryclose(math.atan2_[i], f)
	}
	vfatan2_sc_ := [[inf(-1), inf(-1)], [inf(-1), -pi], [inf(-1), 0],
		[inf(-1), pi], [inf(-1), inf(1)], [inf(-1), nan()], [-pi, inf(-1)],
		[-pi, 0], [-pi, inf(1)], [-pi, nan()], [f64(-0.0), inf(-1)],
		[f64(-0.0), -pi], [f64(-0.0), -0.0], [f64(-0.0), 0], [f64(-0.0), pi],
		[f64(-0.0), inf(1)], [f64(-0.0), nan()], [f64(0), inf(-1)],
		[f64(0), -pi], [f64(0), -0.0], [f64(0), 0], [f64(0), pi],
		[f64(0), inf(1)], [f64(0), nan()], [pi, inf(-1)], [pi, 0],
		[pi, inf(1)], [pi, nan()], [inf(1), inf(-1)], [inf(1), -pi],
		[inf(1), 0], [inf(1), pi], [inf(1), inf(1)], [inf(1), nan()],
		[nan(), nan()]]
	atan2_sc_ := [f64(-3.0) * pi / 4.0, /* atan2(-inf, -inf) */ -pi / 2, /* atan2(-inf, -pi) */
		-pi / 2,
		/* atan2(-inf, +0) */ -pi / 2, /* atan2(-inf, pi) */ -pi / 4, /* atan2(-inf, +inf) */
		nan(), /* atan2(-inf, nan) */ -pi, /* atan2(-pi, -inf) */ -pi / 2, /* atan2(-pi, +0) */
		-0.0,
		/* atan2(-pi, inf) */ nan(), /* atan2(-pi, nan) */ -pi, /* atan2(-0, -inf) */ -pi,
		/* atan2(-0, -pi) */ -pi, /* atan2(-0, -0) */ -0.0, /* atan2(-0, +0) */ -0.0, /* atan2(-0, pi) */
		-0.0,
		/* atan2(-0, +inf) */ nan(), /* atan2(-0, nan) */ pi, /* atan2(+0, -inf) */ pi, /* atan2(+0, -pi) */
		pi, /* atan2(+0, -0) */ 0, /* atan2(+0, +0) */ 0, /* atan2(+0, pi) */ 0, /* atan2(+0, +inf) */
		nan(), /* atan2(+0, nan) */ pi, /* atan2(pi, -inf) */ pi / 2, /* atan2(pi, +0) */ 0,
		/* atan2(pi, +inf) */ nan(), /* atan2(pi, nan) */ 3.0 * pi / 4, /* atan2(+inf, -inf) */
		pi / 2, /* atan2(+inf, -pi) */ pi / 2, /* atan2(+inf, +0) */ pi / 2, /* atan2(+inf, pi) */
		pi / 4, /* atan2(+inf, +inf) */ nan(), /* atan2(+inf, nan) */
		nan(), /* atan2(nan, nan) */
	]
	for i := 0; i < vfatan2_sc_.len; i++ {
		f := atan2(vfatan2_sc_[i][0], vfatan2_sc_[i][1])
		assert alike(atan2_sc_[i], f)
	}
}

fn test_ceil() {
	// for i := 0; i < vf_.len; i++ {
	// 	f := ceil(vf_[i])
	// 	assert alike(ceil_[i], f)
	// }
	vfceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	ceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfceil_sc_.len; i++ {
		f := ceil(vfceil_sc_[i])
		assert alike(ceil_sc_[i], f)
	}
}

fn test_cos() {
	for i := 0; i < math.vf_.len; i++ {
		f := cos(math.vf_[i])
		assert veryclose(math.cos_[i], f)
	}
	vfcos_sc_ := [inf(-1), inf(1), nan()]
	cos_sc_ := [nan(), nan(), nan()]
	for i := 0; i < vfcos_sc_.len; i++ {
		f := cos(vfcos_sc_[i])
		assert alike(cos_sc_[i], f)
	}
}

fn test_cosh() {
	for i := 0; i < math.vf_.len; i++ {
		f := cosh(math.vf_[i])
		assert close(math.cosh_[i], f)
	}
	vfcosh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	cosh_sc_ := [inf(1), 1, 1, inf(1), nan()]
	for i := 0; i < vfcosh_sc_.len; i++ {
		f := cosh(vfcosh_sc_[i])
		assert alike(cosh_sc_[i], f)
	}
}

fn test_expm1() {
	for i := 0; i < math.vf_.len; i++ {
		a := math.vf_[i] / 100
		f := expm1(a)
		assert veryclose(math.expm1_[i], f)
	}
	for i := 0; i < math.vf_.len; i++ {
		a := math.vf_[i] * 10
		f := expm1(a)
		assert close(math.expm1_large_[i], f)
	}
	// vfexpm1_sc_            := [f64(-710), copysign(0, -1), 0, 710, inf(1), nan()]
	// expm1_sc_              := [f64(-1), copysign(0, -1), 0, inf(1), inf(1), nan()]
	// for i := 0; i < vfexpm1_sc_.len; i++ {
	// 	f := expm1(vfexpm1_sc_[i])
	// 	assert alike(expm1_sc_[i], f)
	// }
}

fn test_abs() {
	for i := 0; i < math.vf_.len; i++ {
		f := abs(math.vf_[i])
		assert math.fabs_[i] == f
	}
}

fn test_abs_zero() {
	ret1 := abs(0)
	println(ret1)
	assert '$ret1' == '0'

	ret2 := abs(0.0)
	println(ret2)
	assert '$ret2' == '0'
}

fn test_floor() {
	for i := 0; i < math.vf_.len; i++ {
		f := floor(math.vf_[i])
		assert alike(math.floor_[i], f)
	}
	vfceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	ceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfceil_sc_.len; i++ {
		f := floor(vfceil_sc_[i])
		assert alike(ceil_sc_[i], f)
	}
}

fn test_max() {
	for i := 0; i < math.vf_.len; i++ {
		f := max(math.vf_[i], math.ceil_[i])
		assert math.ceil_[i] == f
	}
}

fn test_min() {
	for i := 0; i < math.vf_.len; i++ {
		f := min(math.vf_[i], math.floor_[i])
		assert math.floor_[i] == f
	}
}

fn test_clamp() {
	assert clamp(2, 5, 10) == 5
	assert clamp(7, 5, 10) == 7
	assert clamp(15, 5, 10) == 10
	assert clamp(5, 5, 10) == 5
	assert clamp(10, 5, 10) == 10
}

fn test_signi() {
	assert signi(inf(-1)) == -1
	assert signi(-72234878292.4586129) == -1
	assert signi(-10) == -1
	assert signi(-pi) == -1
	assert signi(-1) == -1
	assert signi(-0.000000000001) == -1
	assert signi(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001) == -1
	assert signi(-0.0) == -1
	//
	assert signi(inf(1)) == 1
	assert signi(72234878292.4586129) == 1
	assert signi(10) == 1
	assert signi(pi) == 1
	assert signi(1) == 1
	assert signi(0.000000000001) == 1
	assert signi(0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001) == 1
	assert signi(0.0) == 1
	assert signi(nan()) == 1
}

fn test_sign() {
	assert sign(inf(-1)) == -1.0
	assert sign(-72234878292.4586129) == -1.0
	assert sign(-10) == -1.0
	assert sign(-pi) == -1.0
	assert sign(-1) == -1.0
	assert sign(-0.000000000001) == -1.0
	assert sign(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001) == -1.0
	assert sign(-0.0) == -1.0
	//
	assert sign(inf(1)) == 1.0
	assert sign(72234878292.4586129) == 1
	assert sign(10) == 1.0
	assert sign(pi) == 1.0
	assert sign(1) == 1.0
	assert sign(0.000000000001) == 1.0
	assert sign(0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001) == 1.0
	assert sign(0.0) == 1.0
	assert is_nan(sign(nan()))
	assert is_nan(sign(-nan()))
}

fn test_mod() {
	for i := 0; i < math.vf_.len; i++ {
		f := mod(10, math.vf_[i])
		assert math.fmod_[i] == f
	}
	// verify precision of result for extreme inputs
	f := mod(5.9790119248836734e+200, 1.1258465975523544)
	assert (0.6447968302508578) == f
}

fn test_cbrt() {
	cbrts := [2.0, 10, 56]
	for idx, i in [8.0, 1000, 175_616] {
		assert cbrt(i) == cbrts[idx]
	}
}

fn test_exp() {
	for i := 0; i < math.vf_.len; i++ {
		f := exp(math.vf_[i])
		assert veryclose(math.exp_[i], f)
	}
	vfexp_sc_ := [inf(-1), -2000, 2000, inf(1), nan(), /* smallest f64 that overflows Exp(x) */
		7.097827128933841e+02, 1.48852223e+09, 1.4885222e+09, 1, /* near zero */
		3.725290298461915e-09,
		/* denormal */ -740]
	exp_sc_ := [f64(0), 0, inf(1), inf(1), nan(), inf(1), inf(1),
		inf(1), 2.718281828459045, 1.0000000037252903, 4.2e-322]
	for i := 0; i < vfexp_sc_.len; i++ {
		f := exp(vfexp_sc_[i])
		assert alike(exp_sc_[i], f)
	}
}

fn test_exp2() {
	for i := 0; i < math.vf_.len; i++ {
		f := exp2(math.vf_[i])
		assert soclose(math.exp2_[i], f, 1e-9)
	}
	vfexp2_sc_ := [f64(-2000), 2000, inf(1), nan(), /* smallest f64 that overflows Exp2(x) */
		1024, /* near underflow */ -1.07399999999999e+03, /* near zero */ 3.725290298461915e-09]
	exp2_sc_ := [f64(0), inf(1), inf(1), nan(), inf(1), 5e-324, 1.0000000025821745]
	for i := 0; i < vfexp2_sc_.len; i++ {
		f := exp2(vfexp2_sc_[i])
		assert alike(exp2_sc_[i], f)
	}
	for n := -1074; n < 1024; n++ {
		f := exp2(f64(n))
		vf := ldexp(1, n)
		assert veryclose(f, vf)
	}
}

fn test_frexp() {
	for i := 0; i < math.vf_.len; i++ {
		f, j := frexp(math.vf_[i])
		assert veryclose(math.frexp_[i].f, f) || math.frexp_[i].i != j
	}
	// vffrexp_sc_            := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	// frexp_sc_              := [Fi{inf(-1), 0}, Fi{copysign(0, -1), 0}, Fi{0, 0},
	// 	Fi{inf(1), 0}, Fi{nan(), 0}]
	// for i := 0; i < vffrexp_sc_.len; i++ {
	// 	f, j := frexp(vffrexp_sc_[i])
	// 	assert alike(frexp_sc_[i].f, f) || frexp_sc_[i].i != j
	// }
}

fn test_gamma() {
	vfgamma_ := [[inf(1), inf(1)], [inf(-1), nan()], [f64(0), inf(1)],
		[f64(-0.0), inf(-1)], [nan(), nan()], [f64(-1), nan()],
		[f64(-2), nan()], [f64(-3), nan()], [f64(-1e+16), nan()],
		[f64(-1e+300), nan()], [f64(1.7e+308), inf(1)], /* Test inputs inspi_red by Python test suite. */
		// Outputs computed at high precision by PARI/GP.
		// If recomputing table entries), be careful to use
		// high-precision (%.1000g) formatting of the f64 inputs.
		// For example), -2.0000000000000004 is the f64 with exact value
		//-2.00000000000000044408920985626161695), and
		// gamma(-2.0000000000000004) = -1249999999999999.5386078562728167651513), while
		// gamma(-2.00000000000000044408920985626161695) = -1125899906826907.2044875028130093136826.
		// Thus the table lists -1.1258999068426235e+15 as the answer.
		[f64(0.5), 1.772453850905516], [f64(1.5), 0.886226925452758],
		[f64(2.5), 1.329340388179137], [f64(3.5), 3.3233509704478426],
		[f64(-0.5), -3.544907701811032], [f64(-1.5), 2.363271801207355],
		[f64(-2.5), -0.9453087204829419], [f64(-3.5), 0.2700882058522691],
		[f64(0.1), 9.51350769866873], [f64(0.01), 99.4325851191506],
		[f64(1e-08), 9.999999942278434e+07], [f64(1e-16), 1e+16],
		[f64(0.001), 999.4237724845955], [f64(1e-16), 1e+16],
		[f64(1e-308), 1e+308], [f64(5.6e-309), 1.7857142857142864e+308],
		[f64(5.5e-309), inf(1)], [f64(1e-309), inf(1)], [f64(1e-323), inf(1)],
		[f64(5e-324), inf(1)], [f64(-0.1), -10.686287021193193],
		[f64(-0.01), -100.58719796441078], [f64(-1e-08), -1.0000000057721567e+08],
		[f64(-1e-16), -1e+16], [f64(-0.001), -1000.5782056293586],
		[f64(-1e-16), -1e+16], [f64(-1e-308), -1e+308], [f64(-5.6e-309), -1.7857142857142864e+308],
		[f64(-5.5e-309), inf(-1)], [f64(-1e-309), inf(-1)], [f64(-1e-323), inf(-1)],
		[f64(-5e-324), inf(-1)], [f64(-0.9999999999999999), -9.007199254740992e+15],
		[f64(-1.0000000000000002), 4.5035996273704955e+15],
		[f64(-1.9999999999999998),
			2.2517998136852485e+15],
		[f64(-2.0000000000000004), -1.1258999068426235e+15],
		[f64(-100.00000000000001),
			-7.540083334883109e-145],
		[f64(-99.99999999999999), 7.540083334884096e-145], [f64(17), 2.0922789888e+13],
		[f64(171), 7.257415615307999e+306], [f64(171.6), 1.5858969096672565e+308],
		[f64(171.624), 1.7942117599248104e+308], [f64(171.625), inf(1)],
		[f64(172), inf(1)], [f64(2000), inf(1)], [f64(-100.5), -3.3536908198076787e-159],
		[f64(-160.5), -5.255546447007829e-286], [f64(-170.5), -3.3127395215386074e-308],
		[f64(-171.5), 1.9316265431712e-310], [f64(-176.5), -1.196e-321],
		[f64(-177.5), 5e-324], [f64(-178.5), -0.0], [f64(-179.5), 0],
		[f64(-201.0001), 0], [f64(-202.9999), -0.0], [f64(-1000.5), -0.0],
		[f64(-1.0000000003e+09), -0.0], [f64(-4.5035996273704955e+15), 0],
		[f64(-63.349078729022985), 4.177797167776188e-88],
		[f64(-127.45117632943295),
			1.183111089623681e-214]]
	_ := vfgamma_[0][0]
	// @todo: Figure out solution for C backend
	// for i := 0; i < math.vf_.len; i++ {
	// 	f := gamma(math.vf_[i])
	// 	assert veryclose(math.gamma_[i], f)
	// }
	// for _, g in vfgamma_ {
	// 	f := gamma(g[0])
	// 	if is_nan(g[1]) || is_inf(g[1], 0) || g[1] == 0 || f == 0 {
	// 		assert alike(g[1], f)
	// 	} else if g[0] > -50 && g[0] <= 171 {
	// 		assert veryclose(g[1], f)
	// 	} else {
	// 		assert soclose(g[1], f, 1e-9)
	// 	}
	// }
}

fn test_hypot() {
	for i := 0; i < math.vf_.len; i++ {
		a := abs(1e+200 * math.tanh_[i] * sqrt(2.0))
		f := hypot(1e+200 * math.tanh_[i], 1e+200 * math.tanh_[i])
		assert veryclose(a, f)
	}
	vfhypot_sc_ := [[inf(-1), inf(-1)], [inf(-1), 0], [inf(-1),
		inf(1)],
		[inf(-1), nan()], [f64(-0.0), -0.0], [f64(-0.0), 0], [f64(0), -0.0],
		[f64(0), 0], /* +0,0 */ [f64(0), inf(-1)], [f64(0), inf(1)],
		[f64(0), nan()], [inf(1), inf(-1)], [inf(1), 0], [inf(1),
			inf(1)],
		[inf(1), nan()], [nan(), inf(-1)], [nan(), 0], [nan(),
			inf(1)],
		[nan(), nan()]]
	hypot_sc_ := [inf(1), inf(1), inf(1), inf(1), 0, 0, 0, 0, inf(1),
		inf(1), nan(), inf(1), inf(1), inf(1), inf(1), inf(1),
		nan(), inf(1), nan()]
	for i := 0; i < vfhypot_sc_.len; i++ {
		f := hypot(vfhypot_sc_[i][0], vfhypot_sc_[i][1])
		assert alike(hypot_sc_[i], f)
	}
}

fn test_ldexp() {
	for i := 0; i < math.vf_.len; i++ {
		f := ldexp(math.frexp_[i].f, math.frexp_[i].i)
		assert veryclose(math.vf_[i], f)
	}
	vffrexp_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	frexp_sc_ := [Fi{inf(-1), 0}, Fi{copysign(0, -1), 0}, Fi{0, 0},
		Fi{inf(1), 0}, Fi{nan(), 0}]
	for i := 0; i < vffrexp_sc_.len; i++ {
		f := ldexp(frexp_sc_[i].f, frexp_sc_[i].i)
		assert alike(vffrexp_sc_[i], f)
	}
	vfldexp_sc_ := [Fi{0, 0}, Fi{0, -1075}, Fi{0, 1024}, Fi{copysign(0, -1), 0},
		Fi{copysign(0, -1), -1075}, Fi{copysign(0, -1), 1024},
		Fi{inf(1), 0}, Fi{inf(1), -1024}, Fi{inf(-1), 0}, Fi{inf(-1), -1024},
		Fi{nan(), -1024}, Fi{10, 1 << (u64(sizeof(int) - 1) * 8)},
		Fi{10, -(1 << (u64(sizeof(int) - 1) * 8))}]
	ldexp_sc_ := [f64(0), 0, 0, copysign(0, -1), copysign(0, -1),
		copysign(0, -1), inf(1), inf(1), inf(-1), inf(-1), nan(),
		inf(1), 0]
	for i := 0; i < vfldexp_sc_.len; i++ {
		f := ldexp(vfldexp_sc_[i].f, vfldexp_sc_[i].i)
		assert alike(ldexp_sc_[i], f)
	}
}

fn test_log_gamma() {
	for i := 0; i < math.vf_.len; i++ {
		f, s := log_gamma_sign(math.vf_[i])
		assert soclose(math.log_gamma_[i].f, f, 1e-6) && math.log_gamma_[i].i == s
	}
	// vflog_gamma_sc_        := [inf(-1), -3, 0, 1, 2, inf(1), nan()]
	// log_gamma_sc_          := [Fi{inf(-1), 1}, Fi{inf(1), 1}, Fi{inf(1), 1},
	// 	Fi{0, 1}, Fi{0, 1}, Fi{inf(1), 1}, Fi{nan(), 1}]
	// for i := 0; i < vflog_gamma_sc_.len; i++ {
	// 	f, s := log_gamma_sign(vflog_gamma_sc_[i])
	// 	assert alike(log_gamma_sc_[i].f, f) && log_gamma_sc_[i].i == s
	// }
}

fn test_log() {
	for i := 0; i < math.vf_.len; i++ {
		a := abs(math.vf_[i])
		f := log(a)
		assert math.log_[i] == f
	}
	vflog_sc_ := [inf(-1), -pi, copysign(0, -1), 0, 1, inf(1),
		nan()]
	log_sc_ := [nan(), nan(), inf(-1), inf(-1), 0, inf(1), nan()]
	f := log(10)
	assert f == ln10
	for i := 0; i < vflog_sc_.len; i++ {
		g := log(vflog_sc_[i])
		assert alike(log_sc_[i], g)
	}
}

fn test_log10() {
	for i := 0; i < math.vf_.len; i++ {
		a := abs(math.vf_[i])
		f := log10(a)
		assert veryclose(math.log10_[i], f)
	}
	vflog_sc_ := [inf(-1), -pi, copysign(0, -1), 0, 1, inf(1),
		nan()]
	log_sc_ := [nan(), nan(), inf(-1), inf(-1), 0, inf(1), nan()]
	for i := 0; i < vflog_sc_.len; i++ {
		f := log10(vflog_sc_[i])
		assert alike(log_sc_[i], f)
	}
}

fn test_pow() {
	for i := 0; i < math.vf_.len; i++ {
		f := pow(10, math.vf_[i])
		assert close(math.pow_[i], f)
	}
	vfpow_sc_ := [[inf(-1), -pi], [inf(-1), -3], [inf(-1), -0.0],
		[inf(-1), 0], [inf(-1), 1], [inf(-1), 3], [inf(-1), pi],
		[inf(-1), 0.5], [inf(-1), nan()], [-pi, inf(-1)], [-pi, -pi],
		[-pi, -0.0], [-pi, 0], [-pi, 1], [-pi, pi], [-pi, inf(1)],
		[-pi, nan()], [f64(-1), inf(-1)], [f64(-1), inf(1)], [f64(-1), nan()],
		[f64(-1 / 2), inf(-1)], [f64(-1 / 2), inf(1)], [f64(-0.0), inf(-1)],
		[f64(-0.0), -pi], [f64(-0.0), -0.5], [f64(-0.0), -3],
		[f64(-0.0), 3], [f64(-0.0), pi], [f64(-0.0), 0.5], [f64(-0.0), inf(1)],
		[f64(0), inf(-1)], [f64(0), -pi], [f64(0), -3], [f64(0), -0.0],
		[f64(0), 0], [f64(0), 3], [f64(0), pi], [f64(0), inf(1)],
		[f64(0), nan()], [f64(1 / 2), inf(-1)], [f64(1 / 2), inf(1)],
		[f64(1), inf(-1)], [f64(1), inf(1)], [f64(1), nan()],
		[pi, inf(-1)], [pi, -0.0], [pi, 0], [pi, 1], [pi, inf(1)],
		[pi, nan()], [inf(1), -pi], [inf(1), -0.0], [inf(1), 0],
		[inf(1), 1], [inf(1), pi], [inf(1), nan()], [nan(), -pi],
		[nan(), -0.0], [nan(), 0], [nan(), 1], [nan(), pi], [nan(),
			nan()]]
	pow_sc_ := [f64(0), /* pow(-inf, -pi) */ -0.0, /* pow(-inf, -3) */ 1, /* pow(-inf, -0) */ 1, /* pow(-inf, +0) */
		inf(-1), /* pow(-inf, 1) */ inf(-1), /* pow(-inf, 3) */
		inf(1), /* pow(-inf, pi) */ inf(1), /* pow(-inf, 0.5) */
		nan(), /* pow(-inf, nan) */ 0, /* pow(-pi, -inf) */ nan(), /* pow(-pi, -pi) */
		1, /* pow(-pi, -0) */ 1, /* pow(-pi, +0) */ -pi, /* pow(-pi, 1) */ nan(), /* pow(-pi, pi) */
		inf(1), /* pow(-pi, +inf) */ nan(), /* pow(-pi, nan) */ 1, /* pow(-1, -inf) IEEE 754-2008 */
		1, /* pow(-1, +inf) IEEE 754-2008 */ nan(), /* pow(-1, nan) */
		inf(1), /* pow(-1/2, -inf) */ 0, /* pow(-1/2, +inf) */ inf(1), /* pow(-0, -inf) */
		inf(1), /* pow(-0, -pi) */ inf(1), /* pow(-0, -0.5) */
		inf(-1), /* pow(-0, -3) IEEE 754-2008 */ -0.0, /* pow(-0, 3) IEEE 754-2008 */ 0, /* pow(-0, pi) */
		0, /* pow(-0, 0.5) */ 0, /* pow(-0, +inf) */ inf(1), /* pow(+0, -inf) */
		inf(1), /* pow(+0, -pi) */ inf(1), /* pow(+0, -3) */ 1, /* pow(+0, -0) */ 1, /* pow(+0, +0) */
		0, /* pow(+0, 3) */ 0,
		/* pow(+0, pi) */ 0, /* pow(+0, +inf) */ nan(), /* pow(+0, nan) */
		inf(1), /* pow(1/2, -inf) */ 0, /* pow(1/2, +inf) */ 1, /* pow(1, -inf) IEEE 754-2008 */
		1, /* pow(1, +inf) IEEE 754-2008 */ 1, /* pow(1, nan) IEEE 754-2008 */ 0, /* pow(pi, -inf) */
		1, /* pow(pi, -0) */ 1, /* pow(pi, +0) */ pi, /* pow(pi, 1) */ inf(1), /* pow(pi, +inf) */
		nan(), /* pow(pi, nan) */ 0, /* pow(+inf, -pi) */ 1, /* pow(+inf, -0) */ 1, /* pow(+inf, +0) */
		inf(1), /* pow(+inf, 1) */ inf(1), /* pow(+inf, pi) */
		nan(), /* pow(+inf, nan) */ nan(), /* pow(nan, -pi) */ 1, /* pow(nan, -0) */ 1, /* pow(nan, +0) */
		nan(), /* pow(nan, 1) */ nan(), /* pow(nan, pi) */ nan(), /* pow(nan, nan) */]
	for i := 0; i < vfpow_sc_.len; i++ {
		f := pow(vfpow_sc_[i][0], vfpow_sc_[i][1])
		assert alike(pow_sc_[i], f)
	}
}

fn test_round() {
	for i := 0; i < math.vf_.len; i++ {
		f := round(math.vf_[i])
		// @todo: Figure out why is this happening and fix it
		if math.round_[i] == 0 {
			// 0 compared to -0 with alike fails
			continue
		}
		assert alike(math.round_[i], f)
	}
	vfround_sc_ := [[f64(0), 0], [nan(), nan()], [inf(1), inf(1)]]
	// vfround_even_sc_ := [[f64(0), 0], [f64(1.390671161567e-309), 0], /* denormal */
	// 	[f64(0.49999999999999994), 0], /* 0.5-epsilon */ [f64(0.5), 0],
	// 	[f64(0.5000000000000001), 1], /* 0.5+epsilon */ [f64(-1.5), -2],
	// 	[f64(-2.5), -2], [nan(), nan()], [inf(1), inf(1)],
	// 	[f64(2251799813685249.5), 2251799813685250],
	// 	// 1 bit fractian [f64(2251799813685250.5), 2251799813685250],
	// 	[f64(4503599627370495.5), 4503599627370496], /* 1 bit fraction, rounding to 0 bit fractian */
	// 	[f64(4503599627370497), 4503599627370497], /* large integer */
	// ]
	for i := 0; i < vfround_sc_.len; i++ {
		f := round(vfround_sc_[i][0])
		assert alike(vfround_sc_[i][1], f)
	}
}

fn test_sin() {
	for i := 0; i < math.vf_.len; i++ {
		f := sin(math.vf_[i])
		assert veryclose(math.sin_[i], f)
	}
	vfsin_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	sin_sc_ := [nan(), copysign(0, -1), 0, nan(), nan()]
	for i := 0; i < vfsin_sc_.len; i++ {
		f := sin(vfsin_sc_[i])
		assert alike(sin_sc_[i], f)
	}
}

fn test_sincos() {
	for i := 0; i < math.vf_.len; i++ {
		f, g := sincos(math.vf_[i])
		assert veryclose(math.sin_[i], f)
		assert veryclose(math.cos_[i], g)
	}
	vfsin_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	sin_sc_ := [nan(), copysign(0, -1), 0, nan(), nan()]
	for i := 0; i < vfsin_sc_.len; i++ {
		f, _ := sincos(vfsin_sc_[i])
		assert alike(sin_sc_[i], f)
	}
	vfcos_sc_ := [inf(-1), inf(1), nan()]
	cos_sc_ := [nan(), nan(), nan()]
	for i := 0; i < vfcos_sc_.len; i++ {
		_, f := sincos(vfcos_sc_[i])
		assert alike(cos_sc_[i], f)
	}
}

fn test_sinh() {
	for i := 0; i < math.vf_.len; i++ {
		f := sinh(math.vf_[i])
		assert close(math.sinh_[i], f)
	}
	vfsinh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	sinh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfsinh_sc_.len; i++ {
		f := sinh(vfsinh_sc_[i])
		assert alike(sinh_sc_[i], f)
	}
}

fn test_sqrt() {
	for i := 0; i < math.vf_.len; i++ {
		mut a := abs(math.vf_[i])
		mut f := sqrt(a)
		assert veryclose(math.sqrt_[i], f)
		a = abs(math.vf_[i])
		f = sqrt(a)
		assert veryclose(math.sqrt_[i], f)
	}
	vfsqrt_sc_ := [inf(-1), -pi, copysign(0, -1), 0, inf(1), nan()]
	sqrt_sc_ := [nan(), nan(), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfsqrt_sc_.len; i++ {
		mut f := sqrt(vfsqrt_sc_[i])
		assert alike(sqrt_sc_[i], f)
		f = sqrt(vfsqrt_sc_[i])
		assert alike(sqrt_sc_[i], f)
	}
}

fn test_tan() {
	for i := 0; i < math.vf_.len; i++ {
		f := tan(math.vf_[i])
		assert veryclose(math.tan_[i], f)
	}
	vfsin_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	sin_sc_ := [nan(), copysign(0, -1), 0, nan(), nan()]
	// same special cases as sin
	for i := 0; i < vfsin_sc_.len; i++ {
		f := tan(vfsin_sc_[i])
		assert alike(sin_sc_[i], f)
	}
}

fn test_tanh() {
	for i := 0; i < math.vf_.len; i++ {
		f := tanh(math.vf_[i])
		assert veryclose(math.tanh_[i], f)
	}
	vftanh_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	tanh_sc_ := [f64(-1), copysign(0, -1), 0, 1, nan()]
	for i := 0; i < vftanh_sc_.len; i++ {
		f := tanh(vftanh_sc_[i])
		assert alike(tanh_sc_[i], f)
	}
}

fn test_trunc() {
	// for i := 0; i < vf_.len; i++ {
	// 	f := trunc(vf_[i])
	// 	assert alike(trunc_[i], f)
	// }
	vfceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	ceil_sc_ := [inf(-1), copysign(0, -1), 0, inf(1), nan()]
	for i := 0; i < vfceil_sc_.len; i++ {
		f := trunc(vfceil_sc_[i])
		assert alike(ceil_sc_[i], f)
	}
}

fn test_gcd() {
	assert gcd(6, 9) == 3
	assert gcd(6, -9) == 3
	assert gcd(-6, -9) == 3
	assert gcd(0, 0) == 0
}

fn test_egcd() {
	helper := fn (a i64, b i64, expected_g i64) {
		g, x, y := egcd(a, b)
		assert g == expected_g
		assert abs(a * x + b * y) == g
	}

	helper(6, 9, 3)
	helper(6, -9, 3)
	helper(-6, -9, 3)
	helper(0, 0, 0)
}

fn test_lcm() {
	assert lcm(2, 3) == 6
	assert lcm(-2, 3) == 6
	assert lcm(-2, -3) == 6
	assert lcm(0, 0) == 0
}

fn test_digits() {
	// a small sanity check with a known number like 100,
	// just written in different base systems:
	assert digits(100, reverse: true) == [1, 0, 0]
	assert digits(100, base: 2, reverse: true) == [1, 1, 0, 0, 1, 0, 0]
	assert digits(100, base: 3, reverse: true) == [1, 0, 2, 0, 1]
	assert digits(100, base: 4, reverse: true) == [1, 2, 1, 0]
	assert digits(100, base: 8, reverse: true) == [1, 4, 4]
	assert digits(100, base: 10, reverse: true) == [1, 0, 0]
	assert digits(100, base: 12, reverse: true) == [8, 4]
	assert digits(100, base: 16, reverse: true) == [6, 4]
	assert digits(100, base: 20, reverse: true) == [5, 0]
	assert digits(100, base: 32, reverse: true) == [3, 4]
	assert digits(100, base: 64, reverse: true) == [1, 36]
	assert digits(100, base: 128, reverse: true) == [100]
	assert digits(100, base: 256, reverse: true) == [100]

	assert digits(1234432112344321) == digits(1234432112344321, reverse: true)
	assert digits(1234432112344321) == [1, 2, 3, 4, 4, 3, 2, 1, 1, 2, 3, 4, 4, 3, 2, 1]

	assert digits(125, base: 10, reverse: true) == [1, 2, 5]
	assert digits(125, base: 10).reverse() == [1, 2, 5]

	assert digits(15, base: 16, reverse: true) == [15]
	assert digits(127, base: 16, reverse: true) == [7, 15]
	assert digits(65535, base: 16, reverse: true) == [15, 15, 15, 15]
	assert digits(-65535, base: 16, reverse: true) == [-15, 15, 15, 15]

	assert digits(-127) == [7, 2, -1]
	assert digits(-127).reverse() == [-1, 2, 7]
	assert digits(-127, reverse: true) == [-1, 2, 7]

	assert digits(234, base: 7).reverse() == [4, 5, 3]

	assert digits(67432, base: 12).reverse() == [3, 3, 0, 3, 4]
}

// Check that math functions of high angle values
// return accurate results. [since (vf_[i] + large) - large != vf_[i],
// testing for Trig(vf_[i] + large) == Trig(vf_[i]), where large is
// a multiple of 2 * pi, is misleading.]
fn test_large_cos() {
	large := 100000.0 * pi
	for i := 0; i < math.vf_.len; i++ {
		f1 := math.cos_large_[i]
		f2 := cos(math.vf_[i] + large)
		assert soclose(f1, f2, 4e-8)
	}
}

fn test_large_sin() {
	large := 100000.0 * pi
	for i := 0; i < math.vf_.len; i++ {
		f1 := math.sin_large_[i]
		f2 := sin(math.vf_[i] + large)
		assert soclose(f1, f2, 4e-9)
	}
}

fn test_large_tan() {
	large := 100000.0 * pi
	for i := 0; i < math.vf_.len; i++ {
		f1 := math.tan_large_[i]
		f2 := tan(math.vf_[i] + large)
		assert soclose(f1, f2, 4e-8)
	}
}

fn test_sqrti() {
	assert sqrti(i64(123456789) * i64(123456789)) == 123456789
	assert sqrti(144) == 12
	assert sqrti(0) == 0
}

fn test_powi() {
	assert powi(2, 62) == i64(4611686018427387904)
	assert powi(0, -2) == -1 // div by 0
	assert powi(2, -1) == 0
}

fn test_count_digits() {
	assert count_digits(-999) == 3
	assert count_digits(-100) == 3
	assert count_digits(-99) == 2
	assert count_digits(-10) == 2
	assert count_digits(-1) == 1
	assert count_digits(0) == 1
	assert count_digits(1) == 1
	assert count_digits(10) == 2
	assert count_digits(99) == 2
	assert count_digits(100) == 3
	assert count_digits(999) == 3
	//
	assert count_digits(12345) == 5
	assert count_digits(123456789012345) == 15
	assert count_digits(-67345) == 5
}
