[translated]
module binaryen

$if dynamic_binaryen ? {
	#flag -lbinaryen
} $else {
	#flag -lbinaryen
	#flag -I@VEXEROOT/thirdparty/binaryen/include
	#flag -L@VEXEROOT/thirdparty/binaryen/lib

	#flag darwin -lc++ -Wl,-rpath,"@executable_path/../../../thirdparty/binaryen/lib"
	// the following, allows linking to the binaryen package from `brew install binaryen`, without having to run cmd/tools/install_binaryen.vsh first
	#flag darwin -I/opt/homebrew/include -L/opt/homebrew/lib -Wl,-rpath,"/opt/homebrew/lib"

	#flag linux -lstdc++
}

type Index = u32
type Type = u64

[c: 'BinaryenTypeNone']
pub fn typenone() Type

[c: 'BinaryenTypeInt32']
pub fn typeint32() Type

[c: 'BinaryenTypeInt64']
pub fn typeint64() Type

[c: 'BinaryenTypeFloat32']
pub fn typefloat32() Type

[c: 'BinaryenTypeFloat64']
pub fn typefloat64() Type

[c: 'BinaryenTypeVec128']
pub fn typevec128() Type

[c: 'BinaryenTypeFuncref']
pub fn typefuncref() Type

[c: 'BinaryenTypeExternref']
pub fn typeexternref() Type

[c: 'BinaryenTypeAnyref']
pub fn typeanyref() Type

[c: 'BinaryenTypeEqref']
pub fn typeeqref() Type

[c: 'BinaryenTypeI31ref']
pub fn typei31ref() Type

[c: 'BinaryenTypeStructref']
pub fn typestructref() Type

[c: 'BinaryenTypeArrayref']
pub fn typearrayref() Type

[c: 'BinaryenTypeStringref']
pub fn typestringref() Type

[c: 'BinaryenTypeStringviewWTF8']
pub fn typestringviewwtf8() Type

[c: 'BinaryenTypeStringviewWTF16']
pub fn typestringviewwtf16() Type

[c: 'BinaryenTypeStringviewIter']
pub fn typestringviewiter() Type

[c: 'BinaryenTypeNullref']
pub fn typenullref() Type

[c: 'BinaryenTypeNullExternref']
pub fn typenullexternref() Type

[c: 'BinaryenTypeNullFuncref']
pub fn typenullfuncref() Type

[c: 'BinaryenTypeUnreachable']
pub fn typeunreachable() Type

[c: 'BinaryenTypeAuto']
pub fn typeauto() Type

fn (t Type) str() string {
	return match t {
		typenone() { 'void' }
		typeint32() { 'i32' }
		typeint64() { 'i64' }
		typefloat32() { 'f32' }
		typefloat64() { 'f64' }
		else { 'unknown binaryen type' }
	}
}

[c: 'BinaryenTypeCreate']
pub fn typecreate(valuetypes &Type, numtypes Index) Type

[c: 'BinaryenTypeArity']
pub fn typearity(t Type) u32

[c: 'BinaryenTypeExpand']
pub fn typeexpand(t Type, buf &Type)

type PackedType = u32

[c: 'BinaryenPackedTypeNotPacked']
pub fn packedtypenotpacked() PackedType

[c: 'BinaryenPackedTypeInt8']
pub fn packedtypeint8() PackedType

[c: 'BinaryenPackedTypeInt16']
pub fn packedtypeint16() PackedType

type HeapType = &u32

[c: 'BinaryenHeapTypeExt']
pub fn heaptypeext() HeapType

[c: 'BinaryenHeapTypeFunc']
pub fn heaptypefunc() HeapType

[c: 'BinaryenHeapTypeAny']
pub fn heaptypeany() HeapType

[c: 'BinaryenHeapTypeEq']
pub fn heaptypeeq() HeapType

[c: 'BinaryenHeapTypeI31']
pub fn heaptypei31() HeapType

[c: 'BinaryenHeapTypeData']
pub fn heaptypedata() HeapType

[c: 'BinaryenHeapTypeArray']
pub fn heaptypearray() HeapType

[c: 'BinaryenHeapTypeString']
pub fn heaptypestring() HeapType

[c: 'BinaryenHeapTypeStringviewWTF8']
pub fn heaptypestringviewwtf8() HeapType

[c: 'BinaryenHeapTypeStringviewWTF16']
pub fn heaptypestringviewwtf16() HeapType

[c: 'BinaryenHeapTypeStringviewIter']
pub fn heaptypestringviewiter() HeapType

[c: 'BinaryenHeapTypeNone']
pub fn heaptypenone() HeapType

[c: 'BinaryenHeapTypeNoext']
pub fn heaptypenoext() HeapType

[c: 'BinaryenHeapTypeNofunc']
pub fn heaptypenofunc() HeapType

[c: 'BinaryenHeapTypeIsBasic']
pub fn heaptypeisbasic(heaptype HeapType) bool

[c: 'BinaryenHeapTypeIsSignature']
pub fn heaptypeissignature(heaptype HeapType) bool

[c: 'BinaryenHeapTypeIsStruct']
pub fn heaptypeisstruct(heaptype HeapType) bool

[c: 'BinaryenHeapTypeIsArray']
pub fn heaptypeisarray(heaptype HeapType) bool

[c: 'BinaryenHeapTypeIsBottom']
pub fn heaptypeisbottom(heaptype HeapType) bool

[c: 'BinaryenHeapTypeGetBottom']
pub fn heaptypegetbottom(heaptype HeapType) HeapType

[c: 'BinaryenHeapTypeIsSubType']
pub fn heaptypeissubtype(left HeapType, right HeapType) bool

[c: 'BinaryenStructTypeGetNumFields']
pub fn structtypegetnumfields(heaptype HeapType) Index

[c: 'BinaryenStructTypeGetFieldType']
pub fn structtypegetfieldtype(heaptype HeapType, index Index) Type

[c: 'BinaryenStructTypeGetFieldPackedType']
pub fn structtypegetfieldpackedtype(heaptype HeapType, index Index) PackedType

[c: 'BinaryenStructTypeIsFieldMutable']
pub fn structtypeisfieldmutable(heaptype HeapType, index Index) bool

[c: 'BinaryenArrayTypeGetElementType']
pub fn arraytypegetelementtype(heaptype HeapType) Type

[c: 'BinaryenArrayTypeGetElementPackedType']
pub fn arraytypegetelementpackedtype(heaptype HeapType) PackedType

[c: 'BinaryenArrayTypeIsElementMutable']
pub fn arraytypeiselementmutable(heaptype HeapType) bool

[c: 'BinaryenSignatureTypeGetParams']
pub fn signaturetypegetparams(heaptype HeapType) Type

[c: 'BinaryenSignatureTypeGetResults']
pub fn signaturetypegetresults(heaptype HeapType) Type

[c: 'BinaryenTypeGetHeapType']
pub fn typegetheaptype(type_ Type) HeapType

[c: 'BinaryenTypeIsNullable']
pub fn typeisnullable(type_ Type) bool

[c: 'BinaryenTypeFromHeapType']
pub fn typefromheaptype(heaptype HeapType, nullable bool) Type

type TypeSystem = u32

[c: 'BinaryenTypeSystemEquirecursive']
pub fn typesystemequirecursive() TypeSystem

[c: 'BinaryenTypeSystemNominal']
pub fn typesystemnominal() TypeSystem

[c: 'BinaryenTypeSystemIsorecursive']
pub fn typesystemisorecursive() TypeSystem

[c: 'BinaryenGetTypeSystem']
pub fn gettypesystem() TypeSystem

[c: 'BinaryenSetTypeSystem']
pub fn settypesystem(typesystem TypeSystem)

type ExpressionId = u32

[c: 'BinaryenInvalidId']
pub fn invalidid() ExpressionId

type ExternalKind = u32

[c: 'BinaryenExternalFunction']
pub fn externalfunction() ExternalKind

[c: 'BinaryenExternalTable']
pub fn externaltable() ExternalKind

[c: 'BinaryenExternalMemory']
pub fn externalmemory() ExternalKind

[c: 'BinaryenExternalGlobal']
pub fn externalglobal() ExternalKind

[c: 'BinaryenExternalTag']
pub fn externaltag() ExternalKind

type Features = u32

[c: 'BinaryenFeatureMVP']
pub fn featuremvp() Features

[c: 'BinaryenFeatureAtomics']
pub fn featureatomics() Features

[c: 'BinaryenFeatureBulkMemory']
pub fn featurebulkmemory() Features

[c: 'BinaryenFeatureMutableGlobals']
pub fn featuremutableglobals() Features

[c: 'BinaryenFeatureNontrappingFPToInt']
pub fn featurenontrappingfptoint() Features

[c: 'BinaryenFeatureSignExt']
pub fn featuresignext() Features

[c: 'BinaryenFeatureSIMD128']
pub fn featuresimd128() Features

[c: 'BinaryenFeatureExceptionHandling']
pub fn featureexceptionhandling() Features

[c: 'BinaryenFeatureTailCall']
pub fn featuretailcall() Features

[c: 'BinaryenFeatureReferenceTypes']
pub fn featurereferencetypes() Features

[c: 'BinaryenFeatureMultivalue']
pub fn featuremultivalue() Features

[c: 'BinaryenFeatureGC']
pub fn featuregc() Features

[c: 'BinaryenFeatureMemory64']
pub fn featurememory64() Features

[c: 'BinaryenFeatureRelaxedSIMD']
pub fn featurerelaxedsimd() Features

[c: 'BinaryenFeatureExtendedConst']
pub fn featureextendedconst() Features

[c: 'BinaryenFeatureStrings']
pub fn featurestrings() Features

[c: 'BinaryenFeatureMultiMemories']
pub fn featuremultimemories() Features

[c: 'BinaryenFeatureAll']
pub fn featureall() Features

type Module = voidptr

[c: 'BinaryenModuleCreate']
pub fn modulecreate() Module

[c: 'BinaryenModuleDispose']
pub fn moduledispose(module_ Module)

union LiteralContainer {
	i32_ int
	i64_ i64
	f32_ f32
	f64_ f64
	v128 [16]u8
	func &char
}

struct Literal {
	type_ &u32
	lit   LiteralContainer
}

[c: 'BinaryenLiteralInt32']
pub fn literalint32(x int) Literal

[c: 'BinaryenLiteralInt64']
pub fn literalint64(x i64) Literal

[c: 'BinaryenLiteralFloat32']
pub fn literalfloat32(x f32) Literal

[c: 'BinaryenLiteralFloat64']
pub fn literalfloat64(x f64) Literal

[c: 'BinaryenLiteralVec128']
pub fn literalvec128(x &u8) Literal

[c: 'BinaryenLiteralFloat32Bits']
pub fn literalfloat32bits(x int) Literal

[c: 'BinaryenLiteralFloat64Bits']
pub fn literalfloat64bits(x i64) Literal

type Op = int

[c: 'BinaryenClzInt32']
pub fn clzint32() Op

[c: 'BinaryenCtzInt32']
pub fn ctzint32() Op

[c: 'BinaryenPopcntInt32']
pub fn popcntint32() Op

[c: 'BinaryenNegFloat32']
pub fn negfloat32() Op

[c: 'BinaryenAbsFloat32']
pub fn absfloat32() Op

[c: 'BinaryenCeilFloat32']
pub fn ceilfloat32() Op

[c: 'BinaryenFloorFloat32']
pub fn floorfloat32() Op

[c: 'BinaryenTruncFloat32']
pub fn truncfloat32() Op

[c: 'BinaryenNearestFloat32']
pub fn nearestfloat32() Op

[c: 'BinaryenSqrtFloat32']
pub fn sqrtfloat32() Op

[c: 'BinaryenEqZInt32']
pub fn eqzint32() Op

[c: 'BinaryenClzInt64']
pub fn clzint64() Op

[c: 'BinaryenCtzInt64']
pub fn ctzint64() Op

[c: 'BinaryenPopcntInt64']
pub fn popcntint64() Op

[c: 'BinaryenNegFloat64']
pub fn negfloat64() Op

[c: 'BinaryenAbsFloat64']
pub fn absfloat64() Op

[c: 'BinaryenCeilFloat64']
pub fn ceilfloat64() Op

[c: 'BinaryenFloorFloat64']
pub fn floorfloat64() Op

[c: 'BinaryenTruncFloat64']
pub fn truncfloat64() Op

[c: 'BinaryenNearestFloat64']
pub fn nearestfloat64() Op

[c: 'BinaryenSqrtFloat64']
pub fn sqrtfloat64() Op

[c: 'BinaryenEqZInt64']
pub fn eqzint64() Op

[c: 'BinaryenExtendSInt32']
pub fn extendsint32() Op

[c: 'BinaryenExtendUInt32']
pub fn extenduint32() Op

[c: 'BinaryenWrapInt64']
pub fn wrapint64() Op

[c: 'BinaryenTruncSFloat32ToInt32']
pub fn truncsfloat32toint32() Op

[c: 'BinaryenTruncSFloat32ToInt64']
pub fn truncsfloat32toint64() Op

[c: 'BinaryenTruncUFloat32ToInt32']
pub fn truncufloat32toint32() Op

[c: 'BinaryenTruncUFloat32ToInt64']
pub fn truncufloat32toint64() Op

[c: 'BinaryenTruncSFloat64ToInt32']
pub fn truncsfloat64toint32() Op

[c: 'BinaryenTruncSFloat64ToInt64']
pub fn truncsfloat64toint64() Op

[c: 'BinaryenTruncUFloat64ToInt32']
pub fn truncufloat64toint32() Op

[c: 'BinaryenTruncUFloat64ToInt64']
pub fn truncufloat64toint64() Op

[c: 'BinaryenReinterpretFloat32']
pub fn reinterpretfloat32() Op

[c: 'BinaryenReinterpretFloat64']
pub fn reinterpretfloat64() Op

[c: 'BinaryenConvertSInt32ToFloat32']
pub fn convertsint32tofloat32() Op

[c: 'BinaryenConvertSInt32ToFloat64']
pub fn convertsint32tofloat64() Op

[c: 'BinaryenConvertUInt32ToFloat32']
pub fn convertuint32tofloat32() Op

[c: 'BinaryenConvertUInt32ToFloat64']
pub fn convertuint32tofloat64() Op

[c: 'BinaryenConvertSInt64ToFloat32']
pub fn convertsint64tofloat32() Op

[c: 'BinaryenConvertSInt64ToFloat64']
pub fn convertsint64tofloat64() Op

[c: 'BinaryenConvertUInt64ToFloat32']
pub fn convertuint64tofloat32() Op

[c: 'BinaryenConvertUInt64ToFloat64']
pub fn convertuint64tofloat64() Op

[c: 'BinaryenPromoteFloat32']
pub fn promotefloat32() Op

[c: 'BinaryenDemoteFloat64']
pub fn demotefloat64() Op

[c: 'BinaryenReinterpretInt32']
pub fn reinterpretint32() Op

[c: 'BinaryenReinterpretInt64']
pub fn reinterpretint64() Op

[c: 'BinaryenExtendS8Int32']
pub fn extends8int32() Op

[c: 'BinaryenExtendS16Int32']
pub fn extends16int32() Op

[c: 'BinaryenExtendS8Int64']
pub fn extends8int64() Op

[c: 'BinaryenExtendS16Int64']
pub fn extends16int64() Op

[c: 'BinaryenExtendS32Int64']
pub fn extends32int64() Op

[c: 'BinaryenAddInt32']
pub fn addint32() Op

[c: 'BinaryenSubInt32']
pub fn subint32() Op

[c: 'BinaryenMulInt32']
pub fn mulint32() Op

[c: 'BinaryenDivSInt32']
pub fn divsint32() Op

[c: 'BinaryenDivUInt32']
pub fn divuint32() Op

[c: 'BinaryenRemSInt32']
pub fn remsint32() Op

[c: 'BinaryenRemUInt32']
pub fn remuint32() Op

[c: 'BinaryenAndInt32']
pub fn andint32() Op

[c: 'BinaryenOrInt32']
pub fn orint32() Op

[c: 'BinaryenXorInt32']
pub fn xorint32() Op

[c: 'BinaryenShlInt32']
pub fn shlint32() Op

[c: 'BinaryenShrUInt32']
pub fn shruint32() Op

[c: 'BinaryenShrSInt32']
pub fn shrsint32() Op

[c: 'BinaryenRotLInt32']
pub fn rotlint32() Op

[c: 'BinaryenRotRInt32']
pub fn rotrint32() Op

[c: 'BinaryenEqInt32']
pub fn eqint32() Op

[c: 'BinaryenNeInt32']
pub fn neint32() Op

[c: 'BinaryenLtSInt32']
pub fn ltsint32() Op

[c: 'BinaryenLtUInt32']
pub fn ltuint32() Op

[c: 'BinaryenLeSInt32']
pub fn lesint32() Op

[c: 'BinaryenLeUInt32']
pub fn leuint32() Op

[c: 'BinaryenGtSInt32']
pub fn gtsint32() Op

[c: 'BinaryenGtUInt32']
pub fn gtuint32() Op

[c: 'BinaryenGeSInt32']
pub fn gesint32() Op

[c: 'BinaryenGeUInt32']
pub fn geuint32() Op

[c: 'BinaryenAddInt64']
pub fn addint64() Op

[c: 'BinaryenSubInt64']
pub fn subint64() Op

[c: 'BinaryenMulInt64']
pub fn mulint64() Op

[c: 'BinaryenDivSInt64']
pub fn divsint64() Op

[c: 'BinaryenDivUInt64']
pub fn divuint64() Op

[c: 'BinaryenRemSInt64']
pub fn remsint64() Op

[c: 'BinaryenRemUInt64']
pub fn remuint64() Op

[c: 'BinaryenAndInt64']
pub fn andint64() Op

[c: 'BinaryenOrInt64']
pub fn orint64() Op

[c: 'BinaryenXorInt64']
pub fn xorint64() Op

[c: 'BinaryenShlInt64']
pub fn shlint64() Op

[c: 'BinaryenShrUInt64']
pub fn shruint64() Op

[c: 'BinaryenShrSInt64']
pub fn shrsint64() Op

[c: 'BinaryenRotLInt64']
pub fn rotlint64() Op

[c: 'BinaryenRotRInt64']
pub fn rotrint64() Op

[c: 'BinaryenEqInt64']
pub fn eqint64() Op

[c: 'BinaryenNeInt64']
pub fn neint64() Op

[c: 'BinaryenLtSInt64']
pub fn ltsint64() Op

[c: 'BinaryenLtUInt64']
pub fn ltuint64() Op

[c: 'BinaryenLeSInt64']
pub fn lesint64() Op

[c: 'BinaryenLeUInt64']
pub fn leuint64() Op

[c: 'BinaryenGtSInt64']
pub fn gtsint64() Op

[c: 'BinaryenGtUInt64']
pub fn gtuint64() Op

[c: 'BinaryenGeSInt64']
pub fn gesint64() Op

[c: 'BinaryenGeUInt64']
pub fn geuint64() Op

[c: 'BinaryenAddFloat32']
pub fn addfloat32() Op

[c: 'BinaryenSubFloat32']
pub fn subfloat32() Op

[c: 'BinaryenMulFloat32']
pub fn mulfloat32() Op

[c: 'BinaryenDivFloat32']
pub fn divfloat32() Op

[c: 'BinaryenCopySignFloat32']
pub fn copysignfloat32() Op

[c: 'BinaryenMinFloat32']
pub fn minfloat32() Op

[c: 'BinaryenMaxFloat32']
pub fn maxfloat32() Op

[c: 'BinaryenEqFloat32']
pub fn eqfloat32() Op

[c: 'BinaryenNeFloat32']
pub fn nefloat32() Op

[c: 'BinaryenLtFloat32']
pub fn ltfloat32() Op

[c: 'BinaryenLeFloat32']
pub fn lefloat32() Op

[c: 'BinaryenGtFloat32']
pub fn gtfloat32() Op

[c: 'BinaryenGeFloat32']
pub fn gefloat32() Op

[c: 'BinaryenAddFloat64']
pub fn addfloat64() Op

[c: 'BinaryenSubFloat64']
pub fn subfloat64() Op

[c: 'BinaryenMulFloat64']
pub fn mulfloat64() Op

[c: 'BinaryenDivFloat64']
pub fn divfloat64() Op

[c: 'BinaryenCopySignFloat64']
pub fn copysignfloat64() Op

[c: 'BinaryenMinFloat64']
pub fn minfloat64() Op

[c: 'BinaryenMaxFloat64']
pub fn maxfloat64() Op

[c: 'BinaryenEqFloat64']
pub fn eqfloat64() Op

[c: 'BinaryenNeFloat64']
pub fn nefloat64() Op

[c: 'BinaryenLtFloat64']
pub fn ltfloat64() Op

[c: 'BinaryenLeFloat64']
pub fn lefloat64() Op

[c: 'BinaryenGtFloat64']
pub fn gtfloat64() Op

[c: 'BinaryenGeFloat64']
pub fn gefloat64() Op

[c: 'BinaryenAtomicRMWAdd']
pub fn atomicrmwadd() Op

[c: 'BinaryenAtomicRMWSub']
pub fn atomicrmwsub() Op

[c: 'BinaryenAtomicRMWAnd']
pub fn atomicrmwand() Op

[c: 'BinaryenAtomicRMWOr']
pub fn atomicrmwor() Op

[c: 'BinaryenAtomicRMWXor']
pub fn atomicrmwxor() Op

[c: 'BinaryenAtomicRMWXchg']
pub fn atomicrmwxchg() Op

[c: 'BinaryenTruncSatSFloat32ToInt32']
pub fn truncsatsfloat32toint32() Op

[c: 'BinaryenTruncSatSFloat32ToInt64']
pub fn truncsatsfloat32toint64() Op

[c: 'BinaryenTruncSatUFloat32ToInt32']
pub fn truncsatufloat32toint32() Op

[c: 'BinaryenTruncSatUFloat32ToInt64']
pub fn truncsatufloat32toint64() Op

[c: 'BinaryenTruncSatSFloat64ToInt32']
pub fn truncsatsfloat64toint32() Op

[c: 'BinaryenTruncSatSFloat64ToInt64']
pub fn truncsatsfloat64toint64() Op

[c: 'BinaryenTruncSatUFloat64ToInt32']
pub fn truncsatufloat64toint32() Op

[c: 'BinaryenTruncSatUFloat64ToInt64']
pub fn truncsatufloat64toint64() Op

[c: 'BinaryenSplatVecI8x16']
pub fn splatveci8x16() Op

[c: 'BinaryenExtractLaneSVecI8x16']
pub fn extractlanesveci8x16() Op

[c: 'BinaryenExtractLaneUVecI8x16']
pub fn extractlaneuveci8x16() Op

[c: 'BinaryenReplaceLaneVecI8x16']
pub fn replacelaneveci8x16() Op

[c: 'BinaryenSplatVecI16x8']
pub fn splatveci16x8() Op

[c: 'BinaryenExtractLaneSVecI16x8']
pub fn extractlanesveci16x8() Op

[c: 'BinaryenExtractLaneUVecI16x8']
pub fn extractlaneuveci16x8() Op

[c: 'BinaryenReplaceLaneVecI16x8']
pub fn replacelaneveci16x8() Op

[c: 'BinaryenSplatVecI32x4']
pub fn splatveci32x4() Op

[c: 'BinaryenExtractLaneVecI32x4']
pub fn extractlaneveci32x4() Op

[c: 'BinaryenReplaceLaneVecI32x4']
pub fn replacelaneveci32x4() Op

[c: 'BinaryenSplatVecI64x2']
pub fn splatveci64x2() Op

[c: 'BinaryenExtractLaneVecI64x2']
pub fn extractlaneveci64x2() Op

[c: 'BinaryenReplaceLaneVecI64x2']
pub fn replacelaneveci64x2() Op

[c: 'BinaryenSplatVecF32x4']
pub fn splatvecf32x4() Op

[c: 'BinaryenExtractLaneVecF32x4']
pub fn extractlanevecf32x4() Op

[c: 'BinaryenReplaceLaneVecF32x4']
pub fn replacelanevecf32x4() Op

[c: 'BinaryenSplatVecF64x2']
pub fn splatvecf64x2() Op

[c: 'BinaryenExtractLaneVecF64x2']
pub fn extractlanevecf64x2() Op

[c: 'BinaryenReplaceLaneVecF64x2']
pub fn replacelanevecf64x2() Op

[c: 'BinaryenEqVecI8x16']
pub fn eqveci8x16() Op

[c: 'BinaryenNeVecI8x16']
pub fn neveci8x16() Op

[c: 'BinaryenLtSVecI8x16']
pub fn ltsveci8x16() Op

[c: 'BinaryenLtUVecI8x16']
pub fn ltuveci8x16() Op

[c: 'BinaryenGtSVecI8x16']
pub fn gtsveci8x16() Op

[c: 'BinaryenGtUVecI8x16']
pub fn gtuveci8x16() Op

[c: 'BinaryenLeSVecI8x16']
pub fn lesveci8x16() Op

[c: 'BinaryenLeUVecI8x16']
pub fn leuveci8x16() Op

[c: 'BinaryenGeSVecI8x16']
pub fn gesveci8x16() Op

[c: 'BinaryenGeUVecI8x16']
pub fn geuveci8x16() Op

[c: 'BinaryenEqVecI16x8']
pub fn eqveci16x8() Op

[c: 'BinaryenNeVecI16x8']
pub fn neveci16x8() Op

[c: 'BinaryenLtSVecI16x8']
pub fn ltsveci16x8() Op

[c: 'BinaryenLtUVecI16x8']
pub fn ltuveci16x8() Op

[c: 'BinaryenGtSVecI16x8']
pub fn gtsveci16x8() Op

[c: 'BinaryenGtUVecI16x8']
pub fn gtuveci16x8() Op

[c: 'BinaryenLeSVecI16x8']
pub fn lesveci16x8() Op

[c: 'BinaryenLeUVecI16x8']
pub fn leuveci16x8() Op

[c: 'BinaryenGeSVecI16x8']
pub fn gesveci16x8() Op

[c: 'BinaryenGeUVecI16x8']
pub fn geuveci16x8() Op

[c: 'BinaryenEqVecI32x4']
pub fn eqveci32x4() Op

[c: 'BinaryenNeVecI32x4']
pub fn neveci32x4() Op

[c: 'BinaryenLtSVecI32x4']
pub fn ltsveci32x4() Op

[c: 'BinaryenLtUVecI32x4']
pub fn ltuveci32x4() Op

[c: 'BinaryenGtSVecI32x4']
pub fn gtsveci32x4() Op

[c: 'BinaryenGtUVecI32x4']
pub fn gtuveci32x4() Op

[c: 'BinaryenLeSVecI32x4']
pub fn lesveci32x4() Op

[c: 'BinaryenLeUVecI32x4']
pub fn leuveci32x4() Op

[c: 'BinaryenGeSVecI32x4']
pub fn gesveci32x4() Op

[c: 'BinaryenGeUVecI32x4']
pub fn geuveci32x4() Op

[c: 'BinaryenEqVecI64x2']
pub fn eqveci64x2() Op

[c: 'BinaryenNeVecI64x2']
pub fn neveci64x2() Op

[c: 'BinaryenLtSVecI64x2']
pub fn ltsveci64x2() Op

[c: 'BinaryenGtSVecI64x2']
pub fn gtsveci64x2() Op

[c: 'BinaryenLeSVecI64x2']
pub fn lesveci64x2() Op

[c: 'BinaryenGeSVecI64x2']
pub fn gesveci64x2() Op

[c: 'BinaryenEqVecF32x4']
pub fn eqvecf32x4() Op

[c: 'BinaryenNeVecF32x4']
pub fn nevecf32x4() Op

[c: 'BinaryenLtVecF32x4']
pub fn ltvecf32x4() Op

[c: 'BinaryenGtVecF32x4']
pub fn gtvecf32x4() Op

[c: 'BinaryenLeVecF32x4']
pub fn levecf32x4() Op

[c: 'BinaryenGeVecF32x4']
pub fn gevecf32x4() Op

[c: 'BinaryenEqVecF64x2']
pub fn eqvecf64x2() Op

[c: 'BinaryenNeVecF64x2']
pub fn nevecf64x2() Op

[c: 'BinaryenLtVecF64x2']
pub fn ltvecf64x2() Op

[c: 'BinaryenGtVecF64x2']
pub fn gtvecf64x2() Op

[c: 'BinaryenLeVecF64x2']
pub fn levecf64x2() Op

[c: 'BinaryenGeVecF64x2']
pub fn gevecf64x2() Op

[c: 'BinaryenNotVec128']
pub fn notvec128() Op

[c: 'BinaryenAndVec128']
pub fn andvec128() Op

[c: 'BinaryenOrVec128']
pub fn orvec128() Op

[c: 'BinaryenXorVec128']
pub fn xorvec128() Op

[c: 'BinaryenAndNotVec128']
pub fn andnotvec128() Op

[c: 'BinaryenBitselectVec128']
pub fn bitselectvec128() Op

[c: 'BinaryenAnyTrueVec128']
pub fn anytruevec128() Op

[c: 'BinaryenPopcntVecI8x16']
pub fn popcntveci8x16() Op

[c: 'BinaryenAbsVecI8x16']
pub fn absveci8x16() Op

[c: 'BinaryenNegVecI8x16']
pub fn negveci8x16() Op

[c: 'BinaryenAllTrueVecI8x16']
pub fn alltrueveci8x16() Op

[c: 'BinaryenBitmaskVecI8x16']
pub fn bitmaskveci8x16() Op

[c: 'BinaryenShlVecI8x16']
pub fn shlveci8x16() Op

[c: 'BinaryenShrSVecI8x16']
pub fn shrsveci8x16() Op

[c: 'BinaryenShrUVecI8x16']
pub fn shruveci8x16() Op

[c: 'BinaryenAddVecI8x16']
pub fn addveci8x16() Op

[c: 'BinaryenAddSatSVecI8x16']
pub fn addsatsveci8x16() Op

[c: 'BinaryenAddSatUVecI8x16']
pub fn addsatuveci8x16() Op

[c: 'BinaryenSubVecI8x16']
pub fn subveci8x16() Op

[c: 'BinaryenSubSatSVecI8x16']
pub fn subsatsveci8x16() Op

[c: 'BinaryenSubSatUVecI8x16']
pub fn subsatuveci8x16() Op

[c: 'BinaryenMinSVecI8x16']
pub fn minsveci8x16() Op

[c: 'BinaryenMinUVecI8x16']
pub fn minuveci8x16() Op

[c: 'BinaryenMaxSVecI8x16']
pub fn maxsveci8x16() Op

[c: 'BinaryenMaxUVecI8x16']
pub fn maxuveci8x16() Op

[c: 'BinaryenAvgrUVecI8x16']
pub fn avgruveci8x16() Op

[c: 'BinaryenAbsVecI16x8']
pub fn absveci16x8() Op

[c: 'BinaryenNegVecI16x8']
pub fn negveci16x8() Op

[c: 'BinaryenAllTrueVecI16x8']
pub fn alltrueveci16x8() Op

[c: 'BinaryenBitmaskVecI16x8']
pub fn bitmaskveci16x8() Op

[c: 'BinaryenShlVecI16x8']
pub fn shlveci16x8() Op

[c: 'BinaryenShrSVecI16x8']
pub fn shrsveci16x8() Op

[c: 'BinaryenShrUVecI16x8']
pub fn shruveci16x8() Op

[c: 'BinaryenAddVecI16x8']
pub fn addveci16x8() Op

[c: 'BinaryenAddSatSVecI16x8']
pub fn addsatsveci16x8() Op

[c: 'BinaryenAddSatUVecI16x8']
pub fn addsatuveci16x8() Op

[c: 'BinaryenSubVecI16x8']
pub fn subveci16x8() Op

[c: 'BinaryenSubSatSVecI16x8']
pub fn subsatsveci16x8() Op

[c: 'BinaryenSubSatUVecI16x8']
pub fn subsatuveci16x8() Op

[c: 'BinaryenMulVecI16x8']
pub fn mulveci16x8() Op

[c: 'BinaryenMinSVecI16x8']
pub fn minsveci16x8() Op

[c: 'BinaryenMinUVecI16x8']
pub fn minuveci16x8() Op

[c: 'BinaryenMaxSVecI16x8']
pub fn maxsveci16x8() Op

[c: 'BinaryenMaxUVecI16x8']
pub fn maxuveci16x8() Op

[c: 'BinaryenAvgrUVecI16x8']
pub fn avgruveci16x8() Op

[c: 'BinaryenQ15MulrSatSVecI16x8']
pub fn q15mulrsatsveci16x8() Op

[c: 'BinaryenExtMulLowSVecI16x8']
pub fn extmullowsveci16x8() Op

[c: 'BinaryenExtMulHighSVecI16x8']
pub fn extmulhighsveci16x8() Op

[c: 'BinaryenExtMulLowUVecI16x8']
pub fn extmullowuveci16x8() Op

[c: 'BinaryenExtMulHighUVecI16x8']
pub fn extmulhighuveci16x8() Op

[c: 'BinaryenAbsVecI32x4']
pub fn absveci32x4() Op

[c: 'BinaryenNegVecI32x4']
pub fn negveci32x4() Op

[c: 'BinaryenAllTrueVecI32x4']
pub fn alltrueveci32x4() Op

[c: 'BinaryenBitmaskVecI32x4']
pub fn bitmaskveci32x4() Op

[c: 'BinaryenShlVecI32x4']
pub fn shlveci32x4() Op

[c: 'BinaryenShrSVecI32x4']
pub fn shrsveci32x4() Op

[c: 'BinaryenShrUVecI32x4']
pub fn shruveci32x4() Op

[c: 'BinaryenAddVecI32x4']
pub fn addveci32x4() Op

[c: 'BinaryenSubVecI32x4']
pub fn subveci32x4() Op

[c: 'BinaryenMulVecI32x4']
pub fn mulveci32x4() Op

[c: 'BinaryenMinSVecI32x4']
pub fn minsveci32x4() Op

[c: 'BinaryenMinUVecI32x4']
pub fn minuveci32x4() Op

[c: 'BinaryenMaxSVecI32x4']
pub fn maxsveci32x4() Op

[c: 'BinaryenMaxUVecI32x4']
pub fn maxuveci32x4() Op

[c: 'BinaryenDotSVecI16x8ToVecI32x4']
pub fn dotsveci16x8toveci32x4() Op

[c: 'BinaryenExtMulLowSVecI32x4']
pub fn extmullowsveci32x4() Op

[c: 'BinaryenExtMulHighSVecI32x4']
pub fn extmulhighsveci32x4() Op

[c: 'BinaryenExtMulLowUVecI32x4']
pub fn extmullowuveci32x4() Op

[c: 'BinaryenExtMulHighUVecI32x4']
pub fn extmulhighuveci32x4() Op

[c: 'BinaryenAbsVecI64x2']
pub fn absveci64x2() Op

[c: 'BinaryenNegVecI64x2']
pub fn negveci64x2() Op

[c: 'BinaryenAllTrueVecI64x2']
pub fn alltrueveci64x2() Op

[c: 'BinaryenBitmaskVecI64x2']
pub fn bitmaskveci64x2() Op

[c: 'BinaryenShlVecI64x2']
pub fn shlveci64x2() Op

[c: 'BinaryenShrSVecI64x2']
pub fn shrsveci64x2() Op

[c: 'BinaryenShrUVecI64x2']
pub fn shruveci64x2() Op

[c: 'BinaryenAddVecI64x2']
pub fn addveci64x2() Op

[c: 'BinaryenSubVecI64x2']
pub fn subveci64x2() Op

[c: 'BinaryenMulVecI64x2']
pub fn mulveci64x2() Op

[c: 'BinaryenExtMulLowSVecI64x2']
pub fn extmullowsveci64x2() Op

[c: 'BinaryenExtMulHighSVecI64x2']
pub fn extmulhighsveci64x2() Op

[c: 'BinaryenExtMulLowUVecI64x2']
pub fn extmullowuveci64x2() Op

[c: 'BinaryenExtMulHighUVecI64x2']
pub fn extmulhighuveci64x2() Op

[c: 'BinaryenAbsVecF32x4']
pub fn absvecf32x4() Op

[c: 'BinaryenNegVecF32x4']
pub fn negvecf32x4() Op

[c: 'BinaryenSqrtVecF32x4']
pub fn sqrtvecf32x4() Op

[c: 'BinaryenAddVecF32x4']
pub fn addvecf32x4() Op

[c: 'BinaryenSubVecF32x4']
pub fn subvecf32x4() Op

[c: 'BinaryenMulVecF32x4']
pub fn mulvecf32x4() Op

[c: 'BinaryenDivVecF32x4']
pub fn divvecf32x4() Op

[c: 'BinaryenMinVecF32x4']
pub fn minvecf32x4() Op

[c: 'BinaryenMaxVecF32x4']
pub fn maxvecf32x4() Op

[c: 'BinaryenPMinVecF32x4']
pub fn pminvecf32x4() Op

[c: 'BinaryenPMaxVecF32x4']
pub fn pmaxvecf32x4() Op

[c: 'BinaryenCeilVecF32x4']
pub fn ceilvecf32x4() Op

[c: 'BinaryenFloorVecF32x4']
pub fn floorvecf32x4() Op

[c: 'BinaryenTruncVecF32x4']
pub fn truncvecf32x4() Op

[c: 'BinaryenNearestVecF32x4']
pub fn nearestvecf32x4() Op

[c: 'BinaryenAbsVecF64x2']
pub fn absvecf64x2() Op

[c: 'BinaryenNegVecF64x2']
pub fn negvecf64x2() Op

[c: 'BinaryenSqrtVecF64x2']
pub fn sqrtvecf64x2() Op

[c: 'BinaryenAddVecF64x2']
pub fn addvecf64x2() Op

[c: 'BinaryenSubVecF64x2']
pub fn subvecf64x2() Op

[c: 'BinaryenMulVecF64x2']
pub fn mulvecf64x2() Op

[c: 'BinaryenDivVecF64x2']
pub fn divvecf64x2() Op

[c: 'BinaryenMinVecF64x2']
pub fn minvecf64x2() Op

[c: 'BinaryenMaxVecF64x2']
pub fn maxvecf64x2() Op

[c: 'BinaryenPMinVecF64x2']
pub fn pminvecf64x2() Op

[c: 'BinaryenPMaxVecF64x2']
pub fn pmaxvecf64x2() Op

[c: 'BinaryenCeilVecF64x2']
pub fn ceilvecf64x2() Op

[c: 'BinaryenFloorVecF64x2']
pub fn floorvecf64x2() Op

[c: 'BinaryenTruncVecF64x2']
pub fn truncvecf64x2() Op

[c: 'BinaryenNearestVecF64x2']
pub fn nearestvecf64x2() Op

[c: 'BinaryenExtAddPairwiseSVecI8x16ToI16x8']
pub fn extaddpairwisesveci8x16toi16x8() Op

[c: 'BinaryenExtAddPairwiseUVecI8x16ToI16x8']
pub fn extaddpairwiseuveci8x16toi16x8() Op

[c: 'BinaryenExtAddPairwiseSVecI16x8ToI32x4']
pub fn extaddpairwisesveci16x8toi32x4() Op

[c: 'BinaryenExtAddPairwiseUVecI16x8ToI32x4']
pub fn extaddpairwiseuveci16x8toi32x4() Op

[c: 'BinaryenTruncSatSVecF32x4ToVecI32x4']
pub fn truncsatsvecf32x4toveci32x4() Op

[c: 'BinaryenTruncSatUVecF32x4ToVecI32x4']
pub fn truncsatuvecf32x4toveci32x4() Op

[c: 'BinaryenConvertSVecI32x4ToVecF32x4']
pub fn convertsveci32x4tovecf32x4() Op

[c: 'BinaryenConvertUVecI32x4ToVecF32x4']
pub fn convertuveci32x4tovecf32x4() Op

[c: 'BinaryenLoad8SplatVec128']
pub fn load8splatvec128() Op

[c: 'BinaryenLoad16SplatVec128']
pub fn load16splatvec128() Op

[c: 'BinaryenLoad32SplatVec128']
pub fn load32splatvec128() Op

[c: 'BinaryenLoad64SplatVec128']
pub fn load64splatvec128() Op

[c: 'BinaryenLoad8x8SVec128']
pub fn load8x8svec128() Op

[c: 'BinaryenLoad8x8UVec128']
pub fn load8x8uvec128() Op

[c: 'BinaryenLoad16x4SVec128']
pub fn load16x4svec128() Op

[c: 'BinaryenLoad16x4UVec128']
pub fn load16x4uvec128() Op

[c: 'BinaryenLoad32x2SVec128']
pub fn load32x2svec128() Op

[c: 'BinaryenLoad32x2UVec128']
pub fn load32x2uvec128() Op

[c: 'BinaryenLoad32ZeroVec128']
pub fn load32zerovec128() Op

[c: 'BinaryenLoad64ZeroVec128']
pub fn load64zerovec128() Op

[c: 'BinaryenLoad8LaneVec128']
pub fn load8lanevec128() Op

[c: 'BinaryenLoad16LaneVec128']
pub fn load16lanevec128() Op

[c: 'BinaryenLoad32LaneVec128']
pub fn load32lanevec128() Op

[c: 'BinaryenLoad64LaneVec128']
pub fn load64lanevec128() Op

[c: 'BinaryenStore8LaneVec128']
pub fn store8lanevec128() Op

[c: 'BinaryenStore16LaneVec128']
pub fn store16lanevec128() Op

[c: 'BinaryenStore32LaneVec128']
pub fn store32lanevec128() Op

[c: 'BinaryenStore64LaneVec128']
pub fn store64lanevec128() Op

[c: 'BinaryenNarrowSVecI16x8ToVecI8x16']
pub fn narrowsveci16x8toveci8x16() Op

[c: 'BinaryenNarrowUVecI16x8ToVecI8x16']
pub fn narrowuveci16x8toveci8x16() Op

[c: 'BinaryenNarrowSVecI32x4ToVecI16x8']
pub fn narrowsveci32x4toveci16x8() Op

[c: 'BinaryenNarrowUVecI32x4ToVecI16x8']
pub fn narrowuveci32x4toveci16x8() Op

[c: 'BinaryenExtendLowSVecI8x16ToVecI16x8']
pub fn extendlowsveci8x16toveci16x8() Op

[c: 'BinaryenExtendHighSVecI8x16ToVecI16x8']
pub fn extendhighsveci8x16toveci16x8() Op

[c: 'BinaryenExtendLowUVecI8x16ToVecI16x8']
pub fn extendlowuveci8x16toveci16x8() Op

[c: 'BinaryenExtendHighUVecI8x16ToVecI16x8']
pub fn extendhighuveci8x16toveci16x8() Op

[c: 'BinaryenExtendLowSVecI16x8ToVecI32x4']
pub fn extendlowsveci16x8toveci32x4() Op

[c: 'BinaryenExtendHighSVecI16x8ToVecI32x4']
pub fn extendhighsveci16x8toveci32x4() Op

[c: 'BinaryenExtendLowUVecI16x8ToVecI32x4']
pub fn extendlowuveci16x8toveci32x4() Op

[c: 'BinaryenExtendHighUVecI16x8ToVecI32x4']
pub fn extendhighuveci16x8toveci32x4() Op

[c: 'BinaryenExtendLowSVecI32x4ToVecI64x2']
pub fn extendlowsveci32x4toveci64x2() Op

[c: 'BinaryenExtendHighSVecI32x4ToVecI64x2']
pub fn extendhighsveci32x4toveci64x2() Op

[c: 'BinaryenExtendLowUVecI32x4ToVecI64x2']
pub fn extendlowuveci32x4toveci64x2() Op

[c: 'BinaryenExtendHighUVecI32x4ToVecI64x2']
pub fn extendhighuveci32x4toveci64x2() Op

[c: 'BinaryenConvertLowSVecI32x4ToVecF64x2']
pub fn convertlowsveci32x4tovecf64x2() Op

[c: 'BinaryenConvertLowUVecI32x4ToVecF64x2']
pub fn convertlowuveci32x4tovecf64x2() Op

[c: 'BinaryenTruncSatZeroSVecF64x2ToVecI32x4']
pub fn truncsatzerosvecf64x2toveci32x4() Op

[c: 'BinaryenTruncSatZeroUVecF64x2ToVecI32x4']
pub fn truncsatzerouvecf64x2toveci32x4() Op

[c: 'BinaryenDemoteZeroVecF64x2ToVecF32x4']
pub fn demotezerovecf64x2tovecf32x4() Op

[c: 'BinaryenPromoteLowVecF32x4ToVecF64x2']
pub fn promotelowvecf32x4tovecf64x2() Op

[c: 'BinaryenSwizzleVecI8x16']
pub fn swizzleveci8x16() Op

[c: 'BinaryenRefIsNull']
pub fn refisnull() Op

[c: 'BinaryenRefIsFunc']
pub fn refisfunc() Op

[c: 'BinaryenRefIsData']
pub fn refisdata() Op

[c: 'BinaryenRefIsI31']
pub fn refisi31() Op

[c: 'BinaryenRefAsNonNull']
pub fn refasnonnull() Op

[c: 'BinaryenRefAsFunc']
pub fn refasfunc() Op

[c: 'BinaryenRefAsData']
pub fn refasdata() Op

[c: 'BinaryenRefAsI31']
pub fn refasi31() Op

[c: 'BinaryenRefAsExternInternalize']
pub fn refasexterninternalize() Op

[c: 'BinaryenRefAsExternExternalize']
pub fn refasexternexternalize() Op

[c: 'BinaryenBrOnNull']
pub fn bronnull() Op

[c: 'BinaryenBrOnNonNull']
pub fn bronnonnull() Op

[c: 'BinaryenBrOnCast']
pub fn broncast() Op

[c: 'BinaryenBrOnCastFail']
pub fn broncastfail() Op

[c: 'BinaryenBrOnFunc']
pub fn bronfunc() Op

[c: 'BinaryenBrOnNonFunc']
pub fn bronnonfunc() Op

[c: 'BinaryenBrOnData']
pub fn brondata() Op

[c: 'BinaryenBrOnNonData']
pub fn bronnondata() Op

[c: 'BinaryenBrOnI31']
pub fn broni31() Op

[c: 'BinaryenBrOnNonI31']
pub fn bronnoni31() Op

[c: 'BinaryenStringNewUTF8']
pub fn stringnewutf8() Op

[c: 'BinaryenStringNewWTF8']
pub fn stringnewwtf8() Op

[c: 'BinaryenStringNewReplace']
pub fn stringnewreplace() Op

[c: 'BinaryenStringNewWTF16']
pub fn stringnewwtf16() Op

[c: 'BinaryenStringNewUTF8Array']
pub fn stringnewutf8array() Op

[c: 'BinaryenStringNewWTF8Array']
pub fn stringnewwtf8array() Op

[c: 'BinaryenStringNewReplaceArray']
pub fn stringnewreplacearray() Op

[c: 'BinaryenStringNewWTF16Array']
pub fn stringnewwtf16array() Op

[c: 'BinaryenStringMeasureUTF8']
pub fn stringmeasureutf8() Op

[c: 'BinaryenStringMeasureWTF8']
pub fn stringmeasurewtf8() Op

[c: 'BinaryenStringMeasureWTF16']
pub fn stringmeasurewtf16() Op

[c: 'BinaryenStringMeasureIsUSV']
pub fn stringmeasureisusv() Op

[c: 'BinaryenStringMeasureWTF16View']
pub fn stringmeasurewtf16view() Op

[c: 'BinaryenStringEncodeUTF8']
pub fn stringencodeutf8() Op

[c: 'BinaryenStringEncodeWTF8']
pub fn stringencodewtf8() Op

[c: 'BinaryenStringEncodeWTF16']
pub fn stringencodewtf16() Op

[c: 'BinaryenStringEncodeUTF8Array']
pub fn stringencodeutf8array() Op

[c: 'BinaryenStringEncodeWTF8Array']
pub fn stringencodewtf8array() Op

[c: 'BinaryenStringEncodeWTF16Array']
pub fn stringencodewtf16array() Op

[c: 'BinaryenStringAsWTF8']
pub fn stringaswtf8() Op

[c: 'BinaryenStringAsWTF16']
pub fn stringaswtf16() Op

[c: 'BinaryenStringAsIter']
pub fn stringasiter() Op

[c: 'BinaryenStringIterMoveAdvance']
pub fn stringitermoveadvance() Op

[c: 'BinaryenStringIterMoveRewind']
pub fn stringitermoverewind() Op

[c: 'BinaryenStringSliceWTF8']
pub fn stringslicewtf8() Op

[c: 'BinaryenStringSliceWTF16']
pub fn stringslicewtf16() Op

type Expression = voidptr

[c: 'BinaryenBlock']
pub fn block(module_ Module, name &i8, children &Expression, numchildren Index, type_ Type) Expression

[c: 'BinaryenIf']
pub fn bif(module_ Module, condition Expression, iftrue Expression, iffalse Expression) Expression

[c: 'BinaryenLoop']
pub fn loop(module_ Module, in_ &i8, body Expression) Expression

[c: 'BinaryenBreak']
pub fn br(module_ Module, name &i8, condition Expression, value Expression) Expression

[c: 'BinaryenSwitch']
pub fn switch(module_ Module, names &&u8, numnames Index, defaultname &i8, condition Expression, value Expression) Expression

[c: 'BinaryenCall']
pub fn call(module_ Module, target &i8, operands &Expression, numoperands Index, returntype Type) Expression

[c: 'BinaryenCallIndirect']
pub fn callindirect(module_ Module, table &i8, target Expression, operands &Expression, numoperands Index, params Type, results Type) Expression

[c: 'BinaryenReturnCall']
pub fn returncall(module_ Module, target &i8, operands &Expression, numoperands Index, returntype Type) Expression

[c: 'BinaryenReturnCallIndirect']
pub fn returncallindirect(module_ Module, table &i8, target Expression, operands &Expression, numoperands Index, params Type, results Type) Expression

[c: 'BinaryenLocalGet']
pub fn localget(module_ Module, index Index, type_ Type) Expression

[c: 'BinaryenLocalSet']
pub fn localset(module_ Module, index Index, value Expression) Expression

[c: 'BinaryenLocalTee']
pub fn localtee(module_ Module, index Index, value Expression, type_ Type) Expression

[c: 'BinaryenGlobalGet']
pub fn globalget(module_ Module, name &i8, type_ Type) Expression

[c: 'BinaryenGlobalSet']
pub fn globalset(module_ Module, name &i8, value Expression) Expression

[c: 'BinaryenLoad']
pub fn load(module_ Module, bytes u32, signed_ bool, offset u32, align u32, type_ Type, ptr Expression, memoryname &i8) Expression

[c: 'BinaryenStore']
pub fn store(module_ Module, bytes u32, offset u32, align u32, ptr Expression, value Expression, type_ Type, memoryname &i8) Expression

[c: 'BinaryenConst']
pub fn constant(module_ Module, value Literal) Expression

[c: 'BinaryenUnary']
pub fn unary(module_ Module, op Op, value Expression) Expression

[c: 'BinaryenBinary']
pub fn binary(module_ Module, op Op, left Expression, right Expression) Expression

[c: 'BinaryenSelect']
pub fn bselect(module_ Module, condition Expression, iftrue Expression, iffalse Expression, type_ Type) Expression

[c: 'BinaryenDrop']
pub fn drop(module_ Module, value Expression) Expression

[c: 'BinaryenReturn']
pub fn ret(module_ Module, value Expression) Expression

[c: 'BinaryenMemorySize']
pub fn memorysize(module_ Module, memoryname &i8, memoryis64 bool) Expression

[c: 'BinaryenMemoryGrow']
pub fn memorygrow(module_ Module, delta Expression, memoryname &i8, memoryis64 bool) Expression

[c: 'BinaryenNop']
pub fn nop(module_ Module) Expression

[c: 'BinaryenUnreachable']
pub fn unreachable(module_ Module) Expression

[c: 'BinaryenAtomicLoad']
pub fn atomicload(module_ Module, bytes u32, offset u32, type_ Type, ptr Expression, memoryname &i8) Expression

[c: 'BinaryenAtomicStore']
pub fn atomicstore(module_ Module, bytes u32, offset u32, ptr Expression, value Expression, type_ Type, memoryname &i8) Expression

[c: 'BinaryenAtomicRMW']
pub fn atomicrmw(module_ Module, op Op, bytes Index, offset Index, ptr Expression, value Expression, type_ Type, memoryname &i8) Expression

[c: 'BinaryenAtomicCmpxchg']
pub fn atomiccmpxchg(module_ Module, bytes Index, offset Index, ptr Expression, expected Expression, replacement Expression, type_ Type, memoryname &i8) Expression

[c: 'BinaryenAtomicWait']
pub fn atomicwait(module_ Module, ptr Expression, expected Expression, timeout Expression, type_ Type, memoryname &i8) Expression

[c: 'BinaryenAtomicNotify']
pub fn atomicnotify(module_ Module, ptr Expression, notifycount Expression, memoryname &i8) Expression

[c: 'BinaryenAtomicFence']
pub fn atomicfence(module_ Module) Expression

[c: 'BinaryenSIMDExtract']
pub fn simdextract(module_ Module, op Op, vec Expression, index u8) Expression

[c: 'BinaryenSIMDReplace']
pub fn simdreplace(module_ Module, op Op, vec Expression, index u8, value Expression) Expression

[c: 'BinaryenSIMDShuffle']
pub fn simdshuffle(module_ Module, left Expression, right Expression, mask &u8) Expression

[c: 'BinaryenSIMDTernary']
pub fn simdternary(module_ Module, op Op, a Expression, b Expression, c Expression) Expression

[c: 'BinaryenSIMDShift']
pub fn simdshift(module_ Module, op Op, vec Expression, shift Expression) Expression

[c: 'BinaryenSIMDLoad']
pub fn simdload(module_ Module, op Op, offset u32, align u32, ptr Expression, name &i8) Expression

[c: 'BinaryenSIMDLoadStoreLane']
pub fn simdloadstorelane(module_ Module, op Op, offset u32, align u32, index u8, ptr Expression, vec Expression, memoryname &i8) Expression

[c: 'BinaryenMemoryInit']
pub fn memoryinit(module_ Module, segment u32, dest Expression, offset Expression, size Expression, memoryname &i8) Expression

[c: 'BinaryenDataDrop']
pub fn datadrop(module_ Module, segment u32) Expression

[c: 'BinaryenMemoryCopy']
pub fn memorycopy(module_ Module, dest Expression, source Expression, size Expression, destmemory &i8, sourcememory &i8) Expression

[c: 'BinaryenMemoryFill']
pub fn memoryfill(module_ Module, dest Expression, value Expression, size Expression, memoryname &i8) Expression

[c: 'BinaryenRefNull']
pub fn refnull(module_ Module, type_ Type) Expression

[c: 'BinaryenRefIs']
pub fn refis(module_ Module, op Op, value Expression) Expression

[c: 'BinaryenRefAs']
pub fn refas(module_ Module, op Op, value Expression) Expression

[c: 'BinaryenRefFunc']
pub fn reffunc(module_ Module, func &i8, type_ Type) Expression

[c: 'BinaryenRefEq']
pub fn refeq(module_ Module, left Expression, right Expression) Expression

[c: 'BinaryenTableGet']
pub fn tableget(module_ Module, name &i8, index Expression, type_ Type) Expression

[c: 'BinaryenTableSet']
pub fn tableset(module_ Module, name &i8, index Expression, value Expression) Expression

[c: 'BinaryenTableSize']
pub fn tablesize(module_ Module, name &i8) Expression

[c: 'BinaryenTableGrow']
pub fn tablegrow(module_ Module, name &i8, value Expression, delta Expression) Expression

[c: 'BinaryenTry']
pub fn try(module_ Module, name &i8, body Expression, catchtags &&u8, numcatchtags Index, catchbodies &Expression, numcatchbodies Index, delegatetarget &i8) Expression

[c: 'BinaryenThrow']
pub fn throw(module_ Module, tag &i8, operands &Expression, numoperands Index) Expression

[c: 'BinaryenRethrow']
pub fn rethrow(module_ Module, target &i8) Expression

[c: 'BinaryenTupleMake']
pub fn tuplemake(module_ Module, operands &Expression, numoperands Index) Expression

[c: 'BinaryenTupleExtract']
pub fn tupleextract(module_ Module, tuple Expression, index Index) Expression

[c: 'BinaryenPop']
pub fn pop(module_ Module, type_ Type) Expression

[c: 'BinaryenI31New']
pub fn i31new(module_ Module, value Expression) Expression

[c: 'BinaryenI31Get']
pub fn i31get(module_ Module, i31 Expression, signed_ bool) Expression

[c: 'BinaryenCallRef']
pub fn callref(module_ Module, target Expression, operands &Expression, numoperands Index, type_ Type, isreturn bool) Expression

[c: 'BinaryenRefTest']
pub fn reftest(module_ Module, ref Expression, intendedtype HeapType) Expression

[c: 'BinaryenRefCast']
pub fn refcast(module_ Module, ref Expression, intendedtype HeapType) Expression

[c: 'BinaryenBrOn']
pub fn bron(module_ Module, op Op, name &i8, ref Expression, intendedtype HeapType) Expression

[c: 'BinaryenStructNew']
pub fn structnew(module_ Module, operands &Expression, numoperands Index, type_ HeapType) Expression

[c: 'BinaryenStructGet']
pub fn structget(module_ Module, index Index, ref Expression, type_ Type, signed_ bool) Expression

[c: 'BinaryenStructSet']
pub fn structset(module_ Module, index Index, ref Expression, value Expression) Expression

[c: 'BinaryenArrayNew']
pub fn arraynew(module_ Module, type_ HeapType, size Expression, init Expression) Expression

[c: 'BinaryenArrayInit']
pub fn arrayinit(module_ Module, type_ HeapType, values &Expression, numvalues Index) Expression

[c: 'BinaryenArrayGet']
pub fn arrayget(module_ Module, ref Expression, index Expression, type_ Type, signed_ bool) Expression

[c: 'BinaryenArraySet']
pub fn arrayset(module_ Module, ref Expression, index Expression, value Expression) Expression

[c: 'BinaryenArrayLen']
pub fn arraylen(module_ Module, ref Expression) Expression

[c: 'BinaryenArrayCopy']
pub fn arraycopy(module_ Module, destref Expression, destindex Expression, srcref Expression, srcindex Expression, length Expression) Expression

[c: 'BinaryenStringNew']
pub fn stringnew(module_ Module, op Op, ptr Expression, length Expression, start Expression, end Expression) Expression

[c: 'BinaryenStringConst']
pub fn stringconst(module_ Module, name &i8) Expression

[c: 'BinaryenStringMeasure']
pub fn stringmeasure(module_ Module, op Op, ref Expression) Expression

[c: 'BinaryenStringEncode']
pub fn stringencode(module_ Module, op Op, ref Expression, ptr Expression, start Expression) Expression

[c: 'BinaryenStringConcat']
pub fn stringconcat(module_ Module, left Expression, right Expression) Expression

[c: 'BinaryenStringEq']
pub fn stringeq(module_ Module, left Expression, right Expression) Expression

[c: 'BinaryenStringAs']
pub fn stringas(module_ Module, op Op, ref Expression) Expression

[c: 'BinaryenStringWTF8Advance']
pub fn stringwtf8advance(module_ Module, ref Expression, pos Expression, bytes Expression) Expression

[c: 'BinaryenStringWTF16Get']
pub fn stringwtf16get(module_ Module, ref Expression, pos Expression) Expression

[c: 'BinaryenStringIterNext']
pub fn stringiternext(module_ Module, ref Expression) Expression

[c: 'BinaryenStringIterMove']
pub fn stringitermove(module_ Module, op Op, ref Expression, num Expression) Expression

[c: 'BinaryenStringSliceWTF']
pub fn stringslicewtf(module_ Module, op Op, ref Expression, start Expression, end Expression) Expression

[c: 'BinaryenStringSliceIter']
pub fn stringsliceiter(module_ Module, ref Expression, num Expression) Expression

[c: 'BinaryenExpressionGetId']
pub fn expressiongetid(expr Expression) ExpressionId

[c: 'BinaryenExpressionGetType']
pub fn expressiongettype(expr Expression) Type

[c: 'BinaryenExpressionSetType']
pub fn expressionsettype(expr Expression, type_ Type)

[c: 'BinaryenExpressionPrint']
pub fn expressionprint(expr Expression)

[c: 'BinaryenExpressionFinalize']
pub fn expressionfinalize(expr Expression)

[c: 'BinaryenExpressionCopy']
pub fn expressioncopy(expr Expression, module_ Module) Expression

[c: 'BinaryenBlockGetName']
pub fn blockgetname(expr Expression) &i8

[c: 'BinaryenBlockSetName']
pub fn blocksetname(expr Expression, name &i8)

[c: 'BinaryenBlockGetNumChildren']
pub fn blockgetnumchildren(expr Expression) Index

[c: 'BinaryenBlockGetChildAt']
pub fn blockgetchildat(expr Expression, index Index) Expression

[c: 'BinaryenBlockSetChildAt']
pub fn blocksetchildat(expr Expression, index Index, childexpr Expression)

[c: 'BinaryenBlockAppendChild']
pub fn blockappendchild(expr Expression, childexpr Expression) Index

[c: 'BinaryenBlockInsertChildAt']
pub fn blockinsertchildat(expr Expression, index Index, childexpr Expression)

[c: 'BinaryenBlockRemoveChildAt']
pub fn blockremovechildat(expr Expression, index Index) Expression

[c: 'BinaryenIfGetCondition']
pub fn ifgetcondition(expr Expression) Expression

[c: 'BinaryenIfSetCondition']
pub fn ifsetcondition(expr Expression, condexpr Expression)

[c: 'BinaryenIfGetIfTrue']
pub fn ifgetiftrue(expr Expression) Expression

[c: 'BinaryenIfSetIfTrue']
pub fn ifsetiftrue(expr Expression, iftrueexpr Expression)

[c: 'BinaryenIfGetIfFalse']
pub fn ifgetiffalse(expr Expression) Expression

[c: 'BinaryenIfSetIfFalse']
pub fn ifsetiffalse(expr Expression, iffalseexpr Expression)

[c: 'BinaryenLoopGetName']
pub fn loopgetname(expr Expression) &i8

[c: 'BinaryenLoopSetName']
pub fn loopsetname(expr Expression, name &i8)

[c: 'BinaryenLoopGetBody']
pub fn loopgetbody(expr Expression) Expression

[c: 'BinaryenLoopSetBody']
pub fn loopsetbody(expr Expression, bodyexpr Expression)

[c: 'BinaryenBreakGetName']
pub fn breakgetname(expr Expression) &i8

[c: 'BinaryenBreakSetName']
pub fn breaksetname(expr Expression, name &i8)

[c: 'BinaryenBreakGetCondition']
pub fn breakgetcondition(expr Expression) Expression

[c: 'BinaryenBreakSetCondition']
pub fn breaksetcondition(expr Expression, condexpr Expression)

[c: 'BinaryenBreakGetValue']
pub fn breakgetvalue(expr Expression) Expression

[c: 'BinaryenBreakSetValue']
pub fn breaksetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenSwitchGetNumNames']
pub fn switchgetnumnames(expr Expression) Index

[c: 'BinaryenSwitchGetNameAt']
pub fn switchgetnameat(expr Expression, index Index) &i8

[c: 'BinaryenSwitchSetNameAt']
pub fn switchsetnameat(expr Expression, index Index, name &i8)

[c: 'BinaryenSwitchAppendName']
pub fn switchappendname(expr Expression, name &i8) Index

[c: 'BinaryenSwitchInsertNameAt']
pub fn switchinsertnameat(expr Expression, index Index, name &i8)

[c: 'BinaryenSwitchRemoveNameAt']
pub fn switchremovenameat(expr Expression, index Index) &i8

[c: 'BinaryenSwitchGetDefaultName']
pub fn switchgetdefaultname(expr Expression) &i8

[c: 'BinaryenSwitchSetDefaultName']
pub fn switchsetdefaultname(expr Expression, name &i8)

[c: 'BinaryenSwitchGetCondition']
pub fn switchgetcondition(expr Expression) Expression

[c: 'BinaryenSwitchSetCondition']
pub fn switchsetcondition(expr Expression, condexpr Expression)

[c: 'BinaryenSwitchGetValue']
pub fn switchgetvalue(expr Expression) Expression

[c: 'BinaryenSwitchSetValue']
pub fn switchsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenCallGetTarget']
pub fn callgettarget(expr Expression) &i8

[c: 'BinaryenCallSetTarget']
pub fn callsettarget(expr Expression, target &i8)

[c: 'BinaryenCallGetNumOperands']
pub fn callgetnumoperands(expr Expression) Index

[c: 'BinaryenCallGetOperandAt']
pub fn callgetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallSetOperandAt']
pub fn callsetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallAppendOperand']
pub fn callappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenCallInsertOperandAt']
pub fn callinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallRemoveOperandAt']
pub fn callremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallIsReturn']
pub fn callisreturn(expr Expression) bool

[c: 'BinaryenCallSetReturn']
pub fn callsetreturn(expr Expression, isreturn bool)

[c: 'BinaryenCallIndirectGetTarget']
pub fn callindirectgettarget(expr Expression) Expression

[c: 'BinaryenCallIndirectSetTarget']
pub fn callindirectsettarget(expr Expression, targetexpr Expression)

[c: 'BinaryenCallIndirectGetTable']
pub fn callindirectgettable(expr Expression) &i8

[c: 'BinaryenCallIndirectSetTable']
pub fn callindirectsettable(expr Expression, table &i8)

[c: 'BinaryenCallIndirectGetNumOperands']
pub fn callindirectgetnumoperands(expr Expression) Index

[c: 'BinaryenCallIndirectGetOperandAt']
pub fn callindirectgetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallIndirectSetOperandAt']
pub fn callindirectsetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallIndirectAppendOperand']
pub fn callindirectappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenCallIndirectInsertOperandAt']
pub fn callindirectinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallIndirectRemoveOperandAt']
pub fn callindirectremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallIndirectIsReturn']
pub fn callindirectisreturn(expr Expression) bool

[c: 'BinaryenCallIndirectSetReturn']
pub fn callindirectsetreturn(expr Expression, isreturn bool)

[c: 'BinaryenCallIndirectGetParams']
pub fn callindirectgetparams(expr Expression) Type

[c: 'BinaryenCallIndirectSetParams']
pub fn callindirectsetparams(expr Expression, params Type)

[c: 'BinaryenCallIndirectGetResults']
pub fn callindirectgetresults(expr Expression) Type

[c: 'BinaryenCallIndirectSetResults']
pub fn callindirectsetresults(expr Expression, params Type)

[c: 'BinaryenLocalGetGetIndex']
pub fn localgetgetindex(expr Expression) Index

[c: 'BinaryenLocalGetSetIndex']
pub fn localgetsetindex(expr Expression, index Index)

[c: 'BinaryenLocalSetIsTee']
pub fn localsetistee(expr Expression) bool

[c: 'BinaryenLocalSetGetIndex']
pub fn localsetgetindex(expr Expression) Index

[c: 'BinaryenLocalSetSetIndex']
pub fn localsetsetindex(expr Expression, index Index)

[c: 'BinaryenLocalSetGetValue']
pub fn localsetgetvalue(expr Expression) Expression

[c: 'BinaryenLocalSetSetValue']
pub fn localsetsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenGlobalGetGetName']
pub fn globalgetgetname(expr Expression) &i8

[c: 'BinaryenGlobalGetSetName']
pub fn globalgetsetname(expr Expression, name &i8)

[c: 'BinaryenGlobalSetGetName']
pub fn globalsetgetname(expr Expression) &i8

[c: 'BinaryenGlobalSetSetName']
pub fn globalsetsetname(expr Expression, name &i8)

[c: 'BinaryenGlobalSetGetValue']
pub fn globalsetgetvalue(expr Expression) Expression

[c: 'BinaryenGlobalSetSetValue']
pub fn globalsetsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenTableGetGetTable']
pub fn tablegetgettable(expr Expression) &i8

[c: 'BinaryenTableGetSetTable']
pub fn tablegetsettable(expr Expression, table &i8)

[c: 'BinaryenTableGetGetIndex']
pub fn tablegetgetindex(expr Expression) Expression

[c: 'BinaryenTableGetSetIndex']
pub fn tablegetsetindex(expr Expression, indexexpr Expression)

[c: 'BinaryenTableSetGetTable']
pub fn tablesetgettable(expr Expression) &i8

[c: 'BinaryenTableSetSetTable']
pub fn tablesetsettable(expr Expression, table &i8)

[c: 'BinaryenTableSetGetIndex']
pub fn tablesetgetindex(expr Expression) Expression

[c: 'BinaryenTableSetSetIndex']
pub fn tablesetsetindex(expr Expression, indexexpr Expression)

[c: 'BinaryenTableSetGetValue']
pub fn tablesetgetvalue(expr Expression) Expression

[c: 'BinaryenTableSetSetValue']
pub fn tablesetsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenTableSizeGetTable']
pub fn tablesizegettable(expr Expression) &i8

[c: 'BinaryenTableSizeSetTable']
pub fn tablesizesettable(expr Expression, table &i8)

[c: 'BinaryenTableGrowGetTable']
pub fn tablegrowgettable(expr Expression) &i8

[c: 'BinaryenTableGrowSetTable']
pub fn tablegrowsettable(expr Expression, table &i8)

[c: 'BinaryenTableGrowGetValue']
pub fn tablegrowgetvalue(expr Expression) Expression

[c: 'BinaryenTableGrowSetValue']
pub fn tablegrowsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenTableGrowGetDelta']
pub fn tablegrowgetdelta(expr Expression) Expression

[c: 'BinaryenTableGrowSetDelta']
pub fn tablegrowsetdelta(expr Expression, deltaexpr Expression)

[c: 'BinaryenMemoryGrowGetDelta']
pub fn memorygrowgetdelta(expr Expression) Expression

[c: 'BinaryenMemoryGrowSetDelta']
pub fn memorygrowsetdelta(expr Expression, deltaexpr Expression)

[c: 'BinaryenLoadIsAtomic']
pub fn loadisatomic(expr Expression) bool

[c: 'BinaryenLoadSetAtomic']
pub fn loadsetatomic(expr Expression, isatomic bool)

[c: 'BinaryenLoadIsSigned']
pub fn loadissigned(expr Expression) bool

[c: 'BinaryenLoadSetSigned']
pub fn loadsetsigned(expr Expression, issigned bool)

[c: 'BinaryenLoadGetOffset']
pub fn loadgetoffset(expr Expression) u32

[c: 'BinaryenLoadSetOffset']
pub fn loadsetoffset(expr Expression, offset u32)

[c: 'BinaryenLoadGetBytes']
pub fn loadgetbytes(expr Expression) u32

[c: 'BinaryenLoadSetBytes']
pub fn loadsetbytes(expr Expression, bytes u32)

[c: 'BinaryenLoadGetAlign']
pub fn loadgetalign(expr Expression) u32

[c: 'BinaryenLoadSetAlign']
pub fn loadsetalign(expr Expression, align u32)

[c: 'BinaryenLoadGetPtr']
pub fn loadgetptr(expr Expression) Expression

[c: 'BinaryenLoadSetPtr']
pub fn loadsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenStoreIsAtomic']
pub fn storeisatomic(expr Expression) bool

[c: 'BinaryenStoreSetAtomic']
pub fn storesetatomic(expr Expression, isatomic bool)

[c: 'BinaryenStoreGetBytes']
pub fn storegetbytes(expr Expression) u32

[c: 'BinaryenStoreSetBytes']
pub fn storesetbytes(expr Expression, bytes u32)

[c: 'BinaryenStoreGetOffset']
pub fn storegetoffset(expr Expression) u32

[c: 'BinaryenStoreSetOffset']
pub fn storesetoffset(expr Expression, offset u32)

[c: 'BinaryenStoreGetAlign']
pub fn storegetalign(expr Expression) u32

[c: 'BinaryenStoreSetAlign']
pub fn storesetalign(expr Expression, align u32)

[c: 'BinaryenStoreGetPtr']
pub fn storegetptr(expr Expression) Expression

[c: 'BinaryenStoreSetPtr']
pub fn storesetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenStoreGetValue']
pub fn storegetvalue(expr Expression) Expression

[c: 'BinaryenStoreSetValue']
pub fn storesetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenStoreGetValueType']
pub fn storegetvaluetype(expr Expression) Type

[c: 'BinaryenStoreSetValueType']
pub fn storesetvaluetype(expr Expression, valuetype Type)

[c: 'BinaryenConstGetValueI32']
pub fn constgetvaluei32(expr Expression) int

[c: 'BinaryenConstSetValueI32']
pub fn constsetvaluei32(expr Expression, value int)

[c: 'BinaryenConstGetValueI64']
pub fn constgetvaluei64(expr Expression) i64

[c: 'BinaryenConstSetValueI64']
pub fn constsetvaluei64(expr Expression, value i64)

[c: 'BinaryenConstGetValueI64Low']
pub fn constgetvaluei64low(expr Expression) int

[c: 'BinaryenConstSetValueI64Low']
pub fn constsetvaluei64low(expr Expression, valuelow int)

[c: 'BinaryenConstGetValueI64High']
pub fn constgetvaluei64high(expr Expression) int

[c: 'BinaryenConstSetValueI64High']
pub fn constsetvaluei64high(expr Expression, valuehigh int)

[c: 'BinaryenConstGetValueF32']
pub fn constgetvaluef32(expr Expression) f32

[c: 'BinaryenConstSetValueF32']
pub fn constsetvaluef32(expr Expression, value f32)

[c: 'BinaryenConstGetValueF64']
pub fn constgetvaluef64(expr Expression) f64

[c: 'BinaryenConstSetValueF64']
pub fn constsetvaluef64(expr Expression, value f64)

[c: 'BinaryenConstGetValueV128']
pub fn constgetvaluev128(expr Expression, out &u8)

[c: 'BinaryenConstSetValueV128']
pub fn constsetvaluev128(expr Expression, value &u8)

[c: 'BinaryenUnaryGetOp']
pub fn unarygetop(expr Expression) Op

[c: 'BinaryenUnarySetOp']
pub fn unarysetop(expr Expression, op Op)

[c: 'BinaryenUnaryGetValue']
pub fn unarygetvalue(expr Expression) Expression

[c: 'BinaryenUnarySetValue']
pub fn unarysetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenBinaryGetOp']
pub fn binarygetop(expr Expression) Op

[c: 'BinaryenBinarySetOp']
pub fn binarysetop(expr Expression, op Op)

[c: 'BinaryenBinaryGetLeft']
pub fn binarygetleft(expr Expression) Expression

[c: 'BinaryenBinarySetLeft']
pub fn binarysetleft(expr Expression, leftexpr Expression)

[c: 'BinaryenBinaryGetRight']
pub fn binarygetright(expr Expression) Expression

[c: 'BinaryenBinarySetRight']
pub fn binarysetright(expr Expression, rightexpr Expression)

[c: 'BinaryenSelectGetIfTrue']
pub fn selectgetiftrue(expr Expression) Expression

[c: 'BinaryenSelectSetIfTrue']
pub fn selectsetiftrue(expr Expression, iftrueexpr Expression)

[c: 'BinaryenSelectGetIfFalse']
pub fn selectgetiffalse(expr Expression) Expression

[c: 'BinaryenSelectSetIfFalse']
pub fn selectsetiffalse(expr Expression, iffalseexpr Expression)

[c: 'BinaryenSelectGetCondition']
pub fn selectgetcondition(expr Expression) Expression

[c: 'BinaryenSelectSetCondition']
pub fn selectsetcondition(expr Expression, condexpr Expression)

[c: 'BinaryenDropGetValue']
pub fn dropgetvalue(expr Expression) Expression

[c: 'BinaryenDropSetValue']
pub fn dropsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenReturnGetValue']
pub fn returngetvalue(expr Expression) Expression

[c: 'BinaryenReturnSetValue']
pub fn returnsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenAtomicRMWGetOp']
pub fn atomicrmwgetop(expr Expression) Op

[c: 'BinaryenAtomicRMWSetOp']
pub fn atomicrmwsetop(expr Expression, op Op)

[c: 'BinaryenAtomicRMWGetBytes']
pub fn atomicrmwgetbytes(expr Expression) u32

[c: 'BinaryenAtomicRMWSetBytes']
pub fn atomicrmwsetbytes(expr Expression, bytes u32)

[c: 'BinaryenAtomicRMWGetOffset']
pub fn atomicrmwgetoffset(expr Expression) u32

[c: 'BinaryenAtomicRMWSetOffset']
pub fn atomicrmwsetoffset(expr Expression, offset u32)

[c: 'BinaryenAtomicRMWGetPtr']
pub fn atomicrmwgetptr(expr Expression) Expression

[c: 'BinaryenAtomicRMWSetPtr']
pub fn atomicrmwsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenAtomicRMWGetValue']
pub fn atomicrmwgetvalue(expr Expression) Expression

[c: 'BinaryenAtomicRMWSetValue']
pub fn atomicrmwsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenAtomicCmpxchgGetBytes']
pub fn atomiccmpxchggetbytes(expr Expression) u32

[c: 'BinaryenAtomicCmpxchgSetBytes']
pub fn atomiccmpxchgsetbytes(expr Expression, bytes u32)

[c: 'BinaryenAtomicCmpxchgGetOffset']
pub fn atomiccmpxchggetoffset(expr Expression) u32

[c: 'BinaryenAtomicCmpxchgSetOffset']
pub fn atomiccmpxchgsetoffset(expr Expression, offset u32)

[c: 'BinaryenAtomicCmpxchgGetPtr']
pub fn atomiccmpxchggetptr(expr Expression) Expression

[c: 'BinaryenAtomicCmpxchgSetPtr']
pub fn atomiccmpxchgsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenAtomicCmpxchgGetExpected']
pub fn atomiccmpxchggetexpected(expr Expression) Expression

[c: 'BinaryenAtomicCmpxchgSetExpected']
pub fn atomiccmpxchgsetexpected(expr Expression, expectedexpr Expression)

[c: 'BinaryenAtomicCmpxchgGetReplacement']
pub fn atomiccmpxchggetreplacement(expr Expression) Expression

[c: 'BinaryenAtomicCmpxchgSetReplacement']
pub fn atomiccmpxchgsetreplacement(expr Expression, replacementexpr Expression)

[c: 'BinaryenAtomicWaitGetPtr']
pub fn atomicwaitgetptr(expr Expression) Expression

[c: 'BinaryenAtomicWaitSetPtr']
pub fn atomicwaitsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenAtomicWaitGetExpected']
pub fn atomicwaitgetexpected(expr Expression) Expression

[c: 'BinaryenAtomicWaitSetExpected']
pub fn atomicwaitsetexpected(expr Expression, expectedexpr Expression)

[c: 'BinaryenAtomicWaitGetTimeout']
pub fn atomicwaitgettimeout(expr Expression) Expression

[c: 'BinaryenAtomicWaitSetTimeout']
pub fn atomicwaitsettimeout(expr Expression, timeoutexpr Expression)

[c: 'BinaryenAtomicWaitGetExpectedType']
pub fn atomicwaitgetexpectedtype(expr Expression) Type

[c: 'BinaryenAtomicWaitSetExpectedType']
pub fn atomicwaitsetexpectedtype(expr Expression, expectedtype Type)

[c: 'BinaryenAtomicNotifyGetPtr']
pub fn atomicnotifygetptr(expr Expression) Expression

[c: 'BinaryenAtomicNotifySetPtr']
pub fn atomicnotifysetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenAtomicNotifyGetNotifyCount']
pub fn atomicnotifygetnotifycount(expr Expression) Expression

[c: 'BinaryenAtomicNotifySetNotifyCount']
pub fn atomicnotifysetnotifycount(expr Expression, notifycountexpr Expression)

[c: 'BinaryenAtomicFenceGetOrder']
pub fn atomicfencegetorder(expr Expression) u8

[c: 'BinaryenAtomicFenceSetOrder']
pub fn atomicfencesetorder(expr Expression, order u8)

[c: 'BinaryenSIMDExtractGetOp']
pub fn simdextractgetop(expr Expression) Op

[c: 'BinaryenSIMDExtractSetOp']
pub fn simdextractsetop(expr Expression, op Op)

[c: 'BinaryenSIMDExtractGetVec']
pub fn simdextractgetvec(expr Expression) Expression

[c: 'BinaryenSIMDExtractSetVec']
pub fn simdextractsetvec(expr Expression, vecexpr Expression)

[c: 'BinaryenSIMDExtractGetIndex']
pub fn simdextractgetindex(expr Expression) u8

[c: 'BinaryenSIMDExtractSetIndex']
pub fn simdextractsetindex(expr Expression, index u8)

[c: 'BinaryenSIMDReplaceGetOp']
pub fn simdreplacegetop(expr Expression) Op

[c: 'BinaryenSIMDReplaceSetOp']
pub fn simdreplacesetop(expr Expression, op Op)

[c: 'BinaryenSIMDReplaceGetVec']
pub fn simdreplacegetvec(expr Expression) Expression

[c: 'BinaryenSIMDReplaceSetVec']
pub fn simdreplacesetvec(expr Expression, vecexpr Expression)

[c: 'BinaryenSIMDReplaceGetIndex']
pub fn simdreplacegetindex(expr Expression) u8

[c: 'BinaryenSIMDReplaceSetIndex']
pub fn simdreplacesetindex(expr Expression, index u8)

[c: 'BinaryenSIMDReplaceGetValue']
pub fn simdreplacegetvalue(expr Expression) Expression

[c: 'BinaryenSIMDReplaceSetValue']
pub fn simdreplacesetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenSIMDShuffleGetLeft']
pub fn simdshufflegetleft(expr Expression) Expression

[c: 'BinaryenSIMDShuffleSetLeft']
pub fn simdshufflesetleft(expr Expression, leftexpr Expression)

[c: 'BinaryenSIMDShuffleGetRight']
pub fn simdshufflegetright(expr Expression) Expression

[c: 'BinaryenSIMDShuffleSetRight']
pub fn simdshufflesetright(expr Expression, rightexpr Expression)

[c: 'BinaryenSIMDShuffleGetMask']
pub fn simdshufflegetmask(expr Expression, mask &u8)

[c: 'BinaryenSIMDShuffleSetMask']
pub fn simdshufflesetmask(expr Expression, mask &u8)

[c: 'BinaryenSIMDTernaryGetOp']
pub fn simdternarygetop(expr Expression) Op

[c: 'BinaryenSIMDTernarySetOp']
pub fn simdternarysetop(expr Expression, op Op)

[c: 'BinaryenSIMDTernaryGetA']
pub fn simdternarygeta(expr Expression) Expression

[c: 'BinaryenSIMDTernarySetA']
pub fn simdternaryseta(expr Expression, aexpr Expression)

[c: 'BinaryenSIMDTernaryGetB']
pub fn simdternarygetb(expr Expression) Expression

[c: 'BinaryenSIMDTernarySetB']
pub fn simdternarysetb(expr Expression, bexpr Expression)

[c: 'BinaryenSIMDTernaryGetC']
pub fn simdternarygetc(expr Expression) Expression

[c: 'BinaryenSIMDTernarySetC']
pub fn simdternarysetc(expr Expression, cexpr Expression)

[c: 'BinaryenSIMDShiftGetOp']
pub fn simdshiftgetop(expr Expression) Op

[c: 'BinaryenSIMDShiftSetOp']
pub fn simdshiftsetop(expr Expression, op Op)

[c: 'BinaryenSIMDShiftGetVec']
pub fn simdshiftgetvec(expr Expression) Expression

[c: 'BinaryenSIMDShiftSetVec']
pub fn simdshiftsetvec(expr Expression, vecexpr Expression)

[c: 'BinaryenSIMDShiftGetShift']
pub fn simdshiftgetshift(expr Expression) Expression

[c: 'BinaryenSIMDShiftSetShift']
pub fn simdshiftsetshift(expr Expression, shiftexpr Expression)

[c: 'BinaryenSIMDLoadGetOp']
pub fn simdloadgetop(expr Expression) Op

[c: 'BinaryenSIMDLoadSetOp']
pub fn simdloadsetop(expr Expression, op Op)

[c: 'BinaryenSIMDLoadGetOffset']
pub fn simdloadgetoffset(expr Expression) u32

[c: 'BinaryenSIMDLoadSetOffset']
pub fn simdloadsetoffset(expr Expression, offset u32)

[c: 'BinaryenSIMDLoadGetAlign']
pub fn simdloadgetalign(expr Expression) u32

[c: 'BinaryenSIMDLoadSetAlign']
pub fn simdloadsetalign(expr Expression, align u32)

[c: 'BinaryenSIMDLoadGetPtr']
pub fn simdloadgetptr(expr Expression) Expression

[c: 'BinaryenSIMDLoadSetPtr']
pub fn simdloadsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenSIMDLoadStoreLaneGetOp']
pub fn simdloadstorelanegetop(expr Expression) Op

[c: 'BinaryenSIMDLoadStoreLaneSetOp']
pub fn simdloadstorelanesetop(expr Expression, op Op)

[c: 'BinaryenSIMDLoadStoreLaneGetOffset']
pub fn simdloadstorelanegetoffset(expr Expression) u32

[c: 'BinaryenSIMDLoadStoreLaneSetOffset']
pub fn simdloadstorelanesetoffset(expr Expression, offset u32)

[c: 'BinaryenSIMDLoadStoreLaneGetAlign']
pub fn simdloadstorelanegetalign(expr Expression) u32

[c: 'BinaryenSIMDLoadStoreLaneSetAlign']
pub fn simdloadstorelanesetalign(expr Expression, align u32)

[c: 'BinaryenSIMDLoadStoreLaneGetIndex']
pub fn simdloadstorelanegetindex(expr Expression) u8

[c: 'BinaryenSIMDLoadStoreLaneSetIndex']
pub fn simdloadstorelanesetindex(expr Expression, index u8)

[c: 'BinaryenSIMDLoadStoreLaneGetPtr']
pub fn simdloadstorelanegetptr(expr Expression) Expression

[c: 'BinaryenSIMDLoadStoreLaneSetPtr']
pub fn simdloadstorelanesetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenSIMDLoadStoreLaneGetVec']
pub fn simdloadstorelanegetvec(expr Expression) Expression

[c: 'BinaryenSIMDLoadStoreLaneSetVec']
pub fn simdloadstorelanesetvec(expr Expression, vecexpr Expression)

[c: 'BinaryenSIMDLoadStoreLaneIsStore']
pub fn simdloadstorelaneisstore(expr Expression) bool

[c: 'BinaryenMemoryInitGetSegment']
pub fn memoryinitgetsegment(expr Expression) u32

[c: 'BinaryenMemoryInitSetSegment']
pub fn memoryinitsetsegment(expr Expression, segmentindex u32)

[c: 'BinaryenMemoryInitGetDest']
pub fn memoryinitgetdest(expr Expression) Expression

[c: 'BinaryenMemoryInitSetDest']
pub fn memoryinitsetdest(expr Expression, destexpr Expression)

[c: 'BinaryenMemoryInitGetOffset']
pub fn memoryinitgetoffset(expr Expression) Expression

[c: 'BinaryenMemoryInitSetOffset']
pub fn memoryinitsetoffset(expr Expression, offsetexpr Expression)

[c: 'BinaryenMemoryInitGetSize']
pub fn memoryinitgetsize(expr Expression) Expression

[c: 'BinaryenMemoryInitSetSize']
pub fn memoryinitsetsize(expr Expression, sizeexpr Expression)

[c: 'BinaryenDataDropGetSegment']
pub fn datadropgetsegment(expr Expression) u32

[c: 'BinaryenDataDropSetSegment']
pub fn datadropsetsegment(expr Expression, segmentindex u32)

[c: 'BinaryenMemoryCopyGetDest']
pub fn memorycopygetdest(expr Expression) Expression

[c: 'BinaryenMemoryCopySetDest']
pub fn memorycopysetdest(expr Expression, destexpr Expression)

[c: 'BinaryenMemoryCopyGetSource']
pub fn memorycopygetsource(expr Expression) Expression

[c: 'BinaryenMemoryCopySetSource']
pub fn memorycopysetsource(expr Expression, sourceexpr Expression)

[c: 'BinaryenMemoryCopyGetSize']
pub fn memorycopygetsize(expr Expression) Expression

[c: 'BinaryenMemoryCopySetSize']
pub fn memorycopysetsize(expr Expression, sizeexpr Expression)

[c: 'BinaryenMemoryFillGetDest']
pub fn memoryfillgetdest(expr Expression) Expression

[c: 'BinaryenMemoryFillSetDest']
pub fn memoryfillsetdest(expr Expression, destexpr Expression)

[c: 'BinaryenMemoryFillGetValue']
pub fn memoryfillgetvalue(expr Expression) Expression

[c: 'BinaryenMemoryFillSetValue']
pub fn memoryfillsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenMemoryFillGetSize']
pub fn memoryfillgetsize(expr Expression) Expression

[c: 'BinaryenMemoryFillSetSize']
pub fn memoryfillsetsize(expr Expression, sizeexpr Expression)

[c: 'BinaryenRefIsGetOp']
pub fn refisgetop(expr Expression) Op

[c: 'BinaryenRefIsSetOp']
pub fn refissetop(expr Expression, op Op)

[c: 'BinaryenRefIsGetValue']
pub fn refisgetvalue(expr Expression) Expression

[c: 'BinaryenRefIsSetValue']
pub fn refissetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenRefAsGetOp']
pub fn refasgetop(expr Expression) Op

[c: 'BinaryenRefAsSetOp']
pub fn refassetop(expr Expression, op Op)

[c: 'BinaryenRefAsGetValue']
pub fn refasgetvalue(expr Expression) Expression

[c: 'BinaryenRefAsSetValue']
pub fn refassetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenRefFuncGetFunc']
pub fn reffuncgetfunc(expr Expression) &i8

[c: 'BinaryenRefFuncSetFunc']
pub fn reffuncsetfunc(expr Expression, funcname &i8)

[c: 'BinaryenRefEqGetLeft']
pub fn refeqgetleft(expr Expression) Expression

[c: 'BinaryenRefEqSetLeft']
pub fn refeqsetleft(expr Expression, left Expression)

[c: 'BinaryenRefEqGetRight']
pub fn refeqgetright(expr Expression) Expression

[c: 'BinaryenRefEqSetRight']
pub fn refeqsetright(expr Expression, right Expression)

[c: 'BinaryenTryGetName']
pub fn trygetname(expr Expression) &i8

[c: 'BinaryenTrySetName']
pub fn trysetname(expr Expression, name &i8)

[c: 'BinaryenTryGetBody']
pub fn trygetbody(expr Expression) Expression

[c: 'BinaryenTrySetBody']
pub fn trysetbody(expr Expression, bodyexpr Expression)

[c: 'BinaryenTryGetNumCatchTags']
pub fn trygetnumcatchtags(expr Expression) Index

[c: 'BinaryenTryGetNumCatchBodies']
pub fn trygetnumcatchbodies(expr Expression) Index

[c: 'BinaryenTryGetCatchTagAt']
pub fn trygetcatchtagat(expr Expression, index Index) &i8

[c: 'BinaryenTrySetCatchTagAt']
pub fn trysetcatchtagat(expr Expression, index Index, catchtag &i8)

[c: 'BinaryenTryAppendCatchTag']
pub fn tryappendcatchtag(expr Expression, catchtag &i8) Index

[c: 'BinaryenTryInsertCatchTagAt']
pub fn tryinsertcatchtagat(expr Expression, index Index, catchtag &i8)

[c: 'BinaryenTryRemoveCatchTagAt']
pub fn tryremovecatchtagat(expr Expression, index Index) &i8

[c: 'BinaryenTryGetCatchBodyAt']
pub fn trygetcatchbodyat(expr Expression, index Index) Expression

[c: 'BinaryenTrySetCatchBodyAt']
pub fn trysetcatchbodyat(expr Expression, index Index, catchexpr Expression)

[c: 'BinaryenTryAppendCatchBody']
pub fn tryappendcatchbody(expr Expression, catchexpr Expression) Index

[c: 'BinaryenTryInsertCatchBodyAt']
pub fn tryinsertcatchbodyat(expr Expression, index Index, catchexpr Expression)

[c: 'BinaryenTryRemoveCatchBodyAt']
pub fn tryremovecatchbodyat(expr Expression, index Index) Expression

[c: 'BinaryenTryHasCatchAll']
pub fn tryhascatchall(expr Expression) bool

[c: 'BinaryenTryGetDelegateTarget']
pub fn trygetdelegatetarget(expr Expression) &i8

[c: 'BinaryenTrySetDelegateTarget']
pub fn trysetdelegatetarget(expr Expression, delegatetarget &i8)

[c: 'BinaryenTryIsDelegate']
pub fn tryisdelegate(expr Expression) bool

[c: 'BinaryenThrowGetTag']
pub fn throwgettag(expr Expression) &i8

[c: 'BinaryenThrowSetTag']
pub fn throwsettag(expr Expression, tagname &i8)

[c: 'BinaryenThrowGetNumOperands']
pub fn throwgetnumoperands(expr Expression) Index

[c: 'BinaryenThrowGetOperandAt']
pub fn throwgetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenThrowSetOperandAt']
pub fn throwsetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenThrowAppendOperand']
pub fn throwappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenThrowInsertOperandAt']
pub fn throwinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenThrowRemoveOperandAt']
pub fn throwremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenRethrowGetTarget']
pub fn rethrowgettarget(expr Expression) &i8

[c: 'BinaryenRethrowSetTarget']
pub fn rethrowsettarget(expr Expression, target &i8)

[c: 'BinaryenTupleMakeGetNumOperands']
pub fn tuplemakegetnumoperands(expr Expression) Index

[c: 'BinaryenTupleMakeGetOperandAt']
pub fn tuplemakegetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenTupleMakeSetOperandAt']
pub fn tuplemakesetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenTupleMakeAppendOperand']
pub fn tuplemakeappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenTupleMakeInsertOperandAt']
pub fn tuplemakeinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenTupleMakeRemoveOperandAt']
pub fn tuplemakeremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenTupleExtractGetTuple']
pub fn tupleextractgettuple(expr Expression) Expression

[c: 'BinaryenTupleExtractSetTuple']
pub fn tupleextractsettuple(expr Expression, tupleexpr Expression)

[c: 'BinaryenTupleExtractGetIndex']
pub fn tupleextractgetindex(expr Expression) Index

[c: 'BinaryenTupleExtractSetIndex']
pub fn tupleextractsetindex(expr Expression, index Index)

[c: 'BinaryenI31NewGetValue']
pub fn i31newgetvalue(expr Expression) Expression

[c: 'BinaryenI31NewSetValue']
pub fn i31newsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenI31GetGetI31']
pub fn i31getgeti31(expr Expression) Expression

[c: 'BinaryenI31GetSetI31']
pub fn i31getseti31(expr Expression, i31expr Expression)

[c: 'BinaryenI31GetIsSigned']
pub fn i31getissigned(expr Expression) bool

[c: 'BinaryenI31GetSetSigned']
pub fn i31getsetsigned(expr Expression, signed_ bool)

[c: 'BinaryenCallRefGetNumOperands']
pub fn callrefgetnumoperands(expr Expression) Index

[c: 'BinaryenCallRefGetOperandAt']
pub fn callrefgetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallRefSetOperandAt']
pub fn callrefsetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallRefAppendOperand']
pub fn callrefappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenCallRefInsertOperandAt']
pub fn callrefinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenCallRefRemoveOperandAt']
pub fn callrefremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenCallRefGetTarget']
pub fn callrefgettarget(expr Expression) Expression

[c: 'BinaryenCallRefSetTarget']
pub fn callrefsettarget(expr Expression, targetexpr Expression)

[c: 'BinaryenCallRefIsReturn']
pub fn callrefisreturn(expr Expression) bool

[c: 'BinaryenCallRefSetReturn']
pub fn callrefsetreturn(expr Expression, isreturn bool)

[c: 'BinaryenRefTestGetRef']
pub fn reftestgetref(expr Expression) Expression

[c: 'BinaryenRefTestSetRef']
pub fn reftestsetref(expr Expression, refexpr Expression)

[c: 'BinaryenRefTestGetIntendedType']
pub fn reftestgetintendedtype(expr Expression) HeapType

[c: 'BinaryenRefTestSetIntendedType']
pub fn reftestsetintendedtype(expr Expression, intendedtype HeapType)

[c: 'BinaryenRefCastGetRef']
pub fn refcastgetref(expr Expression) Expression

[c: 'BinaryenRefCastSetRef']
pub fn refcastsetref(expr Expression, refexpr Expression)

[c: 'BinaryenRefCastGetIntendedType']
pub fn refcastgetintendedtype(expr Expression) HeapType

[c: 'BinaryenRefCastSetIntendedType']
pub fn refcastsetintendedtype(expr Expression, intendedtype HeapType)

[c: 'BinaryenBrOnGetOp']
pub fn brongetop(expr Expression) Op

[c: 'BinaryenBrOnSetOp']
pub fn bronsetop(expr Expression, op Op)

[c: 'BinaryenBrOnGetName']
pub fn brongetname(expr Expression) &i8

[c: 'BinaryenBrOnSetName']
pub fn bronsetname(expr Expression, namestr &i8)

[c: 'BinaryenBrOnGetRef']
pub fn brongetref(expr Expression) Expression

[c: 'BinaryenBrOnSetRef']
pub fn bronsetref(expr Expression, refexpr Expression)

[c: 'BinaryenBrOnGetIntendedType']
pub fn brongetintendedtype(expr Expression) HeapType

[c: 'BinaryenBrOnSetIntendedType']
pub fn bronsetintendedtype(expr Expression, intendedtype HeapType)

[c: 'BinaryenStructNewGetNumOperands']
pub fn structnewgetnumoperands(expr Expression) Index

[c: 'BinaryenStructNewGetOperandAt']
pub fn structnewgetoperandat(expr Expression, index Index) Expression

[c: 'BinaryenStructNewSetOperandAt']
pub fn structnewsetoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenStructNewAppendOperand']
pub fn structnewappendoperand(expr Expression, operandexpr Expression) Index

[c: 'BinaryenStructNewInsertOperandAt']
pub fn structnewinsertoperandat(expr Expression, index Index, operandexpr Expression)

[c: 'BinaryenStructNewRemoveOperandAt']
pub fn structnewremoveoperandat(expr Expression, index Index) Expression

[c: 'BinaryenStructGetGetIndex']
pub fn structgetgetindex(expr Expression) Index

[c: 'BinaryenStructGetSetIndex']
pub fn structgetsetindex(expr Expression, index Index)

[c: 'BinaryenStructGetGetRef']
pub fn structgetgetref(expr Expression) Expression

[c: 'BinaryenStructGetSetRef']
pub fn structgetsetref(expr Expression, refexpr Expression)

[c: 'BinaryenStructGetIsSigned']
pub fn structgetissigned(expr Expression) bool

[c: 'BinaryenStructGetSetSigned']
pub fn structgetsetsigned(expr Expression, signed_ bool)

[c: 'BinaryenStructSetGetIndex']
pub fn structsetgetindex(expr Expression) Index

[c: 'BinaryenStructSetSetIndex']
pub fn structsetsetindex(expr Expression, index Index)

[c: 'BinaryenStructSetGetRef']
pub fn structsetgetref(expr Expression) Expression

[c: 'BinaryenStructSetSetRef']
pub fn structsetsetref(expr Expression, refexpr Expression)

[c: 'BinaryenStructSetGetValue']
pub fn structsetgetvalue(expr Expression) Expression

[c: 'BinaryenStructSetSetValue']
pub fn structsetsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenArrayNewGetInit']
pub fn arraynewgetinit(expr Expression) Expression

[c: 'BinaryenArrayNewSetInit']
pub fn arraynewsetinit(expr Expression, initexpr Expression)

[c: 'BinaryenArrayNewGetSize']
pub fn arraynewgetsize(expr Expression) Expression

[c: 'BinaryenArrayNewSetSize']
pub fn arraynewsetsize(expr Expression, sizeexpr Expression)

[c: 'BinaryenArrayInitGetNumValues']
pub fn arrayinitgetnumvalues(expr Expression) Index

[c: 'BinaryenArrayInitGetValueAt']
pub fn arrayinitgetvalueat(expr Expression, index Index) Expression

[c: 'BinaryenArrayInitSetValueAt']
pub fn arrayinitsetvalueat(expr Expression, index Index, valueexpr Expression)

[c: 'BinaryenArrayInitAppendValue']
pub fn arrayinitappendvalue(expr Expression, valueexpr Expression) Index

[c: 'BinaryenArrayInitInsertValueAt']
pub fn arrayinitinsertvalueat(expr Expression, index Index, valueexpr Expression)

[c: 'BinaryenArrayInitRemoveValueAt']
pub fn arrayinitremovevalueat(expr Expression, index Index) Expression

[c: 'BinaryenArrayGetGetRef']
pub fn arraygetgetref(expr Expression) Expression

[c: 'BinaryenArrayGetSetRef']
pub fn arraygetsetref(expr Expression, refexpr Expression)

[c: 'BinaryenArrayGetGetIndex']
pub fn arraygetgetindex(expr Expression) Expression

[c: 'BinaryenArrayGetSetIndex']
pub fn arraygetsetindex(expr Expression, indexexpr Expression)

[c: 'BinaryenArrayGetIsSigned']
pub fn arraygetissigned(expr Expression) bool

[c: 'BinaryenArrayGetSetSigned']
pub fn arraygetsetsigned(expr Expression, signed_ bool)

[c: 'BinaryenArraySetGetRef']
pub fn arraysetgetref(expr Expression) Expression

[c: 'BinaryenArraySetSetRef']
pub fn arraysetsetref(expr Expression, refexpr Expression)

[c: 'BinaryenArraySetGetIndex']
pub fn arraysetgetindex(expr Expression) Expression

[c: 'BinaryenArraySetSetIndex']
pub fn arraysetsetindex(expr Expression, indexexpr Expression)

[c: 'BinaryenArraySetGetValue']
pub fn arraysetgetvalue(expr Expression) Expression

[c: 'BinaryenArraySetSetValue']
pub fn arraysetsetvalue(expr Expression, valueexpr Expression)

[c: 'BinaryenArrayLenGetRef']
pub fn arraylengetref(expr Expression) Expression

[c: 'BinaryenArrayLenSetRef']
pub fn arraylensetref(expr Expression, refexpr Expression)

[c: 'BinaryenArrayCopyGetDestRef']
pub fn arraycopygetdestref(expr Expression) Expression

[c: 'BinaryenArrayCopySetDestRef']
pub fn arraycopysetdestref(expr Expression, destrefexpr Expression)

[c: 'BinaryenArrayCopyGetDestIndex']
pub fn arraycopygetdestindex(expr Expression) Expression

[c: 'BinaryenArrayCopySetDestIndex']
pub fn arraycopysetdestindex(expr Expression, destindexexpr Expression)

[c: 'BinaryenArrayCopyGetSrcRef']
pub fn arraycopygetsrcref(expr Expression) Expression

[c: 'BinaryenArrayCopySetSrcRef']
pub fn arraycopysetsrcref(expr Expression, srcrefexpr Expression)

[c: 'BinaryenArrayCopyGetSrcIndex']
pub fn arraycopygetsrcindex(expr Expression) Expression

[c: 'BinaryenArrayCopySetSrcIndex']
pub fn arraycopysetsrcindex(expr Expression, srcindexexpr Expression)

[c: 'BinaryenArrayCopyGetLength']
pub fn arraycopygetlength(expr Expression) Expression

[c: 'BinaryenArrayCopySetLength']
pub fn arraycopysetlength(expr Expression, lengthexpr Expression)

[c: 'BinaryenStringNewGetOp']
pub fn stringnewgetop(expr Expression) Op

[c: 'BinaryenStringNewSetOp']
pub fn stringnewsetop(expr Expression, op Op)

[c: 'BinaryenStringNewGetPtr']
pub fn stringnewgetptr(expr Expression) Expression

[c: 'BinaryenStringNewSetPtr']
pub fn stringnewsetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenStringNewGetLength']
pub fn stringnewgetlength(expr Expression) Expression

[c: 'BinaryenStringNewSetLength']
pub fn stringnewsetlength(expr Expression, lengthexpr Expression)

[c: 'BinaryenStringNewGetStart']
pub fn stringnewgetstart(expr Expression) Expression

[c: 'BinaryenStringNewSetStart']
pub fn stringnewsetstart(expr Expression, startexpr Expression)

[c: 'BinaryenStringNewGetEnd']
pub fn stringnewgetend(expr Expression) Expression

[c: 'BinaryenStringNewSetEnd']
pub fn stringnewsetend(expr Expression, endexpr Expression)

[c: 'BinaryenStringConstGetString']
pub fn stringconstgetstring(expr Expression) &i8

[c: 'BinaryenStringConstSetString']
pub fn stringconstsetstring(expr Expression, stringstr &i8)

[c: 'BinaryenStringMeasureGetOp']
pub fn stringmeasuregetop(expr Expression) Op

[c: 'BinaryenStringMeasureSetOp']
pub fn stringmeasuresetop(expr Expression, op Op)

[c: 'BinaryenStringMeasureGetRef']
pub fn stringmeasuregetref(expr Expression) Expression

[c: 'BinaryenStringMeasureSetRef']
pub fn stringmeasuresetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringEncodeGetOp']
pub fn stringencodegetop(expr Expression) Op

[c: 'BinaryenStringEncodeSetOp']
pub fn stringencodesetop(expr Expression, op Op)

[c: 'BinaryenStringEncodeGetRef']
pub fn stringencodegetref(expr Expression) Expression

[c: 'BinaryenStringEncodeSetRef']
pub fn stringencodesetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringEncodeGetPtr']
pub fn stringencodegetptr(expr Expression) Expression

[c: 'BinaryenStringEncodeSetPtr']
pub fn stringencodesetptr(expr Expression, ptrexpr Expression)

[c: 'BinaryenStringEncodeGetStart']
pub fn stringencodegetstart(expr Expression) Expression

[c: 'BinaryenStringEncodeSetStart']
pub fn stringencodesetstart(expr Expression, startexpr Expression)

[c: 'BinaryenStringConcatGetLeft']
pub fn stringconcatgetleft(expr Expression) Expression

[c: 'BinaryenStringConcatSetLeft']
pub fn stringconcatsetleft(expr Expression, leftexpr Expression)

[c: 'BinaryenStringConcatGetRight']
pub fn stringconcatgetright(expr Expression) Expression

[c: 'BinaryenStringConcatSetRight']
pub fn stringconcatsetright(expr Expression, rightexpr Expression)

[c: 'BinaryenStringEqGetLeft']
pub fn stringeqgetleft(expr Expression) Expression

[c: 'BinaryenStringEqSetLeft']
pub fn stringeqsetleft(expr Expression, leftexpr Expression)

[c: 'BinaryenStringEqGetRight']
pub fn stringeqgetright(expr Expression) Expression

[c: 'BinaryenStringEqSetRight']
pub fn stringeqsetright(expr Expression, rightexpr Expression)

[c: 'BinaryenStringAsGetOp']
pub fn stringasgetop(expr Expression) Op

[c: 'BinaryenStringAsSetOp']
pub fn stringassetop(expr Expression, op Op)

[c: 'BinaryenStringAsGetRef']
pub fn stringasgetref(expr Expression) Expression

[c: 'BinaryenStringAsSetRef']
pub fn stringassetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringWTF8AdvanceGetRef']
pub fn stringwtf8advancegetref(expr Expression) Expression

[c: 'BinaryenStringWTF8AdvanceSetRef']
pub fn stringwtf8advancesetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringWTF8AdvanceGetPos']
pub fn stringwtf8advancegetpos(expr Expression) Expression

[c: 'BinaryenStringWTF8AdvanceSetPos']
pub fn stringwtf8advancesetpos(expr Expression, posexpr Expression)

[c: 'BinaryenStringWTF8AdvanceGetBytes']
pub fn stringwtf8advancegetbytes(expr Expression) Expression

[c: 'BinaryenStringWTF8AdvanceSetBytes']
pub fn stringwtf8advancesetbytes(expr Expression, bytesexpr Expression)

[c: 'BinaryenStringWTF16GetGetRef']
pub fn stringwtf16getgetref(expr Expression) Expression

[c: 'BinaryenStringWTF16GetSetRef']
pub fn stringwtf16getsetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringWTF16GetGetPos']
pub fn stringwtf16getgetpos(expr Expression) Expression

[c: 'BinaryenStringWTF16GetSetPos']
pub fn stringwtf16getsetpos(expr Expression, posexpr Expression)

[c: 'BinaryenStringIterNextGetRef']
pub fn stringiternextgetref(expr Expression) Expression

[c: 'BinaryenStringIterNextSetRef']
pub fn stringiternextsetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringIterMoveGetOp']
pub fn stringitermovegetop(expr Expression) Op

[c: 'BinaryenStringIterMoveSetOp']
pub fn stringitermovesetop(expr Expression, op Op)

[c: 'BinaryenStringIterMoveGetRef']
pub fn stringitermovegetref(expr Expression) Expression

[c: 'BinaryenStringIterMoveSetRef']
pub fn stringitermovesetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringIterMoveGetNum']
pub fn stringitermovegetnum(expr Expression) Expression

[c: 'BinaryenStringIterMoveSetNum']
pub fn stringitermovesetnum(expr Expression, numexpr Expression)

[c: 'BinaryenStringSliceWTFGetOp']
pub fn stringslicewtfgetop(expr Expression) Op

[c: 'BinaryenStringSliceWTFSetOp']
pub fn stringslicewtfsetop(expr Expression, op Op)

[c: 'BinaryenStringSliceWTFGetRef']
pub fn stringslicewtfgetref(expr Expression) Expression

[c: 'BinaryenStringSliceWTFSetRef']
pub fn stringslicewtfsetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringSliceWTFGetStart']
pub fn stringslicewtfgetstart(expr Expression) Expression

[c: 'BinaryenStringSliceWTFSetStart']
pub fn stringslicewtfsetstart(expr Expression, startexpr Expression)

[c: 'BinaryenStringSliceWTFGetEnd']
pub fn stringslicewtfgetend(expr Expression) Expression

[c: 'BinaryenStringSliceWTFSetEnd']
pub fn stringslicewtfsetend(expr Expression, endexpr Expression)

[c: 'BinaryenStringSliceIterGetRef']
pub fn stringsliceitergetref(expr Expression) Expression

[c: 'BinaryenStringSliceIterSetRef']
pub fn stringsliceitersetref(expr Expression, refexpr Expression)

[c: 'BinaryenStringSliceIterGetNum']
pub fn stringsliceitergetnum(expr Expression) Expression

[c: 'BinaryenStringSliceIterSetNum']
pub fn stringsliceitersetnum(expr Expression, numexpr Expression)

type Function = voidptr

[c: 'BinaryenAddFunction']
pub fn addfunction(module_ Module, name &i8, params Type, results Type, vartypes &Type, numvartypes Index, body Expression) Function

[c: 'BinaryenGetFunction']
pub fn getfunction(module_ Module, name &i8) Function

[c: 'BinaryenRemoveFunction']
pub fn removefunction(module_ Module, name &i8)

[c: 'BinaryenGetNumFunctions']
pub fn getnumfunctions(module_ Module) Index

[c: 'BinaryenGetFunctionByIndex']
pub fn getfunctionbyindex(module_ Module, index Index) Function

[c: 'BinaryenAddFunctionImport']
pub fn addfunctionimport(module_ Module, internalname &i8, externalmodulename &i8, externalbasename &i8, params Type, results Type)

[c: 'BinaryenAddTableImport']
pub fn addtableimport(module_ Module, internalname &i8, externalmodulename &i8, externalbasename &i8)

[c: 'BinaryenAddMemoryImport']
pub fn addmemoryimport(module_ Module, internalname &i8, externalmodulename &i8, externalbasename &i8, shared_ u8)

[c: 'BinaryenAddGlobalImport']
pub fn addglobalimport(module_ Module, internalname &i8, externalmodulename &i8, externalbasename &i8, globaltype Type, mutable_ bool)

[c: 'BinaryenAddTagImport']
pub fn addtagimport(module_ Module, internalname &i8, externalmodulename &i8, externalbasename &i8, params Type, results Type)

type Export = voidptr

[c: 'BinaryenAddExport']
pub fn addexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenAddFunctionExport']
pub fn addfunctionexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenAddTableExport']
pub fn addtableexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenAddMemoryExport']
pub fn addmemoryexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenAddGlobalExport']
pub fn addglobalexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenAddTagExport']
pub fn addtagexport(module_ Module, internalname &i8, externalname &i8) Export

[c: 'BinaryenGetExport']
pub fn getexport(module_ Module, externalname &i8) Export

[c: 'BinaryenRemoveExport']
pub fn removeexport(module_ Module, externalname &i8)

[c: 'BinaryenGetNumExports']
pub fn getnumexports(module_ Module) Index

[c: 'BinaryenGetExportByIndex']
pub fn getexportbyindex(module_ Module, index Index) Export

type Global = voidptr

[c: 'BinaryenAddGlobal']
pub fn addglobal(module_ Module, name &i8, type_ Type, mutable_ bool, init Expression) Global

[c: 'BinaryenGetGlobal']
pub fn getglobal(module_ Module, name &i8) Global

[c: 'BinaryenRemoveGlobal']
pub fn removeglobal(module_ Module, name &i8)

[c: 'BinaryenGetNumGlobals']
pub fn getnumglobals(module_ Module) Index

[c: 'BinaryenGetGlobalByIndex']
pub fn getglobalbyindex(module_ Module, index Index) Global

type Tag = voidptr

[c: 'BinaryenAddTag']
pub fn addtag(module_ Module, name &i8, params Type, results Type) Tag

[c: 'BinaryenGetTag']
pub fn gettag(module_ Module, name &i8) Tag

[c: 'BinaryenRemoveTag']
pub fn removetag(module_ Module, name &i8)

type Table = voidptr

[c: 'BinaryenAddTable']
pub fn addtable(module_ Module, table &i8, initial Index, maximum Index, tabletype Type) Table

[c: 'BinaryenRemoveTable']
pub fn removetable(module_ Module, table &i8)

[c: 'BinaryenGetNumTables']
pub fn getnumtables(module_ Module) Index

[c: 'BinaryenGetTable']
pub fn gettable(module_ Module, name &i8) Table

[c: 'BinaryenGetTableByIndex']
pub fn gettablebyindex(module_ Module, index Index) Table

type ElementSegment = voidptr

[c: 'BinaryenAddActiveElementSegment']
pub fn addactiveelementsegment(module_ Module, table &i8, name &i8, funcnames &&u8, numfuncnames Index, offset Expression) ElementSegment

[c: 'BinaryenAddPassiveElementSegment']
pub fn addpassiveelementsegment(module_ Module, name &i8, funcnames &&u8, numfuncnames Index) ElementSegment

[c: 'BinaryenRemoveElementSegment']
pub fn removeelementsegment(module_ Module, name &i8)

[c: 'BinaryenGetNumElementSegments']
pub fn getnumelementsegments(module_ Module) Index

[c: 'BinaryenGetElementSegment']
pub fn getelementsegment(module_ Module, name &i8) ElementSegment

[c: 'BinaryenGetElementSegmentByIndex']
pub fn getelementsegmentbyindex(module_ Module, index Index) ElementSegment

[c: 'BinaryenSetMemory']
pub fn setmemory(module_ Module, initial Index, maximum Index, exportname &i8, segments &&u8, segmentpassive &bool, segmentoffsets &Expression, segmentsizes &Index, numsegments Index, shared_ bool, memory64 bool, name &i8)

[c: 'BinaryenHasMemory']
pub fn hasmemory(module_ Module) bool

[c: 'BinaryenMemoryGetInitial']
pub fn memorygetinitial(module_ Module, name &i8) Index

[c: 'BinaryenMemoryHasMax']
pub fn memoryhasmax(module_ Module, name &i8) bool

[c: 'BinaryenMemoryGetMax']
pub fn memorygetmax(module_ Module, name &i8) Index

[c: 'BinaryenMemoryImportGetModule']
pub fn memoryimportgetmodule(module_ Module, name &i8) &i8

[c: 'BinaryenMemoryImportGetBase']
pub fn memoryimportgetbase(module_ Module, name &i8) &i8

[c: 'BinaryenMemoryIsshared_']
pub fn memoryisshared_(module_ Module, name &i8) bool

[c: 'BinaryenMemoryIs64']
pub fn memoryis64(module_ Module, name &i8) bool

[c: 'BinaryenGetNumMemorySegments']
pub fn getnummemorysegments(module_ Module) u32

[c: 'BinaryenGetMemorySegmentByteOffset']
pub fn getmemorysegmentbyteoffset(module_ Module, id Index) u32

[c: 'BinaryenGetMemorySegmentByteLength']
pub fn getmemorysegmentbytelength(module_ Module, id Index) usize

[c: 'BinaryenGetMemorySegmentPassive']
pub fn getmemorysegmentpassive(module_ Module, id Index) bool

[c: 'BinaryenCopyMemorySegmentData']
pub fn copymemorysegmentdata(module_ Module, id Index, buffer &i8)

[c: 'BinaryenSetStart']
pub fn setstart(module_ Module, start Function)

[c: 'BinaryenModuleGetFeatures']
pub fn modulegetfeatures(module_ Module) Features

[c: 'BinaryenModuleSetFeatures']
pub fn modulesetfeatures(module_ Module, features Features)

[c: 'BinaryenModuleParse']
pub fn moduleparse(text &i8) Module

[c: 'BinaryenModulePrint']
pub fn moduleprint(module_ Module)

[c: 'BinaryenModulePrintStackIR']
pub fn moduleprintstackir(module_ Module, optimize bool)

[c: 'BinaryenModulePrintAsmjs']
pub fn moduleprintasmjs(module_ Module)

[c: 'BinaryenModuleValidate']
pub fn modulevalidate(module_ Module) bool

[c: 'BinaryenModuleOptimize']
pub fn moduleoptimize(module_ Module)

[c: 'BinaryenModuleUpdateMaps']
pub fn moduleupdatemaps(module_ Module)

[c: 'BinaryenGetOptimizeLevel']
pub fn getoptimizelevel() int

[c: 'BinaryenSetOptimizeLevel']
pub fn setoptimizelevel(level int)

[c: 'BinaryenGetShrinkLevel']
pub fn getshrinklevel() int

[c: 'BinaryenSetShrinkLevel']
pub fn setshrinklevel(level int)

[c: 'BinaryenGetDebugInfo']
pub fn getdebuginfo() bool

[c: 'BinaryenSetDebugInfo']
pub fn setdebuginfo(on bool)

[c: 'BinaryenGetLowMemoryUnused']
pub fn getlowmemoryunused() bool

[c: 'BinaryenSetLowMemoryUnused']
pub fn setlowmemoryunused(on bool)

[c: 'BinaryenGetZeroFilledMemory']
pub fn getzerofilledmemory() bool

[c: 'BinaryenSetZeroFilledMemory']
pub fn setzerofilledmemory(on bool)

[c: 'BinaryenGetFastMath']
pub fn getfastmath() bool

[c: 'BinaryenSetFastMath']
pub fn setfastmath(value bool)

[c: 'BinaryenGetPassArgument']
pub fn getpassargument(name &i8) &i8

[c: 'BinaryenSetPassArgument']
pub fn setpassargument(name &i8, value &i8)

[c: 'BinaryenClearPassArguments']
pub fn clearpassarguments()

[c: 'BinaryenGetAlwaysInlineMaxSize']
pub fn getalwaysinlinemaxsize() Index

[c: 'BinaryenSetAlwaysInlineMaxSize']
pub fn setalwaysinlinemaxsize(size Index)

[c: 'BinaryenGetFlexibleInlineMaxSize']
pub fn getflexibleinlinemaxsize() Index

[c: 'BinaryenSetFlexibleInlineMaxSize']
pub fn setflexibleinlinemaxsize(size Index)

[c: 'BinaryenGetOneCallerInlineMaxSize']
pub fn getonecallerinlinemaxsize() Index

[c: 'BinaryenSetOneCallerInlineMaxSize']
pub fn setonecallerinlinemaxsize(size Index)

[c: 'BinaryenGetAllowInliningFunctionsWithLoops']
pub fn getallowinliningfunctionswithloops() bool

[c: 'BinaryenSetAllowInliningFunctionsWithLoops']
pub fn setallowinliningfunctionswithloops(enabled bool)

[c: 'BinaryenModuleRunPasses']
pub fn modulerunpasses(module_ Module, passes &&u8, numpasses Index)

[c: 'BinaryenModuleAutoDrop']
pub fn moduleautodrop(module_ Module)

[c: 'BinaryenModuleWrite']
pub fn modulewrite(module_ Module, output &i8, outputsize usize) usize

[c: 'BinaryenModuleWriteText']
pub fn modulewritetext(module_ Module, output &i8, outputsize usize) usize

[c: 'BinaryenModuleWriteStackIR']
pub fn modulewritestackir(module_ Module, output &i8, outputsize usize, optimize bool) usize

pub struct BufferSizes {
pub:
	outputBytes    usize
	sourceMapBytes usize
}

[c: 'BinaryenModuleWriteWithSourceMap']
pub fn modulewritewithsourcemap(module_ Module, url &i8, output &i8, outputsize usize, sourcemap &i8, sourcemapsize usize) BufferSizes

pub struct ModuleAllocateAndWriteResult {
pub:
	binary      voidptr
	binaryBytes usize
	sourceMap   &i8
}

[c: 'BinaryenModuleAllocateAndWrite']
pub fn moduleallocateandwrite(module_ Module, sourcemapurl &i8) ModuleAllocateAndWriteResult

[c: 'BinaryenModuleAllocateAndWriteText']
pub fn moduleallocateandwritetext(module_ Module) &i8

[c: 'BinaryenModuleAllocateAndWriteStackIR']
pub fn moduleallocateandwritestackir(module_ Module, optimize bool) &i8

[c: 'BinaryenModuleRead']
pub fn moduleread(input &i8, inputsize usize) Module

[c: 'BinaryenModuleInterpret']
pub fn moduleinterpret(module_ Module)

[c: 'BinaryenModuleAddDebugInfoFileName']
pub fn moduleadddebuginfofilename(module_ Module, filename &i8) Index

[c: 'BinaryenModuleGetDebugInfoFileName']
pub fn modulegetdebuginfofilename(module_ Module, index Index) &i8

[c: 'BinaryenFunctionGetName']
pub fn functiongetname(func Function) &i8

[c: 'BinaryenFunctionGetParams']
pub fn functiongetparams(func Function) Type

[c: 'BinaryenFunctionGetResults']
pub fn functiongetresults(func Function) Type

[c: 'BinaryenFunctionGetNumVars']
pub fn functiongetnumvars(func Function) Index

[c: 'BinaryenFunctionGetVar']
pub fn functiongetvar(func Function, index Index) Type

[c: 'BinaryenFunctionGetNumLocals']
pub fn functiongetnumlocals(func Function) Index

[c: 'BinaryenFunctionHasLocalName']
pub fn functionhaslocalname(func Function, index Index) bool

[c: 'BinaryenFunctionGetLocalName']
pub fn functiongetlocalname(func Function, index Index) &i8

[c: 'BinaryenFunctionSetLocalName']
pub fn functionsetlocalname(func Function, index Index, name &i8)

[c: 'BinaryenFunctionGetBody']
pub fn functiongetbody(func Function) Expression

[c: 'BinaryenFunctionSetBody']
pub fn functionsetbody(func Function, body Expression)

[c: 'BinaryenFunctionOptimize']
pub fn functionoptimize(func Function, module_ Module)

[c: 'BinaryenFunctionRunPasses']
pub fn functionrunpasses(func Function, module_ Module, passes &&u8, numpasses Index)

[c: 'BinaryenFunctionSetDebugLocation']
pub fn functionsetdebuglocation(func Function, expr Expression, fileindex Index, linenumber Index, columnnumber Index)

[c: 'BinaryenTableGetName']
pub fn tablegetname(table Table) &i8

[c: 'BinaryenTableSetName']
pub fn tablesetname(table Table, name &i8)

[c: 'BinaryenTableGetInitial']
pub fn tablegetinitial(table Table) Index

[c: 'BinaryenTableSetInitial']
pub fn tablesetinitial(table Table, initial Index)

[c: 'BinaryenTableHasMax']
pub fn tablehasmax(table Table) bool

[c: 'BinaryenTableGetMax']
pub fn tablegetmax(table Table) Index

[c: 'BinaryenTableSetMax']
pub fn tablesetmax(table Table, max Index)

[c: 'BinaryenElementSegmentGetName']
pub fn elementsegmentgetname(elem ElementSegment) &i8

[c: 'BinaryenElementSegmentSetName']
pub fn elementsegmentsetname(elem ElementSegment, name &i8)

[c: 'BinaryenElementSegmentGetTable']
pub fn elementsegmentgettable(elem ElementSegment) &i8

[c: 'BinaryenElementSegmentSetTable']
pub fn elementsegmentsettable(elem ElementSegment, table &i8)

[c: 'BinaryenElementSegmentGetOffset']
pub fn elementsegmentgetoffset(elem ElementSegment) Expression

[c: 'BinaryenElementSegmentGetLength']
pub fn elementsegmentgetlength(elem ElementSegment) Index

[c: 'BinaryenElementSegmentGetData']
pub fn elementsegmentgetdata(elem ElementSegment, dataid Index) &i8

[c: 'BinaryenElementSegmentIsPassive']
pub fn elementsegmentispassive(elem ElementSegment) bool

[c: 'BinaryenGlobalGetName']
pub fn globalgetname(global Global) &i8

[c: 'BinaryenGlobalGetType']
pub fn globalgettype(global Global) Type

[c: 'BinaryenGlobalIsMutable']
pub fn globalismutable(global Global) bool

[c: 'BinaryenGlobalGetInitExpr']
pub fn globalgetinitexpr(global Global) Expression

[c: 'BinaryenTagGetName']
pub fn taggetname(tag Tag) &i8

[c: 'BinaryenTagGetParams']
pub fn taggetparams(tag Tag) Type

[c: 'BinaryenTagGetResults']
pub fn taggetresults(tag Tag) Type

[c: 'BinaryenFunctionImportGetModule']
pub fn functionimportgetmodule(import_ Function) &i8

[c: 'BinaryenTableImportGetModule']
pub fn tableimportgetmodule(import_ Table) &i8

[c: 'BinaryenGlobalImportGetModule']
pub fn globalimportgetmodule(import_ Global) &i8

[c: 'BinaryenTagImportGetModule']
pub fn tagimportgetmodule(import_ Tag) &i8

[c: 'BinaryenFunctionImportGetBase']
pub fn functionimportgetbase(import_ Function) &i8

[c: 'BinaryenTableImportGetBase']
pub fn tableimportgetbase(import_ Table) &i8

[c: 'BinaryenGlobalImportGetBase']
pub fn globalimportgetbase(import_ Global) &i8

[c: 'BinaryenTagImportGetBase']
pub fn tagimportgetbase(import_ Tag) &i8

[c: 'BinaryenExportGetKind']
pub fn exportgetkind(export_ Export) ExternalKind

[c: 'BinaryenExportGetName']
pub fn exportgetname(export_ Export) &i8

[c: 'BinaryenExportGetValue']
pub fn exportgetvalue(export_ Export) &i8

[c: 'BinaryenAddCustomSection']
pub fn addcustomsection(module_ Module, name &i8, contents &i8, contentssize Index)

type SideEffects = u32

[c: 'BinaryenSideEffectNone']
pub fn sideeffectnone() SideEffects

[c: 'BinaryenSideEffectBranches']
pub fn sideeffectbranches() SideEffects

[c: 'BinaryenSideEffectCalls']
pub fn sideeffectcalls() SideEffects

[c: 'BinaryenSideEffectReadsLocal']
pub fn sideeffectreadslocal() SideEffects

[c: 'BinaryenSideEffectWritesLocal']
pub fn sideeffectwriteslocal() SideEffects

[c: 'BinaryenSideEffectReadsGlobal']
pub fn sideeffectreadsglobal() SideEffects

[c: 'BinaryenSideEffectWritesGlobal']
pub fn sideeffectwritesglobal() SideEffects

[c: 'BinaryenSideEffectReadsMemory']
pub fn sideeffectreadsmemory() SideEffects

[c: 'BinaryenSideEffectWritesMemory']
pub fn sideeffectwritesmemory() SideEffects

[c: 'BinaryenSideEffectReadsTable']
pub fn sideeffectreadstable() SideEffects

[c: 'BinaryenSideEffectWritesTable']
pub fn sideeffectwritestable() SideEffects

[c: 'BinaryenSideEffectImplicitTrap']
pub fn sideeffectimplicittrap() SideEffects

[c: 'BinaryenSideEffectTrapsNeverHappen']
pub fn sideeffecttrapsneverhappen() SideEffects

[c: 'BinaryenSideEffectIsAtomic']
pub fn sideeffectisatomic() SideEffects

[c: 'BinaryenSideEffectThrows']
pub fn sideeffectthrows() SideEffects

[c: 'BinaryenSideEffectDanglingPop']
pub fn sideeffectdanglingpop() SideEffects

[c: 'BinaryenSideEffectAny']
pub fn sideeffectany() SideEffects

[c: 'BinaryenExpressionGetSideEffects']
pub fn expressiongetsideeffects(expr Expression, module_ Module) SideEffects

type Relooper = voidptr
type RelooperBlock = voidptr

[c: 'RelooperCreate']
pub fn reloopercreate(module_ Module) Relooper

[c: 'RelooperAddBlock']
pub fn relooperaddblock(relooper Relooper, code Expression) RelooperBlock

[c: 'RelooperAddBranch']
pub fn relooperaddbranch(from RelooperBlock, to RelooperBlock, condition Expression, code Expression)

[c: 'RelooperAddBlockWithSwitch']
pub fn relooperaddblockwithswitch(relooper Relooper, code Expression, condition Expression) RelooperBlock

[c: 'RelooperAddBranchForSwitch']
pub fn relooperaddbranchforswitch(from RelooperBlock, to RelooperBlock, indexes &Index, numindexes Index, code Expression)

[c: 'RelooperRenderAndDispose']
pub fn relooperrenderanddispose(relooper Relooper, entry RelooperBlock, labelhelper Index) Expression

type ExpressionRunner = voidptr
type ExpressionRunnerFlags = u32

[c: 'ExpressionRunnerFlagsDefault']
pub fn expressionrunnerflagsdefault() ExpressionRunnerFlags

[c: 'ExpressionRunnerFlagsPreserveSideeffects']
pub fn expressionrunnerflagspreservesideeffects() ExpressionRunnerFlags

[c: 'ExpressionRunnerFlagsTraverseCalls']
pub fn expressionrunnerflagstraversecalls() ExpressionRunnerFlags

[c: 'ExpressionRunnerCreate']
pub fn expressionrunnercreate(module_ Module, flags ExpressionRunnerFlags, maxdepth Index, maxloopiterations Index) ExpressionRunner

[c: 'ExpressionRunnerSetLocalValue']
pub fn expressionrunnersetlocalvalue(runner ExpressionRunner, index Index, value Expression) bool

[c: 'ExpressionRunnerSetGlobalValue']
pub fn expressionrunnersetglobalvalue(runner ExpressionRunner, name &i8, value Expression) bool

[c: 'ExpressionRunnerRunAndDispose']
pub fn expressionrunnerrunanddispose(runner ExpressionRunner, expr Expression) Expression

type TypeBuilder = voidptr
type TypeBuilderErrorReason = u32

[c: 'TypeBuilderErrorReasonSelfSupertype']
pub fn typebuildererrorreasonselfsupertype() TypeBuilderErrorReason

[c: 'TypeBuilderErrorReasonInvalidSupertype']
pub fn typebuildererrorreasoninvalidsupertype() TypeBuilderErrorReason

[c: 'TypeBuilderErrorReasonForwardSupertypeReference']
pub fn typebuildererrorreasonforwardsupertypereference() TypeBuilderErrorReason

[c: 'TypeBuilderErrorReasonForwardChildReference']
pub fn typebuildererrorreasonforwardchildreference() TypeBuilderErrorReason

type BasicHeapType = u32

[c: 'TypeBuilderCreate']
pub fn typebuildercreate(size Index) TypeBuilder

[c: 'TypeBuilderGrow']
pub fn typebuildergrow(builder TypeBuilder, count Index)

[c: 'TypeBuilderGetSize']
pub fn typebuildergetsize(builder TypeBuilder) Index

[c: 'TypeBuilderSetBasicHeapType']
pub fn typebuildersetbasicheaptype(builder TypeBuilder, index Index, basicheaptype BasicHeapType)

[c: 'TypeBuilderSetSignatureType']
pub fn typebuildersetsignaturetype(builder TypeBuilder, index Index, paramtypes Type, resulttypes Type)

[c: 'TypeBuilderSetStructType']
pub fn typebuildersetstructtype(builder TypeBuilder, index Index, fieldtypes &Type, fieldpackedtypes &Type, fieldmutables &bool, numfields int)

[c: 'TypeBuilderSetArrayType']
pub fn typebuildersetarraytype(builder TypeBuilder, index Index, elementtype Type, elementpackedtype PackedType, elementmutable int)

[c: 'TypeBuilderIsBasic']
pub fn typebuilderisbasic(builder TypeBuilder, index Index) bool

[c: 'TypeBuilderGetBasic']
pub fn typebuildergetbasic(builder TypeBuilder, index Index) BasicHeapType

[c: 'TypeBuilderGetTempHeapType']
pub fn typebuildergettempheaptype(builder TypeBuilder, index Index) HeapType

[c: 'TypeBuilderGetTempTupleType']
pub fn typebuildergettemptupletype(builder TypeBuilder, types &Type, numtypes Index) Type

[c: 'TypeBuilderGetTempRefType']
pub fn typebuildergettempreftype(builder TypeBuilder, heaptype HeapType, nullable int) Type

[c: 'TypeBuilderSetSubType']
pub fn typebuildersetsubtype(builder TypeBuilder, index Index, supertype HeapType)

[c: 'TypeBuilderCreateRecGroup']
pub fn typebuildercreaterecgroup(builder TypeBuilder, index Index, length Index)

[c: 'TypeBuilderBuildAndDispose']
pub fn typebuilderbuildanddispose(builder TypeBuilder, heaptypes &HeapType, errorindex &Index, errorreason &TypeBuilderErrorReason) bool

[c: 'ModuleSetTypeName']
pub fn modulesettypename(module_ Module, heaptype HeapType, name &i8)

[c: 'ModuleSetFieldName']
pub fn modulesetfieldname(module_ Module, heaptype HeapType, index Index, name &i8)

[c: 'BinaryenSetColorsEnabled']
pub fn setcolorsenabled(enabled bool)

[c: 'BinaryenAreColorsEnabled']
pub fn arecolorsenabled() bool
