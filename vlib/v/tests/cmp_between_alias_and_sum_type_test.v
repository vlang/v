struct Empty {}

type IndexNumber = u32
type NameIndexType = Empty | IndexNumber

fn test_casting_with_sumtype_and_alias() {
	elem := NameIndexType(Empty{})
	if elem is IndexNumber {
	}
}
