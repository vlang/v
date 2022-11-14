// see https://discordapp.com/channels/592103645835821068/592114487759470596/762271917293043762
const (
	steamid_id_shift = 2
	steamid_id_mask  = 0x04
)

type SteamId = u64

fn (mut s SteamId) set_id(i u32) {
	(*s) &= ~steamid_id_mask
	(*s) |= ((u64(i) << steamid_id_shift) & steamid_id_mask)
}

fn test_bitops_work_with_type_aliases() {
	mut x := SteamId(123)
	x.set_id(5)
	assert 'x: ${x}' == 'x: 127'
}
