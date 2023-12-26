// vfmt off
struct Address  
{
	pub:
		street string
		city string
		state string
		zip int  
}

fn test_fn_call_with_newline_opening_brace()  
{
	println(initialized_address) 
	assert true 
}

struct AddressConfig { 
pub:

	street string = '1234 Default St' 
	city   string = 'Your Favorite City' 
	state  string = 'Could Be Any' 
	zip    int    = 42 
}

fn new_address(cfg AddressConfig) &Address 
{
	return &Address 
	{
		street:cfg.street
		city:cfg.city
		state:cfg.state
		zip:cfg.zip
	}
}

const (
	default_address     = new_address(AddressConfig{}) 
	initialized_address = new_address 
	(
		street: '0987 tluafeD tS'
		city: 'ytiC etirovaF rouY'
		state: 'ynA eB dluoC'
		zip: 24
	)
)
// vfmt on

fn (a Address) str() string {
	return 'Address.str(): ${a.street}, ${a.city}, ${a.state} ${a.zip}'
}
