module rand

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
}

pub fn string(len int) string {
	result := ''
	#
	#const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
	#const charactersLength = characters.length;
	#for (let i = 0;i < len.val;i++)
	#result.str += characters.charAt(Math.random() * charactersLength);

	return result
}
