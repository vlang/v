module main

pub enum AssetGetStatus {
	ok
	error
}

struct Sound {}

struct Asset {}

pub fn get[T]() (T, AssetGetStatus) {
	$if T is Sound {
		return Sound{}, AssetGetStatus.ok
	} $else $if T is &Asset {
		return &Asset{}, AssetGetStatus.ok
	} $else {
		$compile_error('get[T]: only retreival of Sound and &Asset is currently supported')
	}
	// Should not could be reached:
	return T{}, AssetGetStatus.error
}

fn test_main() {
	asset, status := get[&Asset]()
	assert status == AssetGetStatus.ok
}
