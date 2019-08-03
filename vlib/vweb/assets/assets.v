module assets

import (
	os
	time
	strings
	crypto.md5
)

const (
	UnknownAssetTypeError = 'vewb: unknown asset type'
)

struct AssetManager {
mut:
	css       []Asset
	js        []Asset
pub:
	minify    bool
	// uglify    bool
	cache_dir string
}

struct Asset {
	file_path     string
	last_modified time.Time
}

pub fn new_manager() *AssetManager {
	return &AssetManager{}
}

pub fn (am mut AssetManager) combine_css(to_file bool) string {
	return am.combine('css', to_file)
}

pub fn (am mut AssetManager) combine_js(to_file bool) string {
	return am.combine('js', to_file)
}

// if to_file is true combine will return the path to the combined file
// if it is false combine will return the combined file contents
pub fn (am mut AssetManager) combine(asset_type string, to_file bool) string {
	mut out := ''
	assets := am.get_assets(asset_type)
	mut files_salt := ''
	for asset in assets {
		mut data := os.read_file(asset.file_path) or {
			panic(err)
			return ''
		}
		if am.minify {
			if asset_type == 'css' {
				// data = minify_css(data)
			} else {
				// data = minify_js(data)
			}
		}
		out += data
		files_salt += asset.file_path
	}
	if !to_file {
		return out
	}
	// output to file
	if am.cache_dir == '' {
		panic('vweb.asses: cache_dir is not set.')
	}
	if !os.dir_exists(am.cache_dir) {
		os.mkdir(am.cache_dir)
	}
	now := time.now().uni
	hash := md5.sum(files_salt.bytes()).hex()
	out_file := '$am.cache_dir/$hash-${now}.$asset_type'
	file := os.create(out_file) or {
		panic(err)
	}
	file.write(out)
	file.close()
	return out_file
}

pub fn (am mut AssetManager) include_css(combine bool) string {
	return am.include('css', combine)
}

pub fn (am mut AssetManager) include_js(combine bool) string {
	return am.include('js', combine)
}

// include returns the html link/script
pub fn (am mut AssetManager) include(asset_type string, combine bool) string {
	assets := am.get_assets(asset_type)
	mut out := ''
	if asset_type == 'css' {
		for asset in assets {
			println('here')
			out += '<link rel="stylesheet" href="$asset.file_path">\n'
		}
	}
	if asset_type == 'js' {
		for asset in assets {
			out += '<script type="text/javascript" src="$asset.file_path"></script>\n'
		}
	}
	return out
}


pub fn (am mut AssetManager) add_js(file string) ?bool {
	println('adding: $file')
	return am.add('js', file)
}


pub fn (am mut AssetManager) add_css(file string) ?bool {
	println('adding: $file')
	return am.add('css', file)
}

pub fn (am mut AssetManager) add(asset_type, file string) ?bool {
	if !os.file_exists(file) {
		return error('vweb: cannot add asset $file, it does not exist.')
	}
	asset := Asset{
		file_path: file
		last_modified: time.unix(os.file_last_mod_unix(file))
	}
	if asset_type == 'css' {
		am.css << asset
	} else if asset_type == 'js' {
		am.js << asset
	} else {
		panic('$UnknownAssetTypeError $asset_type')
	}
}

fn (am mut AssetManager) exists(asset_type, file string) bool {
	assets := am.get_assets(asset_type)
	for asset in assets {
		if asset.file_path == file {
			return true
		}
	}
	return false
}

fn (am mut AssetManager) get_assets(asset_type string) []Asset {
	if asset_type != 'css' || asset_type != 'css' {
		panic('$UnknownAssetTypeError $asset_type')
	}
	assets := if asset_type == 'css' {
		am.css
	} else {
		am.js
	}
	return assets
}

pub fn minify_js() {

}

pub fn minify_css() {

}