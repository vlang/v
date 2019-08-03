module vweb.assets

import (
	os
	times
	strings
	crypto.md5
)

const (
	UnknownAssetTypeError = 'vewb: unknown asset type'
)

struct AssetManager {
mut:
	minify    bool
	// uglify    bool
	cache_dir string
	css       []Asset
	js        []Asset
}

struct Asset {
	file_path     string
	last_modified time.Time
}

pub fn new_manager() *AssetManager {
	return &AssetManager{}
}

// if to_file is true combine will return the path to the combined file
// if it is false combine will return the combined file contents
pub fn (am mut AssetManager) combine(asset_type string, to_file bool) string {
	mut sb := string.new_builder(1000) 
	assets := am.get_assets(asset_type)
	mut files_salt := ''
	for asset in assets {
		mut data := os.read_file(asset.file_path)
		if am.minify {
			data = if asset_type == 'css' {
				minify_css(data)
			} else {
				minify_js(data)
			}
		}
		sb.write(data)
		files_salt += asset.file_path
	}
	if !to_file {
		return sb.str()
	}

	data := am.combine(as)
	now := time.now().uni
	hash := md5.sum(files_salt)
	out_file := '$am.cache_dir/$hash-{$now}.$asset_type'
	file := os.create(am.cache_dir) or {
		panic(err)
	}
	return out_file
}

// include returns the html link/script
fn (am mut AssetManager) include(asset_type string, combine bool) string {
	assets := get_assets(asset_type)
	mut sb := strings.new_builder(1000)
	if asset_type == 'css' {
		for asset in assets {
			sb.writeln('<link rel="stylesheet" href="$asset.file_path">')
		}
	}
	if asset_type == 'js' {
		for asset in assets {
			sb.writeln('<script type="text/javascript" src="$asset.file_path"></script>')
		}
	}
	return sb.str()
}


pub fn (am mut AssetManager) add_js(file string) ?bool {
	return am.add('js', file)
}


pub fn (am mut AssetManager) add_css(file string) ?bool {
	return am.add('css', file)
}

pub fn (am mut AssetManager) add(asset_type, file string) ?bool {
	if !os.stat(file) {
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
	assets := get_assets(asset_type)
	for asset in assets {
		if asset.file_path == file {
			return true
		}
	}
	return false
}

fn get_assets(asset_type string) []string {
	assets := if asset_type == 'css' {
		am.css
	} else if asset_type == 'js' {
		am.js
	} else {
		panic('$UnknownAssetTypeError $asset_type')
	}
	return assets
}

pub fn minify_js() {

}

pub fn minify_css() {

}