module assets

import crypto.md5
import os
import strings
import time
import veb

pub enum AssetType {
	css
	js
	all
}

pub struct Asset {
pub:
	kind          AssetType
	file_path     string
	last_modified time.Time
	include_name  string
}

pub struct AssetManager {
mut:
	css               []Asset
	js                []Asset
	cached_file_names []string
pub mut:
	// when true assets will be minified
	minify bool
	// the directory to store the cached/combined files
	cache_dir string
	// how a combined file should be named. For example for css the extension '.css'
	// will be added to the end of `combined_file_name`
	combined_file_name string = 'combined'
}

fn (mut am AssetManager) add_asset_directory(directory_path string, traversed_path string) ! {
	files := os.ls(directory_path)!
	if files.len > 0 {
		for file in files {
			full_path := os.join_path(directory_path, file)
			relative_path := os.join_path(traversed_path, file)

			if os.is_dir(full_path) {
				am.add_asset_directory(full_path, relative_path)!
			} else {
				ext := os.file_ext(full_path)
				match ext {
					'.css' { am.add(.css, full_path, relative_path)! }
					'.js' { am.add(.js, full_path, relative_path)! }
					// ignore non css/js files
					else {}
				}
			}
		}
	}
}

// handle_assets recursively walks `directory_path` and adds any assets to the asset manager
pub fn (mut am AssetManager) handle_assets(directory_path string) ! {
	return am.add_asset_directory(directory_path, '')
}

// handle_assets_at recursively walks `directory_path` and adds any assets to the asset manager.
// The include name of assets are prefixed with `prepend`
pub fn (mut am AssetManager) handle_assets_at(directory_path string, prepend string) ! {
	// remove trailing '/'
	return am.add_asset_directory(directory_path, prepend.trim_right('/'))
}

// get all assets of type `asset_type`
pub fn (am AssetManager) get_assets(asset_type AssetType) []Asset {
	return match asset_type {
		.css {
			am.css
		}
		.js {
			am.js
		}
		.all {
			mut assets := []Asset{}
			assets << am.css
			assets << am.js
			assets
		}
	}
}

// add an asset to the asset manager
pub fn (mut am AssetManager) add(asset_type AssetType, file_path string, include_name string) ! {
	if asset_type == .all {
		return error('cannot minify asset of type "all"')
	}
	if !os.exists(file_path) {
		return error('cannot add asset: file "${file_path}" does not exist')
	}

	last_modified_unix := os.file_last_mod_unix(file_path)

	mut real_path := file_path

	if am.minify {
		// minify and cache file if it was modified
		output_path, is_cached := am.minify_and_cache(asset_type, real_path, last_modified_unix,
			include_name)!

		if is_cached == false && am.exists(asset_type, include_name) {
			// file was not modified between the last call to `add`
			// and the file was already in the asset manager, so we don't need to
			// add it again
			return
		}

		real_path = output_path
	}

	asset := Asset{
		kind:          asset_type
		file_path:     real_path
		last_modified: time.unix(last_modified_unix)
		include_name:  include_name
	}

	match asset_type {
		.css { am.css << asset }
		.js { am.js << asset }
		else {}
	}
}

fn (mut am AssetManager) minify_and_cache(asset_type AssetType, file_path string, last_modified i64, include_name string) !(string, bool) {
	if asset_type == .all {
		return error('cannot minify asset of type "all"')
	}

	if am.cache_dir == '' {
		return error('cannot minify asset: cache directory is not valid')
	} else if !os.exists(am.cache_dir) {
		os.mkdir_all(am.cache_dir)!
	}

	cache_key := am.get_cache_key(file_path, last_modified)
	output_file := '${cache_key}.${asset_type}'
	output_path := os.join_path(am.cache_dir, output_file)

	if os.exists(output_path) {
		// the output path already exists, this means that the file has
		// been minifed and cached before and hasn't changed in the meantime
		am.cached_file_names << output_file
		return output_path, false
	} else {
		// check if the file has been minified before, but is modified.
		// if that's the case we remove the old cached file
		cached_files := os.ls(am.cache_dir)!
		hash := cache_key.all_before('-')
		for file in cached_files {
			if file.starts_with(hash) {
				os.rm(os.join_path(am.cache_dir, file))!
			}
		}
	}

	txt := os.read_file(file_path)!
	minified := match asset_type {
		.css { minify_css(txt) }
		.js { minify_js(txt) }
		else { '' }
	}
	os.write_file(output_path, minified)!

	am.cached_file_names << output_file
	return output_path, true
}

fn (mut am AssetManager) get_cache_key(file_path string, last_modified i64) string {
	abs_path := if os.is_abs_path(file_path) { file_path } else { os.resource_abs_path(file_path) }
	hash := md5.sum(abs_path.bytes())
	return '${hash.hex()}-${last_modified}'
}

// cleanup_cache removes all files in the cache directory that aren't cached at the time
// this function is called
pub fn (mut am AssetManager) cleanup_cache() ! {
	if am.cache_dir == '' {
		return error('[veb.assets]: cache directory is not valid')
	}
	cached_files := os.ls(am.cache_dir)!

	// loop over all the files in the cache directory. If a file isn't cached, remove it
	for file in cached_files {
		ext := os.file_ext(file)
		if ext !in ['.css', '.js'] || file in am.cached_file_names {
			continue
		} else if !file.starts_with(am.combined_file_name) {
			os.rm(os.join_path(am.cache_dir, file))!
		}
	}
}

// check if an asset is already added to the asset manager
pub fn (am AssetManager) exists(asset_type AssetType, include_name string) bool {
	assets := am.get_assets(asset_type)

	return assets.any(it.include_name == include_name)
}

// include css/js files in your veb app from templates
// Example:
// ```html
// @{app.am.include(.css, 'main.css')}
// ```
pub fn (am AssetManager) include(asset_type AssetType, include_name string) veb.RawHtml {
	assets := am.get_assets(asset_type)
	for asset in assets {
		if asset.include_name == include_name {
			// always add link/src from root of web server ('/css/main.css'),
			// but leave absolute paths intact
			mut real_path := asset.file_path
			if real_path[0] != `/` && !os.is_abs_path(real_path) {
				real_path = '/${asset.file_path}'
			}

			return match asset_type {
				.css {
					'<link rel="stylesheet" href="${real_path}">'
				}
				.js {
					'<script src="${real_path}"></script>'
				}
				else {
					eprintln('[veb.assets] can only include css or js assets')
					''
				}
			}
		}
	}
	eprintln('[veb.assets] no asset with include name "${include_name}" exists!')
	return ''
}

// combine assets of type `asset_type` into a single file and return the outputted file path.
// If you call `combine` with asset type `all` the function will return an empty string,
// the minified files will be available at `combined_file_name`.`asset_type`
pub fn (mut am AssetManager) combine(asset_type AssetType) !string {
	if asset_type == .all {
		am.combine(.css)!
		am.combine(.js)!
		return ''
	}
	if am.cache_dir == '' {
		return error('cannot combine assets: cache directory is not valid')
	} else if !os.exists(am.cache_dir) {
		os.mkdir_all(am.cache_dir)!
	}

	assets := am.get_assets(asset_type)
	combined_file_path := os.join_path(am.cache_dir, '${am.combined_file_name}.${asset_type}')
	mut f := os.create(combined_file_path)!

	for asset in assets {
		bytes := os.read_bytes(asset.file_path)!
		f.write(bytes)!
		f.write_string('\n')!
	}

	f.close()

	return combined_file_path
}

// TODO: implement proper minification
@[manualfree]
pub fn minify_css(css string) string {
	mut lines := css.split('\n')
	// estimate arbitrary number of characters for a line of css
	mut sb := strings.new_builder(lines.len * 20)
	defer {
		unsafe { sb.free() }
	}

	for line in lines {
		trimmed := line.trim_space()
		if trimmed != '' {
			sb.write_string(trimmed)
		}
	}

	return sb.str()
}

// TODO: implement proper minification
@[manualfree]
pub fn minify_js(js string) string {
	mut lines := js.split('\n')
	// estimate arbitrary number of characters for a line of js
	mut sb := strings.new_builder(lines.len * 40)
	defer {
		unsafe { sb.free() }
	}

	for line in lines {
		trimmed := line.trim_space()
		if trimmed != '' {
			sb.write_string(trimmed)
			sb.write_u8(` `)
		}
	}

	return sb.str()
}
