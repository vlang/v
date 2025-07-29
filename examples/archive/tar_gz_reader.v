import archive.tar
import flag
import net.http
import os
import term

const default_url = 'https://github.com/vlang/v/archive/refs/tags/v0.1.3.tar.gz'

@[heap]
struct Context {
	url        string // Web starting with http:// or https://. Local starting with file:///
	chunks     bool   // true: decompress with callback
	debug      int    // print debug lines
	max_blocks int    // if max_blocks > 0 and is reached stops early.
	filename   string // if filename is found as a path of a data block, stops early.
}

fn (ctx &Context) read_last_block(mut read tar.Read) bool {
	if ctx.max_blocks > 0 && ctx.max_blocks < read.get_block_number() {
		read.stop_early = true
		return true
	}
	return false
}

fn new_context() !&Context {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('tar_gz_reader')
	fp.version('0.0.20250721')
	fp.description('Reads into memory selected sections of *.tar.gz. archives from https or home_dir.')
	fp.skip_executable()
	ctx := &Context{
		url:        fp.string('url', `u`, default_url, 'archive *.tar.gz URL, default(${default_url}). Start name with file:/// for local')
		chunks:     fp.bool('chunks', `c`, false, 'decompress with chunks to reduce RAM usage, default(false)')
		debug:      fp.int('debug', `d`, 0, 'prints blocks: 1=other, 2:+dirs, 3=+files, 4=+data, default(0=silent)')
		max_blocks: fp.int('max_blocks', `m`, 0, 'maximum blocks to read, stop early. Default(0=read all)')
		filename:   fp.string('filename', `f`, '', 'filename content complete print, stop early. Default(empty means none)')
	}
	additional := fp.finalize()!
	if additional.len > 0 {
		println('unprocessed args ${additional.join_lines()}')
	}
	return ctx
}

// Downloader downloads a *.tar.gz using HTTP chunks
struct Downloader {
mut:
	chunks int
	data   []u8
}

fn new_downloader(url string) !&Downloader {
	mut downloader := &Downloader{}
	params := http.DownloaderParams{
		downloader: downloader
	}
	if url.starts_with('http://') || url.starts_with('https://') {
		http.download_file_with_progress(url, '', params)!
	} else if url.starts_with('file:///') {
		path := '${os.home_dir()}/${url[8..]}'
		println('path ${path}')
		downloader.data = os.read_bytes(path)!
	}
	return downloader
}

fn (mut d Downloader) on_start(mut request http.Request, path string) ! {}

fn (mut d Downloader) on_chunk(request &http.Request, chunk []u8, already_received u64, expected u64) ! {
	if expected == 0 {
		return
	}
	d.chunks++
	d.data << chunk
}

fn (mut d Downloader) on_finish(request &http.Request, response &http.Response) ! {}

struct FileReader implements tar.Reader {
	ctx &Context
mut:
	filepath string
	content  []u8
}

fn new_file_reader(ctx &Context) FileReader {
	return FileReader{
		ctx: ctx
	}
}

fn (mut f FileReader) other_block(mut read tar.Read, details string) {
	if f.ctx.read_last_block(mut read) {
		return
	}
	if f.ctx.debug > 0 {
		row := 'OTHER  block:${read.get_block_number():6} ${read.get_special()} ${details} ${read.get_path()} '
		println(term.colorize(term.bright_yellow, row))
	}
}

fn (mut f FileReader) dir_block(mut read tar.Read, size u64) {
	if f.ctx.read_last_block(mut read) {
		return
	}
	if f.ctx.debug > 1 {
		row := 'DIR    block:${read.get_block_number():6} ${read.get_path()} size:${size}'
		println(term.colorize(term.green, row))
	}
}

fn (mut f FileReader) file_block(mut read tar.Read, size u64) {
	if f.ctx.read_last_block(mut read) {
		return
	}
	path := read.get_path()
	if f.ctx.debug > 2 {
		row := ' FILE  block:${read.get_block_number():6} ${path} size:${size}'
		println(term.colorize(term.bright_blue, row))
	}
	if f.ctx.filename != '' && f.filepath == '' && path.ends_with(f.ctx.filename) {
		f.filepath = path
	}
}

fn (mut f FileReader) data_block(mut read tar.Read, data []u8, pending int) {
	if f.ctx.read_last_block(mut read) {
		return
	}
	path := read.get_path()
	if f.ctx.debug > 3 {
		println('  DATA block:${read.get_block_number():6} ${path} len:${data.len} pend:${pending}')
	}
	if f.ctx.filename != '' {
		if f.filepath == path {
			f.content << data
			if pending == 0 {
				// our file of interest data is complete
				read.stop_early = true
			}
		}
	}
}

fn main() {
	ctx := new_context()!
	reader := FileReader{
		ctx: ctx
	}
	mut untar := tar.new_untar(reader)
	mut decompressor := tar.new_decompresor(untar)
	downloader := new_downloader(ctx.url)!
	if ctx.chunks {
		decompressor.read_chunks(downloader.data)!
	} else {
		decompressor.read_all(downloader.data)!
	}
	println('-'.repeat(80))
	println('Download: ${ctx.url} chunks:${downloader.chunks} bytes=${downloader.data.len}')
	println('Untar:    ${untar}')
	println('Content:  Path:${reader.filepath} bytes:${reader.content.len}')
	println('-'.repeat(80))
	println('${reader.content.bytestr()}')
	println('-'.repeat(80))
}
