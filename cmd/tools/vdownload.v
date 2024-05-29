import os
import log
import time
import flag
import net.http
import crypto.sha1
import crypto.sha256

struct Context {
mut:
	show_help           bool
	target_folder       string
	continue_on_failure bool
	retries             int
	delay               time.Duration
	urls                []string
}

fn main() {
	mut ctx := Context{}
	mut fp := flag.new_flag_parser(os.args#[1..])
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.1')
	fp.description('Download files from http/https servers, given their URLs.')
	fp.arguments_description('URL1 URL2 ...')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)!
	ctx.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	ctx.target_folder = fp.string('target-folder', `t`, '.', 'The target folder, where the file will be stored. It will be created, if it does not exist. Default is current folder.')
	ctx.continue_on_failure = fp.bool('continue', `c`, false, 'Continue on download failures. If you download 5 URLs, and several of them fail, continue without error. False by default.')
	ctx.retries = fp.int('retries', `r`, 10, 'Number of retries, when an URL fails to download.')
	ctx.delay = time.Duration(u64(fp.float('delay', `d`, 1.0, 'Delay in seconds, after each retry.') * time.second))
	if ctx.show_help {
		println(fp.usage())
		exit(0)
	}
	ctx.urls = fp.finalize() or {
		eprintln('Error: ${err}')
		exit(1)
	}
	if ctx.target_folder != '.' {
		ctx.target_folder = ctx.target_folder.replace('\\', '/')
		os.mkdir_all(ctx.target_folder) or {
			eprintln('Can not create target folder `${ctx.target_folder}` . Error: ${err}.')
			exit(1)
		}
		os.chdir(ctx.target_folder)!
	}
	sw := time.new_stopwatch()
	mut errors := 0
	mut downloaded := 0
	for idx, url in ctx.urls {
		fname := url.all_after_last('/')
		fpath := '${ctx.target_folder}/${fname}'
		mut file_errors := 0
		log.info('Downloading [${idx + 1}/${ctx.urls.len}] from url: ${url} to ${fpath} ...')
		for retry in 0 .. ctx.retries {
			http.download_file(url, fname) or {
				log.error('    retry ${retry + 1}/${ctx.retries}, failed downloading from url: ${url}. Error: ${err}.')
				file_errors++
				time.sleep(ctx.delay)
				continue
			}
			downloaded++
			break
		}
		if file_errors == ctx.retries {
			log.error('Failed to download from url: ${url}.')
			errors++
			if ctx.continue_on_failure {
				continue
			} else {
				break
			}
		}
		fstat := os.stat(fname)!
		log.info(' Finished downloading file: ${fpath} .')
		log.info('                      size: ${fstat.size} bytes')

		fbytes := os.read_bytes(fname)!
		mut digest256 := sha256.new()
		digest256.write(fbytes)!
		mut sum256 := digest256.sum([])
		hash256 := sum256.hex()

		mut digest1 := sha1.new()
		digest1.write(fbytes)!
		mut sum1 := digest1.sum([])
		hash1 := sum1.hex()
		log.info('                      SHA1: ${hash1}')
		log.info('                    SHA256: ${hash256}')
	}
	println('Downloaded: ${downloaded} file(s) . Elapsed time: ${sw.elapsed()} . Errors: ${errors} .')
	if !ctx.continue_on_failure && errors > 0 {
		exit(1)
	}
}
