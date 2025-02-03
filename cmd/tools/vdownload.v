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
	show_sha1           bool
	show_sha256         bool
	target_folder       string
	continue_on_failure bool
	retries             int
	delay               time.Duration
	urls                []string
	should_run          bool
	delete_after_run    bool
}

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

fn main() {
	log.use_stdout()
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
	ctx.show_sha1 = fp.bool('sha1', `1`, false, 'Show the SHA1 hash of the downloaded file.')
	ctx.show_sha256 = fp.bool('sha256', `2`, false, 'Show the SHA256 hash of the downloaded file.')
	ctx.continue_on_failure = fp.bool('continue', `c`, false, 'Continue on download failures. If you download 5 URLs, and several of them fail, continue without error. False by default.')
	ctx.retries = fp.int('retries', `r`, 10, 'Number of retries, when an URL fails to download.')
	ctx.delay = time.Duration(u64(fp.float('delay', `d`, 1.0, 'Delay in seconds, after each retry.') * time.second))
	ctx.should_run = fp.bool('run', `R`, false, 'Run, after the script/program is completely downloaded.')
	ctx.delete_after_run = fp.bool('delete-after-run', `D`, false, 'Delete the downloaded script/program, after it has been run.')
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
	downloader := if os.is_atty(1) > 0 {
		&http.Downloader(http.TerminalStreamingDownloader{})
	} else {
		&http.Downloader(http.SilentStreamingDownloader{})
	}
	for idx, url in ctx.urls {
		fname := url.all_after_last('/')
		fpath := '${ctx.target_folder}/${fname}'
		mut file_errors := 0
		log.info('Downloading [${idx + 1}/${ctx.urls.len}] from url: ${url} to ${fpath} ...')
		for retry in 0 .. ctx.retries {
			http.download_file_with_progress(url, fname, downloader: downloader) or {
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

		if ctx.should_run {
			run_cmd := '${os.quoted_path(vexe)} run ${os.quoted_path(fpath)}'
			log.info(' Executing: ${run_cmd}')
			os.system(run_cmd)
		}
		if ctx.delete_after_run {
			log.info(' Removing: ${fpath}')
			os.rm(fpath) or {}
		}
		if !ctx.show_sha256 && !ctx.show_sha1 {
			continue
		}

		fbytes := os.read_bytes(fname)!
		if ctx.show_sha1 {
			mut digest1 := sha1.new()
			digest1.write(fbytes)!
			mut sum1 := digest1.sum([])
			hash1 := sum1.hex()
			log.info('                      SHA1: ${hash1}')
		}

		if ctx.show_sha256 {
			mut digest256 := sha256.new()
			digest256.write(fbytes)!
			mut sum256 := digest256.sum([])
			hash256 := sum256.hex()
			log.info('                    SHA256: ${hash256}')
		}
	}
	println('Downloaded: ${downloaded} file(s) . Elapsed time: ${sw.elapsed()} . Errors: ${errors} .')
	if !ctx.continue_on_failure && errors > 0 {
		exit(1)
	}
}
