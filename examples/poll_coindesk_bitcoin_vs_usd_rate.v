import log
import term
import time
import x.json2
import net.http

const url = 'https://api.coinbase.com/v2/prices/BTC-USD/spot'

struct JsonResult {
mut:
	data PriceResult
}

struct PriceResult {
mut:
	amount   f64
	base     string
	currency string
}

fn main() {
	log.use_stdout()
	mut old_rate := f64(0)
	for i := u64(1); true; i++ {
		data := http.get(url) or {
			log.error('polling ${url} failed')
			time.sleep(10 * time.second)
			continue
		}
		if data.status_code == 200 {
			res := json2.decode[JsonResult](data.body) or {
				log.error('can not decode data: ${data.body}')
				time.sleep(10 * time.second)
				continue
			}
			new_rate := res.data.amount
			show_result(i, res.data, new_rate - old_rate)
			old_rate = new_rate
		}
		time.sleep(3 * time.second)
	}
}

fn show_result(i u64, res PriceResult, delta f64) {
	if delta == 0 {
		return
	}
	mut sdelta := '${delta:10.3f}'
	color := if delta > 0 {
		term.green
	} else if delta == 0 {
		term.white
	} else {
		term.red
	}
	cdelta := term.colorize(color, sdelta)
	log.info('${cdelta}, ${res.amount:10.3f} USD/BTC, cycle: ${i:5}')
}
