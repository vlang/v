import log
import term
import time
import x.json2
import net.http

const url = 'https://api.coindesk.com/v1/bpi/currentprice.json'

fn main() {
	mut old_rate := f64(0)
	for i := u64(1); true; i++ {
		data := http.get(url) or {
			log.error('polling ${url} failed')
			time.sleep(10 * time.second)
			continue
		}
		if data.status_code == 200 {
			res := json2.decode[PriceResult](data.body) or {
				log.error('can not decode data: ${data.body}')
				time.sleep(10 * time.second)
				continue
			}
			new_rate := res.bpi['USD'].rate_float
			show_result(i, res, new_rate - old_rate)
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
	log.info('${cdelta}, ${res.bpi['USD'].rate_float:10.3f} USD/BTC, ${res.time.updated_iso}, cycle: ${i:5}')
}

struct PriceTime {
	updated_iso time.Time @[json: 'updatedISO']
	updated     string
	updateduk   string
}

struct CurrencyResult {
	code        string
	symbol      string
	rate        string
	description string
	rate_float  f64
}

struct PriceResult {
	time       PriceTime
	disclaimer string
	chart_name string @[json: 'chartName']
	bpi        map[string]CurrencyResult
}
