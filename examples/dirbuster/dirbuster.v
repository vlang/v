import os
import net.http

fn main() {
	mut target := 'https://vlang.io/'
	file := 'example.txt'

	list := os.read_lines(file)?

	
	
	for item in list {
		url := target + item
		println("Sending to URL: " + url)
		request := http.get(url)?
		println(request)
	}
}