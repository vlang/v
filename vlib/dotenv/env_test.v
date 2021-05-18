import dotenv
import os

fn test_env() {
	dotenv.loadenv() or {
		panic(err)
		return
	}
	println(os.getenv('DATA'))
}