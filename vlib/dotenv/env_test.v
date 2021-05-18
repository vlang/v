import dotenv
import os

fn test_env() {
	dotenv.loadenv()
	println(os.getenv('DATA'))
}