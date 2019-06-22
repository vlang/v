module gl

fn init_glad() {
	println('init_glad() win')
	ok := false
	# ok= gladLoadGLLoader((GLADloadproc) glfwGetProcAddress);
	if !ok {
		os.exit('Failed to initialize glad OpenGL context')
	}
	# printf("glClear ADDR=%p\n", glClear);
}

