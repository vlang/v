module amd64

@[cold]
fn lowering_error(context string, detail string) IError {
	return error('amd64: ${context}: ${detail}')
}
