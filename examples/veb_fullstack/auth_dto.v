module main

struct AuthRequestDto {
	username string @[nonull]
	password string @[nonull]
}
