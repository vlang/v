interface UserRepository {
	find_by_id(id string) !User
}

struct CreateUserUseCase {
mut:
	repo UserRepository
}

fn new_create_user_usecase(mut repo UserRepository) CreateUserUseCase {
	return CreateUserUseCase{
		repo: repo
	}
}

struct SQLiteUserRepository {
mut:
	calls int
}

struct User {
	calls int
}

fn (mut repo SQLiteUserRepository) find_by_id(id string) !User {
	repo.calls += id.len
	return User{
		calls: repo.calls
	}
}

fn test_mut_interface_parameter_accepts_mut_receiver_implementation() {
	mut sqlite_repo := SQLiteUserRepository{}
	mut use_case := new_create_user_usecase(mut sqlite_repo)
	user := use_case.repo.find_by_id('1') or { panic(err) }
	assert user.calls == 1
	assert sqlite_repo.calls == 1
}
