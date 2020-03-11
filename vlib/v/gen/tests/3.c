struct User {
	int age;
	string name;
};

// multi return structs
// end of definitions #endif

void User_foo(User* u);

void User_foo(User* u) {
	int age = u->age;
}

int main() {
	User user = (User){
};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	return 0;
}
