struct User {
	int age;
	string name;
};

// multi return structs
// end of definitions #endif
int main() {
	User user = (User){
};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	return 0;
}
