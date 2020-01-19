typedef struct {
	int age;
	string name;
} User;

int main() {
	User user = (User){
};
	user.age = 10;
	user.age++;
	user.name = tos3("bob");
	return 0;
}
