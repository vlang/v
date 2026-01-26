// vtest retry: 3
import db.sqlite

// Basic embedded struct
struct Coordinates {
	latitude  f64
	longitude f64
}

// Struct with embedded Coordinates
struct Location {
	Coordinates
	id   int @[primary; sql: serial]
	name string
}

// Another embedded struct with different types
struct Metadata {
	created_by string
	version    int
	is_active  bool
}

// Struct with multiple fields and embedded struct
struct Document {
	Metadata
	id      int @[primary; sql: serial]
	title   string
	content string
}

// Struct embedding another struct that has various field types
struct PersonInfo {
	first_name string
	last_name  string
	age        int
}

struct Employee {
	PersonInfo
	id         int @[primary; sql: serial]
	department string
	salary     f64
}

// Test basic embedded struct insert and select
fn test_embedded_struct_insert_and_select() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	loc := Location{
		name:      'Paris'
		latitude:  48.8566
		longitude: 2.3522
	}

	sql db {
		insert loc into Location
	}!

	locations := sql db {
		select from Location
	}!

	assert locations.len == 1
	fetched := locations.first()
	assert fetched.name == 'Paris'
	assert fetched.latitude == 48.8566
	assert fetched.longitude == 2.3522
}

// Test multiple inserts with embedded struct
fn test_embedded_struct_multiple_inserts() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	locations_to_insert := [
		Location{
			name:      'New York'
			latitude:  40.7128
			longitude: -74.0060
		},
		Location{
			name:      'Tokyo'
			latitude:  35.6762
			longitude: 139.6503
		},
		Location{
			name:      'Sydney'
			latitude:  -33.8688
			longitude: 151.2093
		},
	]

	for loc in locations_to_insert {
		sql db {
			insert loc into Location
		}!
	}

	locations := sql db {
		select from Location
	}!

	assert locations.len == 3

	// Verify each location
	assert locations[0].name == 'New York'
	assert locations[0].latitude == 40.7128
	assert locations[0].longitude == -74.0060

	assert locations[1].name == 'Tokyo'
	assert locations[1].latitude == 35.6762
	assert locations[1].longitude == 139.6503

	assert locations[2].name == 'Sydney'
	assert locations[2].latitude == -33.8688
	assert locations[2].longitude == 151.2093
}

// Test embedded struct with different field types (string, int, bool)
fn test_embedded_struct_various_types() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Document
	}!

	doc := Document{
		title:      'Test Document'
		content:    'This is the content'
		created_by: 'admin'
		version:    1
		is_active:  true
	}

	sql db {
		insert doc into Document
	}!

	docs := sql db {
		select from Document
	}!

	assert docs.len == 1
	fetched := docs.first()
	assert fetched.title == 'Test Document'
	assert fetched.content == 'This is the content'
	assert fetched.created_by == 'admin'
	assert fetched.version == 1
	assert fetched.is_active == true
}

// Test embedded struct with bool false value
fn test_embedded_struct_bool_false() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Document
	}!

	doc := Document{
		title:      'Inactive Doc'
		content:    'Content here'
		created_by: 'user'
		version:    2
		is_active:  false
	}

	sql db {
		insert doc into Document
	}!

	docs := sql db {
		select from Document
	}!

	assert docs.len == 1
	assert docs.first().is_active == false
}

// Test employee with person info embedded
fn test_employee_with_embedded_person_info() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Employee
	}!

	emp := Employee{
		first_name: 'John'
		last_name:  'Doe'
		age:        30
		department: 'Engineering'
		salary:     75000.50
	}

	sql db {
		insert emp into Employee
	}!

	employees := sql db {
		select from Employee
	}!

	assert employees.len == 1
	fetched := employees.first()
	assert fetched.first_name == 'John'
	assert fetched.last_name == 'Doe'
	assert fetched.age == 30
	assert fetched.department == 'Engineering'
	assert fetched.salary == 75000.50
}

// Test selecting specific records with where clause on non-embedded field
fn test_embedded_struct_select_with_where() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Employee
	}!

	employees_to_insert := [
		Employee{
			first_name: 'Alice'
			last_name:  'Smith'
			age:        25
			department: 'HR'
			salary:     50000.0
		},
		Employee{
			first_name: 'Bob'
			last_name:  'Jones'
			age:        35
			department: 'Engineering'
			salary:     80000.0
		},
		Employee{
			first_name: 'Carol'
			last_name:  'Williams'
			age:        28
			department: 'Engineering'
			salary:     70000.0
		},
	]

	for emp in employees_to_insert {
		sql db {
			insert emp into Employee
		}!
	}

	// Select by department
	engineers := sql db {
		select from Employee where department == 'Engineering'
	}!

	assert engineers.len == 2
	assert engineers[0].first_name == 'Bob'
	assert engineers[1].first_name == 'Carol'

	// Verify all embedded fields are correctly fetched
	for eng in engineers {
		assert eng.last_name.len > 0
		assert eng.age > 0
		assert eng.salary > 0
	}
}

// Test that embedded struct fields are stored with correct column names
fn test_embedded_struct_field_values_integrity() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	// Insert with specific values that could be confused
	loc := Location{
		name:      'Test'
		latitude:  12.345
		longitude: 67.890
	}

	sql db {
		insert loc into Location
	}!

	locations := sql db {
		select from Location
	}!

	fetched := locations.first()

	// Ensure latitude and longitude are not swapped
	assert fetched.latitude == 12.345
	assert fetched.longitude == 67.890
	// Ensure they're not equal (which would indicate a bug)
	assert fetched.latitude != fetched.longitude
}

// Test update on non-embedded fields preserves embedded field values
fn test_update_preserves_embedded_fields() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	loc := Location{
		name:      'Original'
		latitude:  10.0
		longitude: 20.0
	}

	sql db {
		insert loc into Location
	}!

	// Update only the name
	sql db {
		update Location set name = 'Updated' where id == 1
	}!

	locations := sql db {
		select from Location where id == 1
	}!

	fetched := locations.first()
	assert fetched.name == 'Updated'
	// Embedded fields should be preserved
	assert fetched.latitude == 10.0
	assert fetched.longitude == 20.0
}

// Test delete with embedded struct
fn test_delete_with_embedded_struct() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	loc1 := Location{
		name:      'ToDelete'
		latitude:  1.0
		longitude: 2.0
	}

	loc2 := Location{
		name:      'ToKeep'
		latitude:  3.0
		longitude: 4.0
	}

	sql db {
		insert loc1 into Location
		insert loc2 into Location
	}!

	sql db {
		delete from Location where name == 'ToDelete'
	}!

	locations := sql db {
		select from Location
	}!

	assert locations.len == 1
	assert locations.first().name == 'ToKeep'
	assert locations.first().latitude == 3.0
	assert locations.first().longitude == 4.0
}

// Test empty/zero values in embedded struct
fn test_embedded_struct_zero_values() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Employee
	}!

	emp := Employee{
		first_name: ''
		last_name:  ''
		age:        0
		department: 'Test'
		salary:     0.0
	}

	sql db {
		insert emp into Employee
	}!

	employees := sql db {
		select from Employee
	}!

	assert employees.len == 1
	fetched := employees.first()
	assert fetched.first_name == ''
	assert fetched.last_name == ''
	assert fetched.age == 0
	assert fetched.salary == 0.0
}

// Test that the struct equality works correctly with embedded fields
fn test_embedded_struct_equality() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Location
	}!

	original := Location{
		name:      'TestCity'
		latitude:  45.0
		longitude: 90.0
	}

	sql db {
		insert original into Location
	}!

	locations := sql db {
		select from Location
	}!

	fetched := locations.first()

	// Compare all fields individually
	assert fetched.name == original.name
	assert fetched.latitude == original.latitude
	assert fetched.longitude == original.longitude
}
