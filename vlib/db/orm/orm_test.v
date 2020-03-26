//import pg
 
struct Modules {
	id int 
	user_id int 
	name string 
	url string
	//nr_downloads int 
}

fn test_orm() {
/* 
	db := pg.connect('vpm', 'alex')
	//nr_modules := db.select count from modules  
	//nr_modules := db.select count from Modules where id == 1 
	nr_modules := db.select count from Modules where  
		name == 'Bob' && id == 1 
	println(nr_modules) 
 
	mod := db.select from Modules where id = 1 limit 1 
	println(mod) 

	mods := db.select from Modules limit 10 
	for mod in mods { 
	println(mod) 
	} 
*/ 

/* 
	mod := db.retrieve<Module>(1) 

	mod := db.update Module set name = name + '!' where id > 10 


	nr_modules := db.select count from Modules 
		where id > 1 && name == '' 
	println(nr_modules) 

	nr_modules := db.select count from modules 
	nr_modules := db.select from modules 
	nr_modules := db[:modules].select 
*/ 
/* 
	mod := select from db.modules where id = 1 limit 1 
	println(mod.name) 
	top_mods := select from db.modules where nr_downloads > 1000 order by nr_downloads desc limit 10 
	top_mods := db.select from modules where nr_downloads > 1000 order by nr_downloads desc limit 10 
	top_mods := db.select<Module>(m => m.nr_downloads > 1000).order_by(m => m.nr_downloads).desc().limit(10) 
	names := select name from db.modules // []string 


	n := db.q_int('select count(*) from modules') 
	println(n) 
*/ 
} 
