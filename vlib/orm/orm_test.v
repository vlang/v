//import pg
 
struct Mod {
	id int 
	name string 
	url string
	nr_downloads int 
}

fn test_orm() {
/* 
	db := pg.connect('vpm', 'alex')
	nr_modules := select count from db.modules 
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
