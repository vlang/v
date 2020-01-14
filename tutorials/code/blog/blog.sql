create database blog;

\c blog

drop table articles;

create table articles (
	id serial primary key,
	title text default '',
	text text default ''
);

insert into articles (title, text) values (
	'Hello, world!',
	'V is great.'
);

insert into articles (title, text) values (
	'Second post.',
	'Hm... what should I write about?'
);

