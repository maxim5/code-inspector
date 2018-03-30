/*
By convention, the name of any column that contains values that are derived from other 
data should start with a underscore.
eg. widget._url is derived from the widget name and is updated when the name is 
updated, hence it is _url instead of url.
eg. widget.created doesn't have an underscore because even though it is always set 
internally by the application it cannot be derived from any other data.
*/
/*Drop in opposite order to avoid foreign key errors.*/
DROP TABLE IF EXISTS property;
DROP TABLE IF EXISTS login;
DROP TABLE IF EXISTS mail_log;
DROP TABLE IF EXISTS password_reset;
DROP TABLE IF EXISTS session;
DROP TABLE IF EXISTS user;

CREATE TABLE user(
	id INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	created DATETIME NOT NULL,
	modified DATETIME NOT NULL,
	name VARCHAR(255) NOT NULL DEFAULT '',
	email VARCHAR(255) NOT NULL,
	password VARCHAR(64) NOT NULL DEFAULT '',
	PRIMARY KEY (id),
	UNIQUE KEY (email)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE session (
	/*The CI docs say char(40), although in practise the value is 32 characters*/
	session_id CHAR(40) DEFAULT '' NOT NULL,
	ip_address VARCHAR(45) DEFAULT '0' NOT NULL,
	user_agent CHAR(120) NOT NULL,
	last_activity INTEGER UNSIGNED DEFAULT 0 NOT NULL,
	user_data TEXT NOT NULL DEFAULT '',
	user_id INTEGER UNSIGNED DEFAULT NULL,
	PRIMARY KEY (session_id),
	KEY (last_activity),
	FOREIGN KEY (user_id) REFERENCES user(id)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE password_reset(
	id INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	created DATETIME NOT NULL,
	user_id INTEGER UNSIGNED NOT NULL,
	token CHAR(64) NOT NULL, 
	expires DATETIME NOT NULL,
	PRIMARY KEY (id),
	UNIQUE KEY (user_id),
	UNIQUE KEY (token),
	FOREIGN KEY (user_id) REFERENCES user(id)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE mail_log(
	id INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	created DATETIME NOT NULL,
	`to` VARCHAR(255) NOT NULL DEFAULT '',
	`from` VARCHAR(255) NOT NULL DEFAULT '',
	subject TEXT NOT NULL,
	body TEXT NOT NULL,
	user_id INTEGER UNSIGNED DEFAULT NULL,
	PRIMARY KEY (id),
	FOREIGN KEY (user_id)  REFERENCES user(id)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE login(
	id INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	created DATETIME NOT NULL,
	modified DATETIME NOT NULL,
	user_id INTEGER UNSIGNED NOT NULL,
	token CHAR(128) NOT NULL DEFAULT '',
	series CHAR(128) NOT NULL DEFAULT '',
	previous_token CHAR(128) NOT NULL DEFAULT '', 
	PRIMARY KEY (id),
	UNIQUE KEY (user_id, series),
	FOREIGN KEY (user_id) REFERENCES user(id)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE property(
	id INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	name VARCHAR(255) NOT NULL,
	value TEXT DEFAULT NULL,
	PRIMARY KEY (id),
	UNIQUE KEY (name)
)ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT INTO property (name, value) VALUES ('database_version', 0);
