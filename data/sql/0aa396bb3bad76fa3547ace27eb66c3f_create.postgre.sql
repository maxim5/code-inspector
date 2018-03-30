/*
By convention, the name of any column that contains values that are derived from other 
data should start with a underscore.
eg. widget._url is derived from the widget name and is updated when the name is 
updated, hence it is _url instead of url.
eg. widget.created doesn't have an underscore because even though it is always set 
internally by the application it cannot be derived from any other data.
*/

CREATE TABLE "user"(
	id SERIAL PRIMARY KEY,
	created TIMESTAMP NOT NULL,
	modified TIMESTAMP NOT NULL,
	name VARCHAR(255) NOT NULL DEFAULT '',
	email VARCHAR(255) NOT NULL UNIQUE,
	password VARCHAR(64) NOT NULL DEFAULT ''
);

CREATE TABLE session (
	/*the CI docs say char(40), although in practise the value is 32 characters*/
	session_id CHAR(40) DEFAULT '' NOT NULL PRIMARY KEY,
	ip_address VARCHAR(45) DEFAULT '0' NOT NULL,
	user_agent CHAR(120) NOT NULL,
	last_activity INTEGER DEFAULT 0 NOT NULL,
	user_data TEXT NOT NULL DEFAULT '',
	user_id INTEGER DEFAULT NULL REFERENCES "user"(id)
);
CREATE INDEX session_last_activity_index ON session(last_activity);

CREATE TABLE password_reset(
	id SERIAL PRIMARY KEY,
	created TIMESTAMP NOT NULL,
	user_id INTEGER NOT NULL UNIQUE REFERENCES "user"(id),
	token CHAR(64) NOT NULL DEFAULT '' UNIQUE,
	expires TIMESTAMP NOT NULL
);

CREATE TABLE mail_log(
	id SERIAL PRIMARY KEY,
	created TIMESTAMP NOT NULL,
	"to" VARCHAR(255) NOT NULL DEFAULT '',
	"from" VARCHAR(255) NOT NULL DEFAULT '',
	subject TEXT NOT NULL,
	body TEXT NOT NULL,
	user_id INTEGER DEFAULT NULL REFERENCES "user"(id)
);

CREATE TABLE login(
	id SERIAL PRIMARY KEY,
	created TIMESTAMP NOT NULL,
	modified TIMESTAMP NOT NULL,
	user_id INTEGER NOT NULL REFERENCES "user"(id),
	token CHAR(128) NOT NULL DEFAULT '',
	series CHAR(128) NOT NULL DEFAULT '',
	previous_token CHAR(128) NOT NULL DEFAULT ''
);
CREATE UNIQUE INDEX login_user_id_series_index ON login(user_id, series);

CREATE TABLE property(
	id SERIAL PRIMARY KEY,
	name VARCHAR(255) NOT NULL UNIQUE,
	value TEXT DEFAULT NULL
);
INSERT INTO property (name, value) VALUES ('database_version', 0);
