-- Simple entity, used an RHS for un-mirrored relationships
CREATE TABLE simple_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255),
  value varchar(255)
);

-- OneToMany relationship - Not mirrored
CREATE TABLE one_to_many_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255)
);

CREATE TABLE one_to_many_rhs(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255),
  one_to_many_entity_id integer NULL REFERENCES one_to_many_entity(id)
);

-- ManyToOne relationship - Not mirrored
CREATE TABLE many_to_one_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255),
  simple_entity_id integer NOT NULL REFERENCES simple_entity (id)
);

-- ManyToMany relationship - Not mirrored
CREATE TABLE many_to_many_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255)
);

CREATE TABLE many_to_many_entity_simple_entity_link(
  simple_entity_id integer NOT NULL REFERENCES simple_entity (id),
  many_to_many_entity_id integer NOT NULL REFERENCES many_to_many_entity (id),

  PRIMARY KEY (simple_entity_id, many_to_many_entity_id)
);

-- ManyToMany relationship - Mirrored
CREATE TABLE many_to_many_lhs_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255)
);

CREATE TABLE many_to_many_rhs_entity(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255)
);

CREATE TABLE many_to_many_lhs_entity_many_to_many_rhs_entity_link(
  id integer PRIMARY KEY AUTOINCREMENT,
  many_to_many_lhs_entity_id integer NOT NULL REFERENCES many_to_many_lhs_entity (id),
  many_to_many_rhs_entity_id integer NOT NULL REFERENCES many_to_many_rhs_entity (id)
);

-- OneToMany/ManyToOne mirrored relationship
CREATE TABLE one_to_many_mirror(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255)
);

CREATE TABLE many_to_one_mirror(
  id integer PRIMARY KEY AUTOINCREMENT,
  name varchar(255),
  one_to_many_mirror_id integer REFERENCES one_to_many_mirror (id)
);
