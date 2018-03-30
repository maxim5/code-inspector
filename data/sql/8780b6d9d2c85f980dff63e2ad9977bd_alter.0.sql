alter table object_id change column type type ENUM('kernel','note','relationship','sandbox') NOT NULL;

insert into object_id (user,type) select user.id, 'sandbox' from user;


------

CREATE TABLE IF NOT EXISTS user_type (
    id MEDIUMINT UNSIGNED PRIMARY KEY AUTO_INCREMENT,
    name varchar(255),
    description varchar(255)
);

alter table user add column user_type INT NOT NULL;

alter table user add foreign key (user_type) REFERENCES user_type (id);

insert into user_type values (1, 'unregistered_trial_user', "Nonpaying trial user that hasn't registered yet - subject to periodc delete");
insert into user_type values (2, 'trial_user', "Nonpaying trial user");
insert into user_type values (3, 'tutorial_user', "Temporary tutorial user - periodically deleted");
insert into user_type values (4, 'paying_user', "Paying user");
insert into user_type values (5, 'admin_user', "Administrator");

update user set user_type=2;
update user set user_type=3 where username like 'tutorial%';
update user set user_type=5 where username = 'tutorial_template';
