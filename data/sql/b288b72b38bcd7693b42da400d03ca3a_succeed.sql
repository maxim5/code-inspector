connect "../test.gdb" user "SYSDBA" password "masterkey";
create table trivial (numbers integer);
commit;
insert into trivial values(1);
insert into trivial values(2);
commit;
drop table trivial;
commit;
quit;
