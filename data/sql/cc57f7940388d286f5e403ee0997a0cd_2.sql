# --- Sample dataset

# --- !Ups

insert into jogador (id,name,caminho) values (  1,'Neymar','imagens\neymar.png');
insert into jogador (id,name,caminho) values (  2,'Bernard','imagens\bernard.jpeg');
insert into jogador (id,name,caminho) values (  3,'Daniel A.','imagens\daniel.jpeg');
insert into jogador (id,name,caminho) values (  4,'Fernandinho','imagens\Fernandinho.jpeg');
insert into jogador (id,name,caminho) values (  5,'Fred','imagens\fred.jpeg');
insert into jogador (id,name,caminho) values (  6,'Dante','imagens\dante.jpeg');
insert into jogador (id,name,caminho) values (  7,'David','imagens\david.jpeg');
insert into jogador (id,name,caminho) values (  8,'Henrique','imagens\henrique.jpeg');
insert into jogador (id,name,caminho) values (  9,'Hernanes','imagens\Hernanes.jpeg');
insert into jogador (id,name,caminho) values ( 10,'Hulk','imagens\hulk.jpeg');
insert into jogador (id,name,caminho) values ( 11,'Jeferson','imagens\jeferson.jpeg');
insert into jogador (id,name,caminho) values ( 12,'J么','imagens\jo.jpeg');
insert into jogador (id,name,caminho) values ( 13,'J煤lio','imagens\julio.jpeg');
insert into jogador (id,name,caminho) values ( 14,'Luiz','imagens\luiz.jpeg');
insert into jogador (id,name,caminho) values ( 15,'Maicon','imagens\maicon.jpeg');
insert into jogador (id,name,caminho) values ( 16,'Marcelo','imagens\marcelo.jpeg');
insert into jogador (id,name,caminho) values ( 17,'Maxwell','imagens\maxwell.jpeg');
insert into jogador (id,name,caminho) values ( 18,'Jeferson','imagens\jeferson.jpeg');
insert into jogador (id,name,caminho) values ( 19,'Oscar','imagens\oscar.jpeg');
insert into jogador (id,name,caminho) values ( 20,'Paulinho','imagens\paulinho.jpeg');
insert into jogador (id,name,caminho) values ( 21,'Ramires','imagens\ramires.jpeg');
insert into jogador (id,name,caminho) values ( 22,'Thiago S.','imagens\thiago.jpeg');
insert into jogador (id,name,caminho) values ( 23,'Vitor','imagens\vitor.jpeg');


insert into posicoes (id,name,jogador_id) values (  1,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  2,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  3,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  4,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  5,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  6,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  7,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  8,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values (  9,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values ( 10,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values ( 11,'imagens\botao.jpg',null);
insert into posicoes (id,name,jogador_id) values ( 12,'imagens\botao.jpg',null);
# --- !Downs

delete from posicoes;
delete from jogador;
