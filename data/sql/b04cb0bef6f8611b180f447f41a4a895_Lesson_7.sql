/*Join*/
select * from STATEGEO g join abr a on upper(g.STATE) = upper(a.STATE);