/*union*/
select distinct o.state from OILPRODBYSTATEYEAR o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.OILWELLS>0
union
select distinct o.state from GasProdByStateYear o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.GASWELLS>0;


/*Intersect*/
select distinct o.state from OILPRODBYSTATEYEAR o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.OILWELLS>0
intersect
select distinct o.state from GasProdByStateYear o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.GASWELLS>0;

/*Except*/
select distinct o.state from GasProdByStateYear o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.GASWELLS>0
except
select distinct o.state from OILPRODBYSTATEYEAR o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.OILWELLS>0;

/*Except*/
select distinct o.state from OILPRODBYSTATEYEAR o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.OILWELLS>0
except
select distinct o.state from GasProdByStateYear o join abr a on o.state = a.ABR join STATEGEO g on upper(a.state) = upper(g.STATE) where o.PROD_YEAR = '2009' and o.GASWELLS>0;

