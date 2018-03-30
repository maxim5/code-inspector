use northwind
select productname, unitprice, address,postalcode, city, country from products inner join suppliers on products.supplierid = suppliers.supplierid where unitprice between 20 and 30
select productname, unitsinstock from products inner join suppliers on products.supplierid = suppliers.supplierid where CompanyName like 'Tokyo Traders'
select productname, companyname, phone from products inner join suppliers on products.supplierid = suppliers.supplierid where unitsinstock = 0
select distinct companyname, postalcode, city, address from Customers left outer join Orders on Orders.CustomerID = Customers.CustomerID and year(OrderDate)=1997 where year(OrderDate) is null
select productname, companyname, phone from products inner join suppliers on products.supplierid = suppliers.supplierid where unitsinstock = 0
select productname, unitprice, postalcode, city, address, country from products inner join categories on products.categoryid = categories.categoryid inner join suppliers on products.supplierid = suppliers.supplierid where CategoryName like 'Meat/Poultry' and UnitPrice between 20 and 30
select productname, unitprice, companyname from products inner join categories on products.categoryid = categories.categoryid inner join suppliers on products.supplierid = suppliers.supplierid where CategoryName like 'Confections'
select Boss.firstname+' '+Boss.lastname as [Boss Name], Worker.firstname+' '+Worker.lastname as [Worker Name] from Employees as Boss join Employees as Worker on Boss.EmployeeID=Worker.reportsTo
select Boss.firstname+' '+Boss.lastname as [Bosses Nobody] from Employees as Boss left outer join Employees as Worker on Boss.EmployeeID=Worker.reportsTo where Worker.firstname is null
select (firstname + ' ' + lastname) as name ,city, postalcode, 'employee' as [type] from employees union select companyname, city, postalcode, 'customer' as [type] from customers union select companyname, city, postalcode, 'supplier' as [type] from suppliers order by type

use joindb
select buy1.buyer_name AS buyer1, prod.prod_name, buy2.buyer_name as buyer2 from sales as a join buyers buy1 on a.buyer_id = buy1.buyer_id  join sales as b on a.prod_id = b.prod_id  join buyers as buy2 on b.buyer_id = buy2.buyer_id join produce as prod on prod.prod_id=a.prod_id where buy1.buyer_id > buy2.buyer_id

use library
select member.firstname, member.lastname, juvenile.birth_date from member inner join juvenile on member.member_no =juvenile.member_no
select distinct title.title from loan inner join title on loan.title_no = title.title_no
select loanhist.in_date, datediff(day,loanhist.due_date, loanhist.in_date) as days, fine_paid from title inner join loanhist on title.title_no = loanhist.title_no where title='Tao Teh King' and (datediff(day,loanhist.due_date, loanhist.in_date) > 0)
select reservation.isbn from reservation inner join member on reservation.member_no=member.member_no where (lastname+firstname)='Stephen A. Graff'
select member.firstname, member.lastname, juvenile.birth_date, adult.street, adult.city from member inner join juvenile on member.member_no =juvenile.member_no inner join adult on juvenile.adult_member_no = adult.member_no
select m.firstname, m.lastname, j.birth_date, a.street, a.city, ma.firstname as ParentName, ma.lastname as ParentLastName from member as m inner join juvenile as j on m.member_no =j.member_no inner join adult as a on j.adult_member_no = a.member_no inner join member as ma on ma.member_no = a.member_no
select adult.street, adult.city from juvenile inner join adult on juvenile.adult_member_no = adult.member_no where juvenile.birth_Date < '1996-01-01'
select distinct (mn.firstname+' '+mn.lastname) as [Name],(adult.street+' '+adult.city) as [Address] from juvenile  inner join adult on juvenile.adult_member_no = adult.member_no inner join member as mn on adult.member_no = mn.member_no left outer join loan on adult.member_no=loan.member_no  where juvenile.birth_Date < '1996-01-01' and (loan.due_date is null or loan.due_date>getDate())
select (firstname+' '+lastname) as Name, (street+' '+city+' '+state+' '+zip) as address from adult inner join member on adult.member_no = member.member_no
select count(adult_member_no) as [Kids], (member.firstname+' '+member.lastname) as [Adult Name] from juvenile inner join adult on juvenile.adult_member_no = adult.member_no inner join member on adult.member_no = member.member_no where adult.state like 'AZ' group by member.firstname, member.lastname, member.member_no having count(*)>2 union select count(adult_member_no) as [Kids], (member.firstname+' '+member.lastname) as [Adult Name] from juvenile  inner join adult on juvenile.adult_member_no = adult.member_no inner join member on adult.member_no = member.member_no where adult.state like 'CA' group by member.firstname, member.lastname, member.member_no having count(*)>2
