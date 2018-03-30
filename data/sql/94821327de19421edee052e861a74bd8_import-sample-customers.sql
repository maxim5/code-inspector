--
--	Sample customers to populate database for acceptance tests.
--

	INSERT INTO cus_customer(id, name) VALUES ('C.0001', 'Jan Kowalski');
	INSERT INTO cus_customer(id, name) VALUES ('C.0002', 'Adam Mickiewicz');
	INSERT INTO cus_customer(id, name) VALUES ('C.0003', 'Adam Nowak');

	INSERT INTO cus_address(id, country, city, street, zip_code) VALUES ('A.0001', 'Poland', 'Warszawa', 'Al. Jerozolimskie', '00-999');
	INSERT INTO cus_cust_address(address_id, customer_id, address_type) VALUES ('A.0001', 'C.0001', 'MAIN');

		