--IMPLEMENTATION OF VIEWS
CREATE VIEW clinet_income_vw
	AS SELECT a.id_client, a.surname, a.first_name, a.middle_name, a.home_phone, a.mobile_phone, a.mail,
		  b.id_report, b.airtickets_price, b.service_price, b.hotel_price, b.fines, b.representative_costs, b.client_paid, b.income 
		FROM CLIENT a, FINANCIAL_REPORT b
			WHERE a.id_client = b.id_client
			AND b.income>=2000;
			
--RSTRICTING ACCESS
GRANT SELECT ON clinet_income_vw TO BD_admin, Logist, Administrator;

--INSERT
CREATE RULE clinet_income_in AS ON INSERT TO clinet_income_vw
        DO INSTEAD (
        INSERT INTO FINANCIAL_REPORT (id_client, airtickets_price, service_price, hotel_price, fines, representative_costs, client_paid, income)
        VALUES (NEW.id_report, NEW.airtickets_price, NEW.service_price, NEW.hotel_price, NEW.fines, NEW.representative_costs, NEW.client_paid, NEW.income);    
        );

--UPDATE 
CREATE RULE clinet_income_up AS ON UPDATE TO clinet_income_vw 
	DO INSTEAD (
        UPDATE FINANCIAL_REPORT
        SET airtickets_price=NEW.airtickets_price, service_price=NEW.service_price, hotel_price=NEW.hotel_price, fines=NEW.fines, 
            representative_costs=NEW.representative_costs, client_paid=NEW.client_paid, income=NEW.income 
                WHERE id_report= OLD.id_report;
       );
         
--DELETE 
CREATE OR REPLACE RULE clinet_income_del AS ON DELETE TO clinet_income_vw
	DO INSTEAD (
	DELETE FROM FINANCIAL_REPORT 
		WHERE id_report = OLD.id_report;
	DELETE FROM CLIENT 
		WHERE id_client = OLD.id_client;
	);

--TRIGGER
CREATE OR REPLACE FUNCTION clinet_income()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $function$
   BEGIN
        IF res = 'INSERT' THEN
		INSERT INTO FINANCIAL_REPORT (id_client, airtickets_price, service_price, hotel_price, fines, representative_costs, client_paid, income)
		VALUES (NEW.id_report, NEW.airtickets_price, NEW.service_price, NEW.hotel_price, NEW.fines, NEW.representative_costs, NEW.client_paid, NEW.income);
		RETURN NEW;
        
	ELSIF res = 'UPDATE' THEN
		UPDATE FINANCIAL_REPORT
		SET airtickets_price=NEW.airtickets_price, service_price=NEW.service_price, hotel_price=NEW.hotel_price, fines=NEW.fines, 
			representative_costs=NEW.representative_costs, client_paid=NEW.client_paid, income=NEW.income 
		WHERE id_report= OLD.id_report;
		RETURN NEW;
        
        ELSIF res = 'DELETE' THEN
                DELETE FROM FINANCIAL_REPORT 
		WHERE id_report = OLD.id_report;
	DELETE FROM CLIENT 
		WHERE id_client = OLD.id_client;
        RETURN NULL;
      END IF;
      RETURN NEW;
    END;
$function$;

CREATE TRIGGER clinet_income
    INSTEAD OF INSERT OR UPDATE OR DELETE ON clinet_income_vw 
    FOR EACH ROW EXECUTE PROCEDURE clinet_income();