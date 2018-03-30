-- 
-- Simple patch, add new fields to entity links

-- 4/20/2007

ALTER TABLE entity_links ADD COLUMN user_id INT(11);
ALTER TABLE entity_links ADD COLUMN full_name VARCHAR(128) NOT NULL;

--UPDATE entity_links set full_name = 'botrover99';

-- End of File