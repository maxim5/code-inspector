-- 
-- Simple patch, add new fields to entity links

-- 4/20/2007

ALTER TABLE entity_links ADD COLUMN rating INT(11) NOT NULL DEFAULT 0;

-- End of File