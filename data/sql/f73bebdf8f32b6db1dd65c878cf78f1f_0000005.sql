--Sample DDL Trigger
IF EXISTS (SELECT * FROM sys.triggers WHERE parent_class = 0 AND name = 'synonym_safety')
DROP TRIGGER synonym_safety
ON DATABASE;
GO

CREATE TRIGGER synonym_safety
ON DATABASE 
FOR DROP_SYNONYM
AS 
   RAISERROR ('You must disable Trigger "synonym_safety" to drop synonyms!',10, 1)
   ROLLBACK
GO

DROP TRIGGER synonym_safety
ON DATABASE;
GO


