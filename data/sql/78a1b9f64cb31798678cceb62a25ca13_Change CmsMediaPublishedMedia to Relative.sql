-- Example from http://localhost:12345/Content/CmsMedia/ to /Content/Media/



DECLARE @CurrentCMSUrl NVARCHAR(500)
SET @CurrentCMSUrl = 'http://localhost:12345/Content/CmsMedia/'

DECLARE @NewCMSUrl NVARCHAR(500)
SET @NewCMSUrl = '/Content/Media/'
-----------------------------------------


UPDATE Page 
SET [Image] =
REPLACE(CAST([Image] AS NVARCHAR(max)), @CurrentCMSUrl, @NewCMSUrl)

UPDATE ContentItem 
SET [Description] =
REPLACE(CAST([Description] AS NVARCHAR(max)), @CurrentCMSUrl, @NewCMSUrl)

UPDATE ContentItem 
SET [Image] =
REPLACE(CAST([Image] AS NVARCHAR(max)), @CurrentCMSUrl, @NewCMSUrl)

UPDATE VersionedPage 
SET [Image] =
REPLACE(CAST([Image] AS NVARCHAR(max)), @CurrentCMSUrl, @NewCMSUrl)

UPDATE VersionedContentItem 
SET [Description] =
REPLACE(CAST([Description] AS NVARCHAR(max)), @CurrentCMSUrl, @NewCMSUrl)
