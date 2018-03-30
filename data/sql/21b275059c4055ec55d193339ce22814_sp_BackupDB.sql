/* Example Usage
-- exec sp_BackupDB @DatabaseName = 'DBAInfrastructure', @BackupPath = 'D:\SQLDBFiles\Backups\'
*/

use master;
go

if object_id('sp_BackupDB','P') is not null
  drop procedure dbo.sp_BackupDB
go

create procedure dbo.sp_BackupDB (@DatabaseName varchar(100),
                                  @BackupPath varchar(500)) as
begin                                  
  set nocount on;

  declare @BackupName varchar(500),
          @BackupPathFileName varchar(1000);
  
  if right(@BackupPath,1) != '\'
    set @BackupPath = @BackupPath + '\'
    
  set @BackupPathFileName = @BackupPath + @DatabaseName + '\' + @DatabaseName + '_backup_' + replace(replace(replace(replace(convert(varchar(23),getdate(),126),'-',''),':',''),'T',''),'.','') + '.bak'

  set @BackupName = @DatabaseName + '_backup_' + replace(replace(replace(replace(convert(varchar(23),getdate(),126),'-',''),':',''),'T',''),'.','')

  backup database @DatabaseName 
      to disk = @BackupPathFileName 
    with noformat, noinit, name = @BackupName, skip, rewind, nounload,  stats = 10

  declare @backupSetId as int

  select @backupSetId = position 
    from msdb..backupset 
   where database_name = @DatabaseName 
     and backup_set_id = (select max(backup_set_id) 
                            from msdb..backupset 
                           where database_name = @DatabaseName)
                           
  if @backupSetId is null 
    begin 
      declare @ErrorMSG varchar(200);
      set @ErrorMSG = 'Verify failed. Backup information for database ''' + @DatabaseName + ''' not found.'
      raiserror(@ErrorMSG, 16, 1) 
    end

  restore verifyonly 
     from disk = @BackupPathFileName
     with file = @backupSetId, nounload, norewind

end