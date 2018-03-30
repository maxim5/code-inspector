/**********************************************************************************************************************
* Simple ASP.NET Providers - database creation script
* Copyright © Michal A. Valasek - Altairis, 2006-2009 | www.altairis.cz | www.rider.cz
* Licensed under terms of Microsoft Shared Source Permissive License (MS-PL)
*********************************************************************************************************************/

-- Create table for membership provider
CREATE TABLE dbo.Users(
	UserId					int IDENTITY(1,1)	NOT NULL,
	UserName				varchar(100)		NOT NULL,
	PasswordHash			char(86)			NOT NULL,
	PasswordSalt			char(5)				NOT NULL,
	Email					varchar(100)		NOT NULL,
	Comment					text				NULL,
	Enabled					bit					NOT NULL,
	DateCreated				datetime			NOT NULL,
	DateLastLogin			datetime			NULL,
	DateLastActivity		datetime			NULL,
	DateLastPasswordChange	datetime			NOT NULL,
	
	CONSTRAINT PK_Users PRIMARY KEY CLUSTERED (UserId ASC),
	CONSTRAINT IX_Users_UserName UNIQUE NONCLUSTERED (UserName ASC)
)

-- Create tables for role provider
CREATE TABLE dbo.Roles(
	RoleName				varchar(100)		NOT NULL,
	
	CONSTRAINT PK_Roles PRIMARY KEY CLUSTERED (RoleName ASC)
)

CREATE TABLE dbo.UsersInRoles(
	HashId					int IDENTITY(1,1)	NOT NULL,
	UserName				varchar(100)		NOT NULL,
	RoleName				varchar(100)		NOT NULL,
	
	CONSTRAINT PK_UsersInRoles PRIMARY KEY CLUSTERED (HashId ASC),
	CONSTRAINT FK_UsersInRoles_Roles FOREIGN KEY (RoleName) REFERENCES dbo.Roles (RoleName) ON UPDATE CASCADE ON DELETE CASCADE,
	CONSTRAINT FK_UsersInRoles_Users FOREIGN KEY (UserName) REFERENCES dbo.Users (UserName) ON UPDATE CASCADE ON DELETE CASCADE
)
