CREATE FULLTEXT CATALOG [RiffFullText]
GO

CREATE TABLE [dbo].[Term](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[Word] [nvarchar](50) NOT NULL,
 CONSTRAINT [PK_dbo.Term] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Document](
	[Id] [varchar](50) NOT NULL,
 CONSTRAINT [PK_Document] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Attachment](
	[ItemId] [uniqueidentifier] NULL,
	[UserId] [uniqueidentifier] NULL,
	[Id] [uniqueidentifier] NOT NULL,
	[FileName] [varchar](255) NULL,
	[ContentLength] [int] NULL,
	[ContentType] [varchar](255) NULL,
 CONSTRAINT [PK_Attachment] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Hypothesis](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[RiffId] [int] NOT NULL,
	[ShortName] [varchar](255) NOT NULL,
	[OwnerId] [uniqueidentifier] NOT NULL,
	[Title] [varchar](255) NOT NULL,
	[Description] [ntext] NOT NULL,
 CONSTRAINT [PK_Hypothesis] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[CronState](
	[CronableName] [varchar](300) NOT NULL,
	[LastExecution] [datetime] NOT NULL,
 CONSTRAINT [PK_CronState] PRIMARY KEY CLUSTERED 
(
	[CronableName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[BlacklistTerm](
	[Term] [nvarchar](255) NOT NULL,
 CONSTRAINT [PK_BlacklistTerm] PRIMARY KEY CLUSTERED 
(
	[Term] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[BasketItem](
	[UserId] [uniqueidentifier] NOT NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_Clipboard] PRIMARY KEY CLUSTERED 
(
	[UserId] ASC,
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_CALENDARS](
	[CALENDAR_NAME] [varchar](200) NOT NULL,
	[CALENDAR] [image] NOT NULL,
 CONSTRAINT [PK_QRTZ_CALENDARS] PRIMARY KEY CLUSTERED 
(
	[CALENDAR_NAME] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_FIRED_TRIGGERS](
	[ENTRY_ID] [varchar](95) NOT NULL,
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[IS_VOLATILE] [varchar](1) NOT NULL,
	[INSTANCE_NAME] [varchar](200) NOT NULL,
	[FIRED_TIME] [bigint] NOT NULL,
	[PRIORITY] [int] NOT NULL,
	[STATE] [varchar](16) NOT NULL,
	[JOB_NAME] [varchar](200) NULL,
	[JOB_GROUP] [varchar](200) NULL,
	[IS_STATEFUL] [varchar](1) NULL,
	[REQUESTS_RECOVERY] [varchar](1) NULL,
 CONSTRAINT [PK_QRTZ_FIRED_TRIGGERS] PRIMARY KEY CLUSTERED 
(
	[ENTRY_ID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_PAUSED_TRIGGER_GRPS](
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
 CONSTRAINT [PK_QRTZ_PAUSED_TRIGGER_GRPS] PRIMARY KEY CLUSTERED 
(
	[TRIGGER_GROUP] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_SCHEDULER_STATE](
	[INSTANCE_NAME] [varchar](200) NOT NULL,
	[LAST_CHECKIN_TIME] [bigint] NOT NULL,
	[CHECKIN_INTERVAL] [bigint] NOT NULL,
 CONSTRAINT [PK_QRTZ_SCHEDULER_STATE] PRIMARY KEY CLUSTERED 
(
	[INSTANCE_NAME] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_LOCKS](
	[LOCK_NAME] [varchar](40) NOT NULL,
 CONSTRAINT [PK_QRTZ_LOCKS] PRIMARY KEY CLUSTERED 
(
	[LOCK_NAME] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_JOB_DETAILS](
	[JOB_NAME] [varchar](200) NOT NULL,
	[JOB_GROUP] [varchar](200) NOT NULL,
	[DESCRIPTION] [varchar](250) NULL,
	[JOB_CLASS_NAME] [varchar](250) NOT NULL,
	[IS_DURABLE] [varchar](1) NOT NULL,
	[IS_VOLATILE] [varchar](1) NOT NULL,
	[IS_STATEFUL] [varchar](1) NOT NULL,
	[REQUESTS_RECOVERY] [varchar](1) NOT NULL,
	[JOB_DATA] [image] NULL,
 CONSTRAINT [PK_QRTZ_JOB_DETAILS] PRIMARY KEY CLUSTERED 
(
	[JOB_NAME] ASC,
	[JOB_GROUP] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[RelatedItem](
	[ItemId] [uniqueidentifier] NOT NULL,
	[ThreadId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_RelatedItem] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_BLOB_TRIGGERS](
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[BLOB_DATA] [image] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[TagClassifier](
	[Id] [varchar](512) NOT NULL,
	[RiffId] [int] NOT NULL,
	[TagId] [int] NOT NULL,
 CONSTRAINT [PK_TagClassifier] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[PerceptronTraining](
	[PerceptronName] [varchar](450) NOT NULL,
	[DocumentId] [varchar](450) NOT NULL,
	[ExpectedValue] [float] NOT NULL,
	[Alpha] [float] NOT NULL,
 CONSTRAINT [PK_PerceptronTraining_1] PRIMARY KEY CLUSTERED 
(
	[PerceptronName] ASC,
	[DocumentId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Updates](
	[Updated] [datetime] NOT NULL CONSTRAINT [Updates_when]  DEFAULT (getdate()),
	[module] [nvarchar](max) NULL,
	[Script] [nvarchar](max) NOT NULL,
	[Comment] [nvarchar](max) NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Social](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[SocialName] [varchar](50) NOT NULL,
	[Title] [varchar](50) NULL,
	[Description] [varchar](2048) NULL,
	[IsPrivate] [bit] NOT NULL CONSTRAINT [DF_Social_IsPrivate]  DEFAULT ((1)),
	[CreationDate] [datetime] NOT NULL CONSTRAINT [DF_Social_CreationDate]  DEFAULT (''),
	[Logo] [image] NULL,
	[LogoUrl] [varchar](255) NULL,
	[CustomSidebar] [text] NULL,
	[AuthenticatedUserRole] [varchar](50) NULL,
	[Activity] [int] NOT NULL CONSTRAINT [DF_Social_Activity]  DEFAULT ((0)),
	[LastModified] [datetime] NOT NULL CONSTRAINT [DF_Social_LastModified]  DEFAULT (getdate()),
	[ItemCount] [int] NULL,
	[NewItemsInLastWeek] [int] NULL,
 CONSTRAINT [PK_Social] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
CREATE UNIQUE NONCLUSTERED INDEX [IX_Social] ON [dbo].[Social] 
(
	[SocialName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_Social_Activity] ON [dbo].[Social] 
(
	[Activity] DESC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Membership](
	[UserID] [uniqueidentifier] NOT NULL,
	[SocialID] [int] NOT NULL,
	[Role] [varchar](50) NOT NULL,
 CONSTRAINT [PK_Membership] PRIMARY KEY CLUSTERED 
(
	[UserID] ASC,
	[SocialID] ASC,
	[Role] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[FeedLog](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[FeedId] [uniqueidentifier] NOT NULL,
	[Date] [datetime] NOT NULL,
	[State] [smallint] NOT NULL,
	[Message] [ntext] NULL,
	[ImportedItems] [int] NOT NULL,
	[UpdatedItems] [int] NOT NULL CONSTRAINT [DF_FeedLog_UpdatedItems]  DEFAULT ((0)),
 CONSTRAINT [PK_FeedLog] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[Tag](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[Name] [varchar](250) NOT NULL,
 CONSTRAINT [PK_dbo.Tag] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_Tag] ON [dbo].[Tag] 
(
	[Name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Module](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[ModuleQualifiedType] [varchar](150) NOT NULL,
	[ModuleName] [varchar](50) NOT NULL,
	[Description] [varchar](2048) NULL,
 CONSTRAINT [PK_Module] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Metadata](
	[Id] [uniqueidentifier] NOT NULL,
	[MetadataType] [varchar](50) NOT NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[ModificationDate] [datetime] NOT NULL,
	[Data] [image] NULL,
	[Filterable] [int] NULL,
	[Searchable] [text] NULL,
 CONSTRAINT [PK_Metadata] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[Ocurrence](
	[DocumentId] [varchar](50) NOT NULL,
	[TermId] [int] NOT NULL,
	[Frequency] [float] NOT NULL,
 CONSTRAINT [PK_dbo.Ocurrence] PRIMARY KEY CLUSTERED 
(
	[DocumentId] ASC,
	[TermId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SpaceDocument](
	[Space] [varchar](50) NOT NULL,
	[DocumentId] [varchar](50) NOT NULL,
 CONSTRAINT [PK_SpaceDocuments] PRIMARY KEY CLUSTERED 
(
	[Space] ASC,
	[DocumentId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[NotificationDigest](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
	[IsCreation] [bit] NOT NULL,
	[Message] [text] NULL,
	[Timestamp] [datetime] NOT NULL,
	[FilterSubscriptionId] [int] NOT NULL,
	[MessageParams] [varchar](255) NULL,
 CONSTRAINT [PK_NotificationDigest] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[HypothesisItem](
	[HypothesisId] [int] NOT NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_HypothesisItem] PRIMARY KEY CLUSTERED 
(
	[HypothesisId] ASC,
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_JOB_LISTENERS](
	[JOB_NAME] [varchar](200) NOT NULL,
	[JOB_GROUP] [varchar](200) NOT NULL,
	[JOB_LISTENER] [varchar](200) NOT NULL,
 CONSTRAINT [PK_QRTZ_JOB_LISTENERS] PRIMARY KEY CLUSTERED 
(
	[JOB_NAME] ASC,
	[JOB_GROUP] ASC,
	[JOB_LISTENER] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_TRIGGERS](
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[JOB_NAME] [varchar](200) NOT NULL,
	[JOB_GROUP] [varchar](200) NOT NULL,
	[IS_VOLATILE] [varchar](1) NOT NULL,
	[DESCRIPTION] [varchar](250) NULL,
	[NEXT_FIRE_TIME] [bigint] NULL,
	[PREV_FIRE_TIME] [bigint] NULL,
	[PRIORITY] [int] NULL,
	[TRIGGER_STATE] [varchar](16) NOT NULL,
	[TRIGGER_TYPE] [varchar](8) NOT NULL,
	[START_TIME] [bigint] NOT NULL,
	[END_TIME] [bigint] NULL,
	[CALENDAR_NAME] [varchar](200) NULL,
	[MISFIRE_INSTR] [smallint] NULL,
	[JOB_DATA] [image] NULL,
 CONSTRAINT [PK_QRTZ_TRIGGERS] PRIMARY KEY CLUSTERED 
(
	[TRIGGER_NAME] ASC,
	[TRIGGER_GROUP] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_TRIGGER_LISTENERS](
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[TRIGGER_LISTENER] [varchar](200) NOT NULL,
 CONSTRAINT [PK_QRTZ_TRIGGER_LISTENERS] PRIMARY KEY CLUSTERED 
(
	[TRIGGER_NAME] ASC,
	[TRIGGER_GROUP] ASC,
	[TRIGGER_LISTENER] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_SIMPLE_TRIGGERS](
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[REPEAT_COUNT] [bigint] NOT NULL,
	[REPEAT_INTERVAL] [bigint] NOT NULL,
	[TIMES_TRIGGERED] [bigint] NOT NULL,
 CONSTRAINT [PK_QRTZ_SIMPLE_TRIGGERS] PRIMARY KEY CLUSTERED 
(
	[TRIGGER_NAME] ASC,
	[TRIGGER_GROUP] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[QRTZ_CRON_TRIGGERS](
	[TRIGGER_NAME] [varchar](200) NOT NULL,
	[TRIGGER_GROUP] [varchar](200) NOT NULL,
	[CRON_EXPRESSION] [varchar](120) NOT NULL,
	[TIME_ZONE_ID] [varchar](80) NULL,
 CONSTRAINT [PK_QRTZ_CRON_TRIGGERS] PRIMARY KEY CLUSTERED 
(
	[TRIGGER_NAME] ASC,
	[TRIGGER_GROUP] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[UserBasketPermission](
	[BasketId] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[SourcePermission] [smallint] NOT NULL CONSTRAINT [DF_UserBasketPermission_SourcePermission]  DEFAULT ((0)),
	[CanInsert] [bit] NOT NULL CONSTRAINT [DF_UserBasketPermission_CanInsert]  DEFAULT ((0)),
 CONSTRAINT [PK_UserBasketPermission] PRIMARY KEY CLUSTERED 
(
	[BasketId] ASC,
	[UserId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Item](
	[Id] [uniqueidentifier] ROWGUIDCOL  NOT NULL CONSTRAINT [DF_Item_Id]  DEFAULT (newid()),
	[FeedId] [uniqueidentifier] NULL,
	[ItemId] [varchar](767) NOT NULL,
	[PublishDate] [datetime] NOT NULL,
	[LastUpdatedTime] [datetime] NOT NULL,
	[BaseUri] [varchar](255) NULL,
	[ContentSubclass] [varchar](50) NULL,
	[ContentType] [varchar](50) NULL,
	[Content] [ntext] NULL,
	[TitleType] [varchar](50) NULL,
	[Title] [nvarchar](1024) NULL,
	[SummaryType] [varchar](50) NULL,
	[Summary] [ntext] NULL,
	[CopyrightType] [varchar](50) NULL,
	[Copyright] [ntext] NULL,
	[ItemXml] [ntext] NULL,
	[SocialId] [int] NOT NULL,
	[OriginalLinkUri] [varchar](2048) NULL,
	[ShortSummary] [ntext] NULL,
	[IsInTrashCan] [bit] NOT NULL CONSTRAINT [DF_Item_IsInTrashCan]  DEFAULT ((0)),
	[CreatorId] [uniqueidentifier] NULL,
 CONSTRAINT [PK_Item] PRIMARY KEY NONCLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
CREATE CLUSTERED INDEX [IX_Item_SocialId_IsInTrashCan_PublishDate] ON [dbo].[Item] 
(
	[SocialId] ASC,
	[IsInTrashCan] ASC,
	[PublishDate] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_Item_SocialId_IsInTrashCan_Id] ON [dbo].[Item] 
(
	[SocialId] ASC,
	[IsInTrashCan] ASC,
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_Item_FeedId_ItemId] ON [dbo].[Item] 
(
	[FeedId] ASC,
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[UserProfile](
	[UserId] [uniqueidentifier] NOT NULL,
	[Name] [varchar](255) NOT NULL,
	[Email] [varchar](255) NOT NULL,
	[Expertise] [varchar](255) NOT NULL,
	[PhoneNumber] [varchar](255) NOT NULL,
	[Job] [varchar](255) NOT NULL,
	[Language] [varchar](30) NULL,
 CONSTRAINT [PK_UserProfile] PRIMARY KEY CLUSTERED 
(
	[UserId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[ItemBasket](
	[ItemId] [uniqueidentifier] NOT NULL,
	[BasketId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_ItemBasket_1] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[FeedBasket](
	[FeedId] [uniqueidentifier] NOT NULL,
	[BasketId] [uniqueidentifier] NULL,
 CONSTRAINT [PK_FeedBasket] PRIMARY KEY CLUSTERED 
(
	[FeedId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[FilterSubscription](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[FilterId] [int] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[Creation] [bit] NOT NULL,
	[Update] [bit] NOT NULL,
	[Frequency] [smallint] NOT NULL CONSTRAINT [DF_FilterSubscription_Frequency]  DEFAULT ((0)),
	[IsOnlyLink] [bit] NOT NULL CONSTRAINT [DF__FilterSub__IsOnl__16A44564]  DEFAULT ((0)),
 CONSTRAINT [PK_FilterSubscription] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[ScoreConfiguration](
	[MinValue] [int] NOT NULL,
	[MaxValue] [int] NOT NULL,
	[SocialId] [int] NOT NULL CONSTRAINT [DF_ScoreConfiguration_ModuleId]  DEFAULT ((0)),
	[Name] [text] NULL CONSTRAINT [DF_ScoreConfiguration_Name]  DEFAULT (''),
 CONSTRAINT [PK_ScoreConfiguration] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[HeatmapConfiguration](
	[SocialId] [int] NOT NULL,
	[UseScore] [bit] NOT NULL,
	[DecreaseByDate] [bit] NOT NULL,
 CONSTRAINT [PK_HeatmapConfiguration] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[AnonymousContributor](
	[PhoneNumber] [varchar](100) NOT NULL,
	[SocialId] [int] NOT NULL,
	[Latitude] [float] NULL,
	[Longitude] [float] NULL
) ON [PRIMARY]
SET ANSI_PADDING OFF
ALTER TABLE [dbo].[AnonymousContributor] ADD [LocationName] [varchar](100) NULL
ALTER TABLE [dbo].[AnonymousContributor] ADD [WorkingGroupId] [int] NULL
SET ANSI_PADDING ON
ALTER TABLE [dbo].[AnonymousContributor] ADD [Name] [varchar](100) NULL
ALTER TABLE [dbo].[AnonymousContributor] ADD [FirstItemId] [uniqueidentifier] NULL
ALTER TABLE [dbo].[AnonymousContributor] ADD  CONSTRAINT [PK_AnonymousContributor] PRIMARY KEY CLUSTERED 
(
	[PhoneNumber] ASC,
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[AlertConfiguration](
	[SocialId] [int] NOT NULL,
	[Label] [text] NOT NULL,
 CONSTRAINT [PK_AlertConfiguration] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[Basket](
	[Id] [uniqueidentifier] NOT NULL,
	[Name] [varchar](100) NOT NULL,
	[SocialId] [int] NOT NULL,
 CONSTRAINT [PK_Basket] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Filter](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[SocialId] [int] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[QueryString] [text] NOT NULL,
	[Name] [varchar](50) NULL,
 CONSTRAINT [PK_Filter] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_Filter_SocialId] ON [dbo].[Filter] 
(
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SmsConfiguration](
	[SocialId] [int] NOT NULL,
	[Url] [varchar](255) NOT NULL,
	[WhatToSend] [int] NOT NULL,
	[Username] [varchar](50) NULL,
	[Password] [varchar](50) NULL,
	[DefaultLanguage] [varchar](50) NULL,
 CONSTRAINT [PK_SmsConfiguration] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Feed](
	[Id] [uniqueidentifier] ROWGUIDCOL  NOT NULL CONSTRAINT [DF_Feed_Id]  DEFAULT (newid()),
	[Title] [nvarchar](255) NOT NULL,
	[Url] [varchar](2048) NOT NULL,
	[SocialId] [int] NOT NULL,
	[LastChecked] [datetime] NULL,
	[LastModified] [datetime] NULL,
	[ETag] [varchar](255) NULL,
	[Active] [bit] NOT NULL,
	[UpdateInterval] [int] NOT NULL CONSTRAINT [DF_Feed_UpdateInterval]  DEFAULT ((60)),
	[IsDeleted] [bit] NOT NULL CONSTRAINT [DF_Item_IsDeleted]  DEFAULT ((0)),
	[Transformation] [ntext] NULL,
 CONSTRAINT [PK_Feed] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[SocialModule](
	[SocialId] [int] NOT NULL,
	[ModuleId] [int] NOT NULL,
 CONSTRAINT [PK_SocialModules] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC,
	[ModuleId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[TaggingPhraseMapping](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[SocialId] [int] NOT NULL,
	[FromPhrase] [nvarchar](max) NOT NULL,
	[ToPhrase] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK_TaggingPhraseMapping] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[ModuleConfiguration](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[SocialId] [int] NOT NULL,
	[ModuleName] [varchar](50) NOT NULL,
	[Configuration] [xml] NULL,
 CONSTRAINT [PK_ModuleConfiguration] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[WorkingGroup](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[Name] [varchar](50) NOT NULL,
	[SocialId] [int] NOT NULL,
	[DefaultBasketId] [uniqueidentifier] NULL,
 CONSTRAINT [PK_WorkingGroup] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [Idx_WorkingGroup_SocialId_Name] ON [dbo].[WorkingGroup] 
(
	[SocialId] ASC,
	[Name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[SocialTag](
	[SocialId] [int] NOT NULL,
	[TagId] [int] NOT NULL,
	[TagCount] [int] NOT NULL,
	[TagName] [varchar](250) NOT NULL DEFAULT (''),
 CONSTRAINT [PK_SocialTag] PRIMARY KEY CLUSTERED 
(
	[SocialId] ASC,
	[TagId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Invite](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[Email] [varchar](max) NOT NULL,
	[SocialId] [int] NOT NULL,
	[Role] [varchar](50) NOT NULL,
 CONSTRAINT [PK_Invite] PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[DeletedItem](
	[ItemId] [varchar](255) NOT NULL,
	[FeedId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_DeletedItem] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC,
	[FeedId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[ItemTag](
	[ItemId] [uniqueidentifier] NOT NULL,
	[TagId] [int] NOT NULL,
	[UserId] [uniqueidentifier] NULL,
	[Timestamp] [timestamp] NOT NULL,
	[SocialId] [int] NULL,
	[ItemPublishDate] [datetime] NULL,
 CONSTRAINT [PK_ItemTag] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC,
	[TagId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_ItemTag] ON [dbo].[ItemTag] 
(
	[SocialId] ASC,
	[TagId] ASC,
	[ItemPublishDate] DESC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[ItemSmsAlertField](
	[ItemId] [uniqueidentifier] NOT NULL,
	[SmsText] [varchar](140) NOT NULL,
 CONSTRAINT [PK_ItemSmsAlertField] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Score](
	[ItemId] [uniqueidentifier] NOT NULL,
	[Value] [int] NOT NULL,
 CONSTRAINT [PK_Score] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Comment](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NULL,
	[Text] [ntext] NOT NULL,
	[DateTime] [datetime] NOT NULL,
	[ParentCommentId] [int] NULL,
 CONSTRAINT [PK_Comment] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [Idx_Comment_ItemId] ON [dbo].[Comment] 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Favorite](
	[Owner] [uniqueidentifier] NOT NULL,
	[Item] [uniqueidentifier] NOT NULL,
	[Timestamp] [timestamp] NOT NULL,
	[CreationDate] [datetime] NOT NULL CONSTRAINT [DF_Favorite_CreationDate]  DEFAULT (getdate()),
 CONSTRAINT [PK_Favorite] PRIMARY KEY CLUSTERED 
(
	[Owner] ASC,
	[Item] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Annotation](
	[ItemId] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[Id] [uniqueidentifier] NOT NULL,
	[Text] [ntext] NULL,
	[LastModified] [datetime] NOT NULL,
 CONSTRAINT [PK_Annotation] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[ReadItem](
	[ItemId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_ReadItem] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[Alert](
	[Owner] [uniqueidentifier] NOT NULL,
	[Item] [uniqueidentifier] NOT NULL,
	[Timestamp] [timestamp] NOT NULL,
	[CreationDate] [datetime] NOT NULL CONSTRAINT [DF_Alert_CreationDate]  DEFAULT (getdate()),
 CONSTRAINT [PK_Alert] PRIMARY KEY CLUSTERED 
(
	[Owner] ASC,
	[Item] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[GeoLocation](
	[Latitude] [float] NULL,
	[Longitude] [float] NULL,
	[Description] [varchar](255) NULL,
	[ItemId] [uniqueidentifier] NOT NULL,
	[Id] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
	[Term] [nvarchar](255) NULL,
 CONSTRAINT [PK_GeoLocation] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [Idx_GeoLocation_ItemId] ON [dbo].[GeoLocation] 
(
	[ItemId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO

CREATE TABLE [dbo].[HiddenItem](
	[ItemId] [uniqueidentifier] NOT NULL,
	[UserId] [uniqueidentifier] NOT NULL,
 CONSTRAINT [PK_HiddenItem] PRIMARY KEY CLUSTERED 
(
	[ItemId] ASC,
	[UserId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE VIEW [dbo].[ItemInSocial]
AS
SELECT     dbo.Item.*, dbo.Social.SocialName
FROM         dbo.Feed INNER JOIN
                      dbo.Item ON dbo.Feed.Id = dbo.Item.FeedId INNER JOIN
                      dbo.Social ON dbo.Feed.SocialId = dbo.Social.Id
GO

CREATE VIEW [dbo].[MetadataInSocial]
AS
SELECT     dbo.Social.Id AS SocialId, dbo.Social.IsPrivate, dbo.Metadata.Id AS MetadataId
FROM         dbo.Feed INNER JOIN
                      dbo.Item ON dbo.Feed.Id = dbo.Item.FeedId INNER JOIN
                      dbo.Social ON dbo.Feed.SocialId = dbo.Social.Id INNER JOIN
                      dbo.Metadata ON dbo.Item.Id = dbo.Metadata.ItemId
GO

CREATE VIEW [dbo].[TagStats]
AS
SELECT     TOP (100) PERCENT dbo.Tag.Name, COUNT(dbo.ItemTag.ItemId) AS q, dbo.Feed.Title
FROM         dbo.Tag INNER JOIN
                      dbo.ItemTag ON dbo.Tag.Id = dbo.ItemTag.TagId INNER JOIN
                      dbo.Item ON dbo.ItemTag.ItemId = dbo.Item.Id INNER JOIN
                      dbo.Feed ON dbo.Item.FeedId = dbo.Feed.Id
GROUP BY dbo.Tag.Name, dbo.Feed.Title
ORDER BY q DESC
GO

ALTER TABLE [dbo].[Alert]  WITH CHECK ADD  CONSTRAINT [FK_Alert_Item] FOREIGN KEY([Item])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Alert] CHECK CONSTRAINT [FK_Alert_Item]
GO

ALTER TABLE [dbo].[AlertConfiguration]  WITH CHECK ADD  CONSTRAINT [FK_AlertConfiguration_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[AlertConfiguration] CHECK CONSTRAINT [FK_AlertConfiguration_Social]
GO

ALTER TABLE [dbo].[Annotation]  WITH CHECK ADD  CONSTRAINT [FK_Annotation_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Annotation] CHECK CONSTRAINT [FK_Annotation_Item]
GO

ALTER TABLE [dbo].[AnonymousContributor]  WITH CHECK ADD  CONSTRAINT [FK_AnonymousContributor_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[AnonymousContributor] CHECK CONSTRAINT [FK_AnonymousContributor_Social]
GO

ALTER TABLE [dbo].[AnonymousContributor]  WITH CHECK ADD  CONSTRAINT [FK_AnonymousContributor_WorkingGroup] FOREIGN KEY([WorkingGroupId])
REFERENCES [dbo].[WorkingGroup] ([Id])
GO
ALTER TABLE [dbo].[AnonymousContributor] CHECK CONSTRAINT [FK_AnonymousContributor_WorkingGroup]
GO

ALTER TABLE [dbo].[Basket]  WITH CHECK ADD  CONSTRAINT [FK_Basket_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Basket] CHECK CONSTRAINT [FK_Basket_Social]
GO

ALTER TABLE [dbo].[Comment]  WITH CHECK ADD  CONSTRAINT [FK_Comment_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Comment] CHECK CONSTRAINT [FK_Comment_Item]
GO

ALTER TABLE [dbo].[DeletedItem]  WITH CHECK ADD  CONSTRAINT [FK_DeletedItem_Feed] FOREIGN KEY([FeedId])
REFERENCES [dbo].[Feed] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[DeletedItem] CHECK CONSTRAINT [FK_DeletedItem_Feed]
GO

ALTER TABLE [dbo].[Favorite]  WITH CHECK ADD  CONSTRAINT [FK_Favorite_Item] FOREIGN KEY([Item])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Favorite] CHECK CONSTRAINT [FK_Favorite_Item]
GO

ALTER TABLE [dbo].[Feed]  WITH CHECK ADD  CONSTRAINT [FK_Feed_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Feed] CHECK CONSTRAINT [FK_Feed_Social]
GO

ALTER TABLE [dbo].[FeedBasket]  WITH CHECK ADD  CONSTRAINT [FK_FeedBasket_Basket] FOREIGN KEY([BasketId])
REFERENCES [dbo].[Basket] ([Id])
GO
ALTER TABLE [dbo].[FeedBasket] CHECK CONSTRAINT [FK_FeedBasket_Basket]
GO

ALTER TABLE [dbo].[FeedBasket]  WITH CHECK ADD  CONSTRAINT [FK_FeedBasket_Feed1] FOREIGN KEY([FeedId])
REFERENCES [dbo].[Feed] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[FeedBasket] CHECK CONSTRAINT [FK_FeedBasket_Feed1]
GO

ALTER TABLE [dbo].[Filter]  WITH CHECK ADD  CONSTRAINT [FK_Filter_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Filter] CHECK CONSTRAINT [FK_Filter_Social]
GO

ALTER TABLE [dbo].[FilterSubscription]  WITH CHECK ADD  CONSTRAINT [FK_FilterSubscription_Filter] FOREIGN KEY([FilterId])
REFERENCES [dbo].[Filter] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[FilterSubscription] CHECK CONSTRAINT [FK_FilterSubscription_Filter]
GO

ALTER TABLE [dbo].[GeoLocation]  WITH CHECK ADD  CONSTRAINT [FK_GeoLocation_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[GeoLocation] CHECK CONSTRAINT [FK_GeoLocation_Item]
GO

ALTER TABLE [dbo].[HeatmapConfiguration]  WITH CHECK ADD  CONSTRAINT [FK_HeatmapConfiguration_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[HeatmapConfiguration] CHECK CONSTRAINT [FK_HeatmapConfiguration_Social]
GO

ALTER TABLE [dbo].[HiddenItem]  WITH CHECK ADD  CONSTRAINT [FK_HiddenItem_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[HiddenItem] CHECK CONSTRAINT [FK_HiddenItem_Item]
GO

ALTER TABLE [dbo].[HypothesisItem]  WITH CHECK ADD  CONSTRAINT [FK_HypothesisItem_Hypothesis] FOREIGN KEY([HypothesisId])
REFERENCES [dbo].[Hypothesis] ([Id])
GO
ALTER TABLE [dbo].[HypothesisItem] CHECK CONSTRAINT [FK_HypothesisItem_Hypothesis]
GO

ALTER TABLE [dbo].[HypothesisItem]  WITH CHECK ADD  CONSTRAINT [FK_HypothesisItem_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[HypothesisItem] CHECK CONSTRAINT [FK_HypothesisItem_Item]
GO

ALTER TABLE [dbo].[Invite]  WITH CHECK ADD  CONSTRAINT [FK_Invite_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Invite] CHECK CONSTRAINT [FK_Invite_Social]
GO

ALTER TABLE [dbo].[Item]  WITH CHECK ADD  CONSTRAINT [FK_Item_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Item] CHECK CONSTRAINT [FK_Item_Social]
GO

ALTER TABLE [dbo].[Item]  WITH CHECK ADD  CONSTRAINT [FK_Item_User] FOREIGN KEY([CreatorId])
REFERENCES [dbo].[aspnet_Users] ([UserId])
GO
ALTER TABLE [dbo].[Item] CHECK CONSTRAINT [FK_Item_User]
GO

ALTER TABLE [dbo].[ItemBasket]  WITH CHECK ADD  CONSTRAINT [FK_ItemBasket_Basket] FOREIGN KEY([BasketId])
REFERENCES [dbo].[Basket] ([Id])
GO
ALTER TABLE [dbo].[ItemBasket] CHECK CONSTRAINT [FK_ItemBasket_Basket]
GO

ALTER TABLE [dbo].[ItemBasket]  WITH CHECK ADD  CONSTRAINT [FK_ItemBasket_ItemBasket] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ItemBasket] CHECK CONSTRAINT [FK_ItemBasket_ItemBasket]
GO

ALTER TABLE [dbo].[ItemLanguage]  WITH CHECK ADD  CONSTRAINT [FK_ItemLanguage_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ItemLanguage] CHECK CONSTRAINT [FK_ItemLanguage_Item]
GO

ALTER TABLE [dbo].[ItemSmsAlertField]  WITH CHECK ADD  CONSTRAINT [FK_ItemSmsAlertField_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ItemSmsAlertField] CHECK CONSTRAINT [FK_ItemSmsAlertField_Item]
GO

ALTER TABLE [dbo].[ItemStatus]  WITH CHECK ADD  CONSTRAINT [FK_ItemStatus_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ItemStatus] CHECK CONSTRAINT [FK_ItemStatus_Item]
GO

ALTER TABLE [dbo].[ItemStatusChange]  WITH CHECK ADD  CONSTRAINT [FK_ItemStatusChange_aspnet_Users] FOREIGN KEY([UserId])
REFERENCES [dbo].[aspnet_Users] ([UserId])
GO
ALTER TABLE [dbo].[ItemStatusChange] CHECK CONSTRAINT [FK_ItemStatusChange_aspnet_Users]
GO

ALTER TABLE [dbo].[ItemStatusChange]  WITH CHECK ADD  CONSTRAINT [FK_ItemStatusChange_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ItemStatusChange] CHECK CONSTRAINT [FK_ItemStatusChange_Item]
GO

ALTER TABLE [dbo].[ItemTag]  WITH CHECK ADD  CONSTRAINT [Tag_ItemTag] FOREIGN KEY([TagId])
REFERENCES [dbo].[Tag] ([Id])
GO
ALTER TABLE [dbo].[ItemTag] CHECK CONSTRAINT [Tag_ItemTag]
GO

ALTER TABLE [dbo].[ModuleConfiguration]  WITH CHECK ADD  CONSTRAINT [FK_ModuleConfiguration_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ModuleConfiguration] CHECK CONSTRAINT [FK_ModuleConfiguration_Social]
GO

ALTER TABLE [dbo].[NotificationDigest]  WITH CHECK ADD  CONSTRAINT [FK_NotificationDigest_FilterSubscription] FOREIGN KEY([FilterSubscriptionId])
REFERENCES [dbo].[FilterSubscription] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[NotificationDigest] CHECK CONSTRAINT [FK_NotificationDigest_FilterSubscription]
GO

ALTER TABLE [dbo].[NotificationDigest]  WITH CHECK ADD  CONSTRAINT [FK_NotificationDigest_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
GO
ALTER TABLE [dbo].[NotificationDigest] CHECK CONSTRAINT [FK_NotificationDigest_Item]
GO

ALTER TABLE [dbo].[QRTZ_CRON_TRIGGERS]  WITH CHECK ADD  CONSTRAINT [FK_QRTZ_CRON_TRIGGERS_QRTZ_TRIGGERS] FOREIGN KEY([TRIGGER_NAME], [TRIGGER_GROUP])
REFERENCES [dbo].[QRTZ_TRIGGERS] ([TRIGGER_NAME], [TRIGGER_GROUP])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[QRTZ_CRON_TRIGGERS] CHECK CONSTRAINT [FK_QRTZ_CRON_TRIGGERS_QRTZ_TRIGGERS]
GO

ALTER TABLE [dbo].[QRTZ_JOB_LISTENERS]  WITH CHECK ADD  CONSTRAINT [FK_QRTZ_JOB_LISTENERS_QRTZ_JOB_DETAILS] FOREIGN KEY([JOB_NAME], [JOB_GROUP])
REFERENCES [dbo].[QRTZ_JOB_DETAILS] ([JOB_NAME], [JOB_GROUP])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[QRTZ_JOB_LISTENERS] CHECK CONSTRAINT [FK_QRTZ_JOB_LISTENERS_QRTZ_JOB_DETAILS]
GO

ALTER TABLE [dbo].[QRTZ_SIMPLE_TRIGGERS]  WITH CHECK ADD  CONSTRAINT [FK_QRTZ_SIMPLE_TRIGGERS_QRTZ_TRIGGERS] FOREIGN KEY([TRIGGER_NAME], [TRIGGER_GROUP])
REFERENCES [dbo].[QRTZ_TRIGGERS] ([TRIGGER_NAME], [TRIGGER_GROUP])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[QRTZ_SIMPLE_TRIGGERS] CHECK CONSTRAINT [FK_QRTZ_SIMPLE_TRIGGERS_QRTZ_TRIGGERS]
GO

ALTER TABLE [dbo].[QRTZ_TRIGGER_LISTENERS]  WITH CHECK ADD  CONSTRAINT [FK_QRTZ_TRIGGER_LISTENERS_QRTZ_TRIGGERS] FOREIGN KEY([TRIGGER_NAME], [TRIGGER_GROUP])
REFERENCES [dbo].[QRTZ_TRIGGERS] ([TRIGGER_NAME], [TRIGGER_GROUP])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[QRTZ_TRIGGER_LISTENERS] CHECK CONSTRAINT [FK_QRTZ_TRIGGER_LISTENERS_QRTZ_TRIGGERS]
GO

ALTER TABLE [dbo].[QRTZ_TRIGGERS]  WITH CHECK ADD  CONSTRAINT [FK_QRTZ_TRIGGERS_QRTZ_JOB_DETAILS] FOREIGN KEY([JOB_NAME], [JOB_GROUP])
REFERENCES [dbo].[QRTZ_JOB_DETAILS] ([JOB_NAME], [JOB_GROUP])
GO
ALTER TABLE [dbo].[QRTZ_TRIGGERS] CHECK CONSTRAINT [FK_QRTZ_TRIGGERS_QRTZ_JOB_DETAILS]
GO

ALTER TABLE [dbo].[ReadItem]  WITH CHECK ADD  CONSTRAINT [FK_ReadItem_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ReadItem] CHECK CONSTRAINT [FK_ReadItem_Item]
GO

ALTER TABLE [dbo].[Score]  WITH CHECK ADD  CONSTRAINT [FK_Score_Item] FOREIGN KEY([ItemId])
REFERENCES [dbo].[Item] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Score] CHECK CONSTRAINT [FK_Score_Item]
GO

ALTER TABLE [dbo].[ScoreConfiguration]  WITH CHECK ADD  CONSTRAINT [FK_ScoreConfiguration_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[ScoreConfiguration] CHECK CONSTRAINT [FK_ScoreConfiguration_Social]
GO

ALTER TABLE [dbo].[SmsConfiguration]  WITH CHECK ADD  CONSTRAINT [FK_SmsConfiguration_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[SmsConfiguration] CHECK CONSTRAINT [FK_SmsConfiguration_Social]
GO

ALTER TABLE [dbo].[SocialModule]  WITH CHECK ADD  CONSTRAINT [FK_SocialModules_Module] FOREIGN KEY([ModuleId])
REFERENCES [dbo].[Module] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[SocialModule] CHECK CONSTRAINT [FK_SocialModules_Module]
GO

ALTER TABLE [dbo].[SocialModule]  WITH CHECK ADD  CONSTRAINT [FK_SocialModules_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[SocialModule] CHECK CONSTRAINT [FK_SocialModules_Social]
GO

ALTER TABLE [dbo].[SocialTag]  WITH CHECK ADD  CONSTRAINT [FK_SocialTag_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[SocialTag] CHECK CONSTRAINT [FK_SocialTag_Social]
GO

ALTER TABLE [dbo].[SocialTag]  WITH CHECK ADD  CONSTRAINT [FK_SocialTag_Tag] FOREIGN KEY([TagId])
REFERENCES [dbo].[Tag] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[SocialTag] CHECK CONSTRAINT [FK_SocialTag_Tag]
GO

ALTER TABLE [dbo].[TaggingPhraseMapping]  WITH CHECK ADD  CONSTRAINT [FK_TaggingPhraseMapping_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[TaggingPhraseMapping] CHECK CONSTRAINT [FK_TaggingPhraseMapping_Social]
GO

ALTER TABLE [dbo].[UserBasketPermission]  WITH CHECK ADD  CONSTRAINT [FK_UserBasketPermission_aspnet_Users] FOREIGN KEY([UserId])
REFERENCES [dbo].[aspnet_Users] ([UserId])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[UserBasketPermission] CHECK CONSTRAINT [FK_UserBasketPermission_aspnet_Users]
GO

ALTER TABLE [dbo].[UserBasketPermission]  WITH CHECK ADD  CONSTRAINT [FK_UserBasketPermission_Basket] FOREIGN KEY([BasketId])
REFERENCES [dbo].[Basket] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[UserBasketPermission] CHECK CONSTRAINT [FK_UserBasketPermission_Basket]
GO

ALTER TABLE [dbo].[UserProfile]  WITH CHECK ADD  CONSTRAINT [FK_UserProfile_aspnet_Users] FOREIGN KEY([UserId])
REFERENCES [dbo].[aspnet_Users] ([UserId])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[UserProfile] CHECK CONSTRAINT [FK_UserProfile_aspnet_Users]
GO

ALTER TABLE [dbo].[WorkingGroup]  WITH CHECK ADD  CONSTRAINT [FK_WorkingGroup_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[WorkingGroup] CHECK CONSTRAINT [FK_WorkingGroup_Social]
GO

ALTER TABLE [dbo].[ItemTag] ADD CONSTRAINT [FK_ItemTag_Social] FOREIGN KEY([SocialId])
REFERENCES [dbo].[Social] ([Id])
ON UPDATE NO ACTION 
ON DELETE CASCADE
GO

CREATE FULLTEXT INDEX ON [dbo].[Item](
[Content], 
[Summary], 
[Title])
KEY INDEX [PK_Item] ON [RiffFullText]
WITH CHANGE_TRACKING AUTO
GO

CREATE FULLTEXT INDEX ON [dbo].[Social](
[SocialName],
[Title],
[Description])
KEY INDEX [PK_Social] ON [RiffFullText]
WITH CHANGE_TRACKING AUTO
GO


CREATE FUNCTION [dbo].[SearchItemsFullText]
(   
    @content varchar(1000)
)
RETURNS TABLE
AS
RETURN
(
    SELECT *
    FROM Item
    WHERE CONTAINS(Title, @content) or CONTAINS([Content], @content) or CONTAINS([Summary], @content)
)
GO

CREATE FUNCTION [dbo].[SearchSocialsFullText]
(	
	@criteria VARCHAR(1000)
)
RETURNS TABLE 
AS
RETURN 
(
	SELECT *
	FROM Social
	WHERE CONTAINS(SocialName, @criteria)
	OR CONTAINS(Title, @criteria)
	OR CONTAINS(Description, @criteria)
)
GO
