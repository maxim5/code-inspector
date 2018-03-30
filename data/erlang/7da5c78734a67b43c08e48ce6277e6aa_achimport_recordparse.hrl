%% Interface to the achimport_recordparse module

-record(recordParseState, 
	{
		fileHeader,
		batchHeader,
		detailEntry,
		addenda,
		batchControl,
		fileControl,
		detailEntryMode,
		recordCount = 0,
		detailCountWithinBatch  = 0,
		addendaCountForDetailEntry = 0,
		batchCount = 0
	}).