// ALGORITHM.cpp : Implementation of ALGORITHM

#include "stdafx.h"
#include "ALGORITHM.h"


// ALGORITHM
BEGIN_PARAMETER_DECLARATION(ALGORITHM)
	DECLARE_PARAMETER(L"MINIMUM_DEPENDENCY_SCORE",	// Name
				IDS_MINIMUM_DEPENDENCY_SCORE_DESCR,	// Res ID 
				DBTYPE_R4,		// Type, as a DBTYPEENUM
				false,			// Required flag
				true,			// Exposed flag
				0,				// General flags
				L"3",			// Default value, as a string
			  	L"(-inf,inf)")	// Enumeration, as a string
	DECLARE_PARAMETER(L"DISPLAY_CORRELATION",	// Name
				IDS_DISPLAY_CORRELATION_DESCR,	// Res ID 
				DBTYPE_BOOL,	// Type, as a DBTYPEENUM
				false,			// Required flag
				true,			// Exposed flag
				0,				// General flags
				L"FALSE",			// Default value, as a string
			  	L"TRUE or FALSE")	// Enumeration, as a string
END_PARAMETER_DECLARATION(ALGORITHM) 

STDMETHODIMP ALGORITHM::InterfaceSupportsErrorInfo(REFIID riid)
{
	static const IID* arr[] = 
	{
		&IID_IDMAlgorithm
	};

	for (int i=0; i < sizeof(arr) / sizeof(arr[0]); i++)
	{
		if (InlineIsEqualGUID(*arr[i],riid))
			return S_OK;
	}
	return S_FALSE;
}
