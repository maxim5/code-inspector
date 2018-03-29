// ALGORITHM.cpp : Implementation of ALGORITHM

#include "stdafx.h"
#include "ALGORITHM.h"


// ALGORITHM
BEGIN_PARAMETER_DECLARATION(ALGORITHM)
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
