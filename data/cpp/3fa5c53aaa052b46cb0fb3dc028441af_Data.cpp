// Data.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

// Our data size is 128 8k pages = 1Mb
#define DATA_SIZE (8192*128)

#pragma section( ".shared", read, write, shared )
#pragma data_seg(".shared") 
extern "C"
{
	__declspec(dllexport) char data[DATA_SIZE] = {0};
}
#pragma data_seg()


