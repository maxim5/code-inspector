#include "Data.h"
#include "Parser.h"
#include "String.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

@implementation DataObject
-(char *)createCStr
{
	return NULL;
}

-(String *)createStr
{
	return nil;
}

-setFromBuffer: (const char *)buffer withLength: (int)len
{
	return self;
}
@end

@implementation DictEntry
-init
{
	self = [super init];
	key = NULL; data = NULL;
	return self;
}

-setKey: (String *)keyStr
{
	key = keyStr;
	return self;
}

-setData: (DataObject *)dataPtr
{
	data = dataPtr;
	return self;
}

-getKey
{
	return key;
}

-getData
{
	return data;
}
@end
