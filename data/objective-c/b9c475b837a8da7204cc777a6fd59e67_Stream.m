#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Stream.h"

#define INITIAL_LEN		8
#define WRITE_SIZE		256

@interface Stream				/* Private methods */
- expand;
@end

@implementation Stream

/*
 * Read a file's contents into the stream. Only returns nil if the file
 * can't be opened. If there is an error during reading, still returns
 * self.
 */
- initFromFile: (char *)fname mode: (int)mode
{
	int c;
	char modeString[4];
	FILE *fp;

	[super init];

	fileName = malloc((size_t)(strlen(fname) + 1));
	strcpy(fileName, fname);

	modeString[0] = '\0';
	if (mode & STREAM_WRITE)
		isWritable = YES;
	else
		isWritable = NO;
	if (mode & STREAM_READ)
		strcat(modeString, "r");
	else if (mode & STREAM_WRITE)
		strcat(modeString, "w");
	if (mode & STREAM_BINARY) {
		isBinary = YES;
	}

	if ((fp = fopen(fname, modeString)) == NULL)
		return nil;

	fseek(fp, 0L, SEEK_SET);
	while ((c = fgetc(fp)) != EOF) {
		if ([self putc: c] == nil) {
			fclose(fp);
			return self;
		}
	}

	if (mode & STREAM_APPEND)
		[self seek: (StreamPos)0 from: StreamFromBottom];
	else
		[self seek: (StreamPos)0 from: StreamFromTop];

	fclose(fp);
	return self;
}

- free
{
	if (data) {
		[self flush];
	}
	if (fileName)
		free(fileName);
	return [super free];
}

/*
 * Private method that allocates more memory to a stream.
 */
- expand
{
	if (allocated == 0) {
		allocated = INITIAL_LEN;
		data = malloc((size_t)allocated);
	}
	else {
		allocated *= 2;
		data = realloc(data, (size_t)allocated);
	}

	if (data == NULL) {
		allocated = length = pos = 0;
		return nil;
	}
	return self;
}

/*
 * Flush the stream to its file.
 */
- flush
{
	StreamCount nBytes;
	char *modeString = (isBinary ? "wb" : "w");
	unsigned char *p;
	FILE *fp;

	if (data && isWritable && fileName) {
		if ((fp = fopen(fileName, modeString)) == NULL)
			return nil;

		nBytes = length;
		p = data;
		while (nBytes >= (StreamCount)WRITE_SIZE) {
			if (fwrite(p, (size_t)WRITE_SIZE, (size_t)1, fp) != WRITE_SIZE) {
				fclose(fp);
				return nil;
			}
			nBytes -= (StreamCount)WRITE_SIZE;
			p += WRITE_SIZE;
		}
		if (nBytes > 0 &&
			fwrite(p, (size_t)nBytes, (size_t)1, fp) != (int)nBytes) {

			fclose(fp);
			return nil;
		}
		fclose(fp);
	}
	length = pos = (StreamCount)0;
	pos = (StreamPos)0;
	isWritable = isBinary = NO;
	if (data) free(data);
	if (fileName) free(fileName);
	return self;
}

/*
 * Set the current position, relative to the top, bottom, or current position
 * of the stream.
 */
- seek: (StreamPos)pos from: (int)where
{
	StreamCount newPos;

	if (pos < 0)
		return nil;

	switch (where) {
	case StreamFromTop:
		if (pos >= length && pos != 0)
			return nil;
		pos = pos;
		break;
	case StreamFromBottom:
		if (pos >= length && pos != 0)
			return nil;
		pos = (length - 1) - pos;
		break;
	case StreamFromCurr:
		newPos = pos + pos;
		if (newPos < 0 || newPos >= length)
			return nil;
		pos = newPos;
		break;
	default:
		return nil;
	}
	return self;
}

/*
 * Return the current stream position.
 */
- (StreamPos)tell
{
	return pos;
}

/*
 * Get the next character from the stream.
 */
- (int)getc
{
	if (pos < 0 || pos >= length - 1)
		return EOF;

	return (int)data[pos++];
}

/*
 * Put a character to the stream.
 */
- putc: (int)ch
{
	if (pos >= allocated - 1)
		if ([self expand] == nil)
			[self error: "Ran out of memory trying to expand stream."];

	data[pos++] = (unsigned char)ch;
	if (pos >= length)
		length = pos + 1;
	return self;
}

/*
 * Unget a character from the stream.
 */
- ungetc
{
	if (pos < 0)
		return nil;

	--pos;
	if (pos >= length)
		length = pos + 1;
	return self;
}

/*
 * Return YES if at end of stream, else NO.
 */
- (BOOL)atEos
{
	return (pos >= length) ? YES : NO;
}

/*
 * Get a newline-terminated string. Do not return the newline.
 */
- gets: (char *)buf maxlen: (int)maxlen
{
	int ch;

	if (maxlen < 0)
		return nil;
	if (maxlen == 0)
		return self;

	while (maxlen-- && (ch = [self getc]) != EOF && ![self atEos]) {
		*buf++ = (char)ch;
		if (ch == '\n')
			break;
	}
	if (maxlen <= 0)
		--buf;
	*buf = '\0';
	return self;
}

/*
 * Append a string to the stream.
 */
- puts: (char *)buf
{
	while (*buf) {
		if ([self putc: *buf] == nil)
			return nil;
		++buf;
	}
	return self;
}

/*
 * Read "len" characters from the stream, and return the number
 * of characters actually read.
 */
- (StreamCount)read: (char *)buf len: (StreamCount)len
{
	StreamCount nRead;

	if (len < 0)
		return (StreamCount)0;

	for (nRead = 0; len--; ++nRead) {
		if ([self atEos])
				break;
		*buf++ = [self getc];
	}
	return nRead;
}

/*
 * Write "len" characters to the stream, and return the number
 * of characters actually written.
 */
- (StreamCount)write: (char *)buf len: (StreamCount) len
{
	StreamCount nWritten;

	if (len < 0)
		return (StreamCount)0;

	for (nWritten = 0; len--; ++nWritten) {
		if ([self putc: *buf++] == nil)
				break;
	}
	return nWritten;
}

@end

/*
;;; Local Variables: ***
;;; tab-width:4 ***
;;; End: ***
*/
