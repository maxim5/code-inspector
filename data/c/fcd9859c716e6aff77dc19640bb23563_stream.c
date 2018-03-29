/*
 * stream.c
 *
 *  Created on: 12/07/2009
 *      Author: gr00vy
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <errno.h>

#include "../include/stream.h"

int
stream_create(stream_t **stream, const char *filename)
{
	int fd;
	stream_t *tmp;

	if((fd = open(filename, O_RDONLY)) == -1)
	{
		perror("stream_create():");
		return -1;
	}

	tmp = (stream_t *) malloc(sizeof(stream_t));
	if(!tmp)
	{
		perror("stream_create():");
		*stream = NULL;
		close(fd);
		return -1;
	}

	struct stat sb;
	if (fstat(fd, &sb) == -1)
	{
		perror("stream_create():");
		free(tmp);
		*stream = NULL;
		close(fd);
		return -1;
	}

	void *addr;

	addr = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if(addr == MAP_FAILED)
	{
		perror("stream_create():");
		free(tmp);
		*stream = NULL;
		close(fd);
		return -1;
	}

	tmp->filename 	= strdup(filename);
	tmp->id 		= 0;
	tmp->size 		= sb.st_size;
	tmp->bytes 		= (char *) addr;

	*stream = tmp;

	close(fd);

	return 0;
}

int
stream_destroy(stream_t *stream)
{
	assert(stream != NULL);

	if(munmap(stream->bytes, stream->size) == -1)
	{
		perror("stream_destroy():");
		return -1;
	}

	free(stream->filename);
	free(stream);

	return 0;
}

int
stream_read(stream_t *stream, off_t offset, size_t size, char **buffer)
{
	assert(buffer != NULL);
	assert(size != 0);
	assert(offset+size <= stream->size);

	*buffer = stream->bytes + offset;

	// Retornamos el tamano de bloque que "leimos"
	return (offset + size <= stream->size) ? size : stream->size - offset;
}

