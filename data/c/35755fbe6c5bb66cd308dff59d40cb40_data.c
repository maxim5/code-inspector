/*
 *  data.c
 *  libanidb
 *
 *  Created by Andreas Meingast on 25.08.06.
 *  Copyright 2006 Andreas Meingast. All rights reserved.
 *
 *  $Id: data.c 1 2007-03-24 13:06:42Z ameingast $
 *
 */

#include <anidb.h>

ani_response_t *ani_get_anime_by_anime_id(int anime_id)
{
	assert(anime_id > 0);
    return ani_send_cmd(ANIDB_CMD_ANIME_BY_AID, anime_id);
}

ani_response_t *ani_get_anime_by_anime_id_and_acode(int anime_id, int acode)
{
	assert(anime_id > 0);
	assert(acode >= 0);
    return ani_send_cmd(ANIDB_CMD_ANIME_BY_AID_AND_ACODE, anime_id, acode);
}

ani_response_t *ani_get_anime_by_name(const char *anime_name)
{
	assert(NULL != anime_name);
    return ani_send_cmd(ANIDB_CMD_ANIME_BY_NAME, anime_name);
}

ani_response_t *ani_get_anime_by_name_and_acode(const char *anime_name,
    int acode)
{
	assert(NULL != anime_name);
	assert(acode >= 0);
    return ani_send_cmd(ANIDB_CMD_ANIME_BY_NAME_AND_ACODE, anime_name, acode);
}

ani_response_t *ani_get_episode_by_id(int episode_id)
{
	assert(episode_id > 0);
    return ani_send_cmd(ANIDB_CMD_EPISODE_BY_EID, episode_id);
}

ani_response_t *ani_get_episode_by_anime_name(const char *anime_name,
    int episode_number)
{
	assert(NULL != anime_name);
	assert(episode_number > 0);
    return ani_send_cmd(ANIDB_CMD_EPISODE_BY_ANAME_AND_EPNO, anime_name,
        episode_number);
}

ani_response_t *ani_get_episode_by_anime_id(int anime_id, int episode_number)
{
	assert(anime_id > 0);
	assert(episode_number > 0);
    return ani_send_cmd(ANIDB_CMD_EPISODE_BY_AID_AND_EPNO, anime_id,
        episode_number);
}

ani_response_t *ani_get_file_by_id(int file_id)
{
	assert(file_id > 0);
    return ani_send_cmd(ANIDB_CMD_FILE_BY_FID, file_id);
}

ani_response_t *ani_get_file_by_id_and_fcode(int file_id, int fcode, int acode)
{
	assert(file_id > 0);
	assert(fcode >= 0);
	assert(acode >= 0);
    return ani_send_cmd(ANIDB_CMD_FILE_BY_FID_AND_FCODE, file_id, fcode, acode);
}

ani_response_t *ani_get_file_by_md4(int file_size, const char *md4_digest)
{
	assert(file_size > 0);
	assert(NULL != md4_digest);
    return ani_send_cmd(ANIDB_CMD_FILE_BY_MD4, file_size, md4_digest);
}

ani_response_t *ani_get_file_by_md4_and_fcode(int file_size,
    const char *md4_digest, int fcode, int acode)
{
	assert(file_size > 0);
	assert(NULL != md4_digest);
	assert(fcode >= 0);
	assert(acode >= 0);
    return ani_send_cmd(ANIDB_CMD_FILE_BY_MD4_AND_FCODE, file_size, md4_digest,
        fcode, acode);
}

ani_response_t *ani_get_group_by_id(int group_id)
{
	assert(group_id > 0);
    return ani_send_cmd(ANIDB_CMD_GROUP_BY_GID, group_id);
}

ani_response_t *ani_get_group_by_name(const char *group_name)
{	
	assert(NULL != group_name);
    return ani_send_cmd(ANIDB_CMD_GROUP_BY_GNAME, group_name);
}

ani_response_t *ani_get_producer_by_id(int producer_id)
{	
	assert(producer_id > 0);
    return ani_send_cmd(ANIDB_CMD_PRODUCER_BY_PID, producer_id);
}

ani_response_t *ani_get_producer_by_name(const char *producer_name)
{
	assert(NULL != producer_name);
    return ani_send_cmd(ANIDB_CMD_PRODUCER_BY_PNAME, producer_name);
}
