/*
 * Manage environmental variables.
 * Copyright (C) 2011 Zack Parsons <k3bacon@gmail.com>
 *
 * This file is part of kbsh.
 *
 * Kbsh is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Kbsh is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with kbsh.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "core/kbsh.h"
#include "core/env.h"

static void kbsh_env_get_cwd_end(void);

void kbsh_env_exit(void)
{
	free(env.cwd);
}

void kbsh_env_init(void)
{
	env.cwd = getcwd(NULL, 0);
	if (!env.cwd)
		kbsh_exit(errno);
	env.home = getenv("HOME");
	if (!env.home)
		kbsh_exit(errno);
	env.user = getenv("USER");
	if (!env.user)
		kbsh_exit(errno);
	kbsh_env_get_cwd_end();
}

void kbsh_env_update(void)
{
	if (env.cwd)
		free(env.cwd);
	env.cwd = getcwd(NULL, 0);
	if (!env.cwd)
		kbsh_exit(errno);
	kbsh_env_get_cwd_end();
}

char *kbsh_env_prepend_homedir(const char *dir)
{
	if (!dir) {
		errno = EINVAL;
		return NULL;
	}
	size_t home_len = strlen(env.home);
	size_t dir_len  = strlen(dir);
	char *full = NULL;

	full = malloc(sizeof(*full) * (home_len + dir_len + 1));
	if (!full)
		kbsh_exit(errno);

	strcpy(full, env.home);
	strcat(full, dir);

	return full;
}

static void kbsh_env_get_cwd_end(void)
{
	if (!env.cwd || !env.home)
		return;
	char *cwd = env.cwd;
	char *sep = "/";
	size_t index = 0;
	if (!strcmp(cwd, env.home)) {
		env.cwd_end = "~";
		return;
	}
	while (cwd[index] != '\0')
		index++;
	while (cwd[index] != *sep)
		index--;
	if (&cwd[index] == cwd && !cwd[index + 1])
		cwd = sep;
	else
		cwd = &cwd[index + 1];
	env.cwd_end = cwd;
}
