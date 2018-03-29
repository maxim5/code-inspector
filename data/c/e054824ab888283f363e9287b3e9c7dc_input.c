/*
 * Manage user input.
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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "localize.h"

#include "core/kbsh.h"
#include "core/buffer.h"
#include "core/input.h"
#include "core/env.h"
#include "core/parse.h"
#include "core/prompt.h"

static struct Buffer buffer;

static char *kbsh_gets(char *);
static char *kbsh_input_gets_more(void);
static void kbsh_create_histfname(void);

void kbsh_input_exit(void)
{
	free(history_fname);
	kbsh_prompt_exit();
	kbsh_buffer_reset(&buffer);
}

void kbsh_input_init(void)
{
	kbsh_clean = kbsh_input_exit;
	kbsh_mode = INTR_M;
	kbsh_buffer_gets_more = kbsh_input_gets_more;
	kbsh_prompt_init();
	kbsh_create_histfname();
	rl_outstream = stderr;
	read_history(history_fname);
	rl_bind_key('\t', rl_complete);
}

void kbsh_input_main(void)
{
	while (1) {
		parse_err = 0;

		while (1) {
			/* Input loop */
			if (buffer.full) {
				free(buffer.full);
				buffer.full = NULL;
			}
			buffer.full = kbsh_gets(prompt.crnt_ch);
			if (!buffer.full) {
				puts("exit");
				kbsh_exit(0);
			}
			if (buffer.full && *buffer.full) {
				if (*buffer.full == '#')
					continue;
				break;
			}
		}

		kbsh_parse(&buffer);
		if (parse_err)
			continue;
		kbsh_main(&buffer);
		kbsh_buffer_reset(&buffer);
	}
}

static char *kbsh_gets(char *prompt_in)
{
	char *line = NULL;
	size_t line_size;

	if (!prompt_in)
		prompt_in = "kbsh$ ";

	line = readline(prompt_in);

	if (!line) {
		/* EOF */
		printf("exit\n");
		return line;
	} else if (*line) {
		add_history(line);
		write_history(history_fname);
	}

	/* Readline doesn't include the newline char so we add it */
	line_size = strlen(line) + 1;
	line = realloc(line, sizeof(*line) * (line_size));
	if (!line)
		kbsh_exit(errno);
	strcat(line, "\n");

	return line;/* return readline buffer */
}

static char *kbsh_input_gets_more(void)
{
	char *temp = NULL;
	while (1) {
		if (temp) {
			free(temp);
			temp = NULL;
		}
		temp = kbsh_gets(prompt.scnd_ch);
		if (!temp)
			return temp;
		if (*temp == '#')
			continue;
		else
			break;
	}
	return temp;
}

static void kbsh_create_histfname(void)
{
	if (!env.home)
		return;
	history_fname = NULL;
	const char *name = "/.kbsh_history";
	history_fname = kbsh_env_prepend_homedir(name);
	if (!history_fname)
		kbsh_exit(errno);
}
