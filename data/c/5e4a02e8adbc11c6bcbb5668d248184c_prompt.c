/*
 * Manage input prompt.
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
#include <string.h>
#include <errno.h>

#include <unistd.h>

#include "core/kbsh.h"
#include "core/env.h"
#include "core/prompt.h"

void kbsh_prompt_exit(void)
{
	return;
}

void kbsh_prompt_init(void)
{
	uid_t uid = getuid();

	prompt.root_ch = "# ";
	prompt.dflt_ch = "$ ";
	prompt.scnd_ch = "> ";

	if (!uid) /* root */
		prompt.crnt_ch = prompt.root_ch;
	else if (uid > 0) /* not root */
		prompt.crnt_ch = prompt.dflt_ch;
}
