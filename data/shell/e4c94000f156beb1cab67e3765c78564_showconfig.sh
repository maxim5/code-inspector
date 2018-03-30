#!/bin/bash

# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# author: rafal
 
function showconfig {
  echo 'All good, bravo!'
  echo
  echo 'Managed templates:'
  for template in $(listtemplates); do
    echo "$template = $(templatedir "$template")"
  done
  echo
  echo 'Known variables:'
  local varname
  for varname in CATS_SCRIPTS_DIR CATS_COMMANDS_DIR CATS_CONFIG CATS_DIR CATS_USER CATS_INSTANCES_DIR \
      CATS_META_FILE CATS_COMMAND_NAME CATS_COMMAND_ARGS "${!CATS_TEMPLATE_@}" CATS_PID_DIR; do
    local val
    if declare -p "$varname" 2> /dev/null | grep -q '^declare -a'; then
      varname="$varname"'[@]'
      val="$(dumpargs "${!varname}")"
    else
      val="${!varname}"
    fi

    if [ -z "$val" ]; then
      val='(none)'
    fi
    echo "$varname = $val"
  done
  echo
  echo 'Commands:'
  listcommands '    '
}

if [ "$BASH_SOURCE" == "$0" ]; then
  echo "Don't call this script directly, use the 'cats' facade instead."
  exit 200
fi
