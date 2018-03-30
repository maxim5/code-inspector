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

function invoke {

  function syntax {
    echo "Usage:"
    echo "$0 $CATS_COMMAND_NAME [identifier] [command [...]]"
    echo "Notes:"
    echo "       'identifier' means one of the instances in the '$CATS_INSTANCES_DIR' directory"
    echo "       'command' is any valid catalina.sh command and it might be multiple words (like 'jpda start')"
  }

  if [ $# -lt 2 ]; then
    echo "Too few arguments."
    syntax
    exit 1
  fi

  local id="$1"
  shift
  CATALINA_BASE="$CATS_INSTANCES_DIR/$id"

  if [ $(exists "$id") = false ]; then
    echo "'$id' does not exists."
    exit 2
  elif [ ! -d "$CATALINA_BASE" ]; then
    echo "'$id' exists, but there is no such a directory within '$CATS_INSTANCES_DIR'."
    exit 3
  fi

  export CATALINA_HOME="$(id2templatedir "$id")"
  export CATALINA_BASE
  export CATALINA_PID="$CATS_PID_DIR/instance_$(replaceslashes "$id" ".").pid"

  "$CATALINA_HOME/bin/catalina.sh" "$@"
}

if [ "$BASH_SOURCE" == "$0" ]; then
  echo "Don't call this script directly, use the 'cats' facade instead."
  exit 200
fi
