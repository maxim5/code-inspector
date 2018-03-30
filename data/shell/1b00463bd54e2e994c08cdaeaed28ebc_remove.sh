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

function remove {
  . "$CATS_SCRIPTS_DIR/meta.sh"

  function syntax {
    echo "Usage:"
    echo "$0 $CATS_COMMAND_NAME [dentifier]"
    echo "Note:"
    echo "       'identifier' must exist in the '$CATS_INSTANCES_DIR' directory"
  }

  if [ $# -lt 1 ]; then
    echo "Too few arguments."
    syntax
    exit 1
  fi

  local id="$1"

  if [ $(exists "$id") = false ]; then
    echo "'$id' does not exists."
    exit 2
  fi

  while read -p "Remove instance '$id' (configuration and deployed applications)? (y|n): " answer; do
    case "$answer" in
    y)
      break
      ;;
    n)
      exit
      ;;
    *)
      echo 'y or n'
      ;;
    esac
  done

  removeinstance "$id"

  local instanceDir="$CATS_INSTANCES_DIR/$id"
  rm -r "$instanceDir"/*

  local dir="$instanceDir"
  local stop="$(dirname "$CATS_INSTANCES_DIR")"
  while [ "$dir" != "$stop" ] && rmdir "$dir" &> /dev/null; do
    dir="$(dirname $dir)"
  done
}

if [ "$BASH_SOURCE" == "$0" ]; then
  echo "Don't call this script directly, use the 'cats' facade instead."
  exit 200
fi
