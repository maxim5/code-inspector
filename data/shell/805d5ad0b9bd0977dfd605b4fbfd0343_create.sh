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

function create {
  . "$CATS_SCRIPTS_DIR/meta.sh"

  function syntax {
    echo "Usage:"
    echo "$0 $CATS_COMMAND_NAME [dentifier] [shutdown port] [http port] [https port] [ajp port] [jpda port] [template]"
    echo "Notes:"
    echo "       'identifier' must be unique, i.e. not be any of the ones already in '$CATS_INSTANCES_DIR' directory"
    echo "       'identifier' may contain slashes, which provides for grouping instances"
    echo "       'identifier' must not be 'path prefixes' of already existing instances"
    echo "       all ports must be globall unique, i.e. not be the same and not used by any other instance"
    echo "       'template' is one of:"
    listtemplates "           "
  }

  if [ $# -lt 7 ]; then
    echo "Too few arguments."
    syntax
    exit 1
  fi

  local id="$1"
  shift
  local shutdownPort="$1"
  local httpPort="$2"
  local httpsPort="$3"
  local ajpPort="$4"
  local jpdaPort="$5"
  local template="$6"

  if [[ ! "$id" =~ ^[a-zA-Z0-9_-](/?[a-zA-Z0-9_-])*$ ]]; then
    echo "Invalid characters in '$id'."
    echo "Only latin letters, digits, '_', '-' and optional single slashes between characters are allowed."
    exit 2
  fi

  local instanceDir="$CATS_INSTANCES_DIR/$id"

  if [ $(exists "$id") == true ]; then
    echo "'$id' already exists, but must be unique."
    exit 3
  fi

  local inst
  for inst in $(listinstances); do
    if [[ $id == $inst/* ]]; then
      echo "'$id' is inside an already existing instance '$inst', use another identifier."
      exit 4
    fi
  done

  for inst in $(listinstances); do
    if [[ $inst == $id/* ]]; then
      echo "'$id' is an instance group, use another identifier."
      exit 5
    fi
  done

  if [ -d "$instanceDir" ]; then
    echo "'$id' does not exist yet, but there is such a directory within '$CATS_INSTANCES_DIR'."
    exit 6
  fi

  local colports=$(echo "$@" | tr ' ' '\n')
  if [ $(echo "$colports" | wc -l) -ne $(echo "$colports" | sort -u | wc -l) ]; then
    echo "Duplicate ports specification in '$@'".
    exit 7
  fi

  local ok=""
  # $1 port
  function checknumber {
    if ! [[ "$1" =~ ^[1-9][0-9]*$ ]]; then
      echo "'$1' is not a number."
      ok=false
      return 1
    fi
  }

  # $1 port
  function checkrange {
    if (("$1" < 1024 || "$1" > 65535 )); then
      echo "'$1' is not within valid range <1024; 65535>."
      ok=false
      return 1
    fi
  }

  # $1 port
  function checkused {
    local i=$((2))
    tail -n +2 "$CATS_META_FILE" | while read portLine; do
      if echo "$portLine" | tr -s ' ' | cut -d ' ' -f 2-6 | grep -q "\b$1\b"; then
        echo "'$1' is already used here:"
        echo "(line $i):$portLine"
        exit 1
      fi
      i=$((i + 1))
    done
    if [ $? -ne 0 ]; then
      ok=false
      return 1
    fi
  }

  local port
  for port in "$shutdownPort" "$httpPort" "$httpsPort" "$ajpPort" "$jpdaPort"; do
    checknumber "$port"
    if [ $? -eq 0 ]; then
      checkrange "$port"
      if [ $? -eq 0 ] && [ -f "$CATS_META_FILE" ]; then
        checkused "$port"
      fi
    fi
  done

  if [ -n "$ok" ]; then
    exit 8
  fi

  local templates="$(listtemplates '    ')"
  if ! echo "$templates" | grep -q "\b$template\b"; then
    echo "Wrong variable '$template' used, it must be one of:"
    echo "$templates"
    exit 9
  fi

  local templateDir="$(templatedir "$template")"
  mkdir -p "$instanceDir"/{bin,conf,lib,logs,webapps}
  local file
  find "$templateDir/conf" -type f \! -path "$templateDir/conf/server.xml" | while read file; do
    ln -s "$file" "$instanceDir/conf"
  done
  cp -r "$templateDir/webapps/"{manager,ROOT} "$instanceDir/webapps"

  cp "$templateDir/conf/server.xml" "$instanceDir/conf"
  sed -i -e s/8005/$shutdownPort/g -e s/8080/$httpPort/g -e s/8443/$httpsPort/g -e s/8009/$ajpPort/g "$instanceDir"/conf/server.xml
  echo "export JPDA_ADDRESS=$jpdaPort" > "$instanceDir"/bin/setenv.sh

  addinstance "$id" "$shutdownPort" "$httpPort" "$httpsPort" "$ajpPort" "$jpdaPort" "$template"
}

if [ "$BASH_SOURCE" == "$0" ]; then
  echo "Don't call this script directly, use the 'cats' facade instead."
  exit 200
fi
