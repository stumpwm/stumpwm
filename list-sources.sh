#!/usr/bin/env sh
# Parses dependencies from stumpwm.asd and prints them line by line.
# File reading code from: https://www.rosettacode.org/wiki/Read_a_file_line_by_line#UNIX_Shell
set -euoE pipefail

exec 3<stumpwm.asd
oldifs=$IFS

module=

while IFS= ; read -r line <&3
do

  IFS=$oldifs

  if echo "$line" | grep '(:module' > /dev/null
  then
      module=$(echo "$line" | grep -Eo '"[a-z-]+"' | grep -Eo '[a-z-]+')
      continue
  fi

  if ! echo "$line" | grep '(:file' > /dev/null
  then
      continue
  fi

  file=$(echo "$line" | grep -Eo '"[a-z-]+"' | grep -Eo '[a-z-]+')
  file="${file}.lisp"

  if [ -n "$module" ]
  then
      printf '%s/%s\n' "$module" "$file"
      module=
  else
      printf '%s\n' "$file"
  fi

done

IFS=$oldifs
exec 3>&-
