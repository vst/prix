#!/usr/bin/env bash

## Consumes a GitHub user handle (--user <USERNAME>) or a GitHub organization
## handle (--org <ORGANIZATION>) and the project number (--project <PROJECT_NUMBER>),
## issues a gh CLI request, transforms the data and return it in JSON format.
##
## The script expects an environment variable for the directory of the data files:
##
##   GH_PRIX_PROJECT_ITEM_LIST
##
## This variable defaults to the current working directory if not set.

## Prints usage information.
_usage() {
  >&2 echo "Usage: ${0} (--user <USERNAME> | --org <ORGANIZATION>) --project <PROJECT_NUMBER>"
}

## Attempts to retrieve the raw, paginated data from GitHub.
_gh() {
  local _shared="${1}"
  local _entity="${2}"
  local _handle="${3}"
  local _number="${4}"
  local _gql

  if [[ "${_entity}" == "org" ]]; then
    _gql="$(cat "${_shared}/for_org.gql")"
  else
    _gql="$(cat "${_shared}/for_usr.gql")"
  fi

  gh api graphql \
    --paginate \
    --slurp \
    -f query="${_gql}" \
    -f login="${_handle}" \
    -F number="${_number}"
}

## Transforms raw, paginated data into a flat list of project items.
_jq() {
  jq --from-file "${1}/transform.jq"
}

## Declare and initialize variables:
_entity="usr"
_handle=""
_number=""
_shared="${GH_PRIX_PROJECT_ITEM_LIST:-$(pwd)}"

## Parse command line arguments:
while [[ "$#" -gt 0 ]]; do
  case ${1} in
  --user)
    _handle="${2}" ## TODO: Ensure that "${2}" is provided.
    shift
    ;;
  --org)
    _handle="${2}" ## TODO: Ensure that "${2}" is provided.
    _entity="org"
    shift
    ;;
  --project)
    _number="${2}" ## TODO: Ensure that "${2}" is provided.
    shift
    ;;
  *)
    echo "Unknown parameter passed: ${1}"
    _usage
    exit 1
    ;;
  esac
  shift
done

## Check arguments:
if [[ -z "${_handle}" || -z "${_number}" ]]; then
  >&2 echo "Error: Missing required arguments."
  _usage
  exit 1
fi

## Issue the request, get the raw response, transform and print it:
_gh "${_shared}" "${_entity}" "${_handle}" "${_number}" |
  _jq "${_shared}"
