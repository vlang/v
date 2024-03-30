#!/usr/bin/env bash

while ! $@; do "command failed, retrying ..."; sleep 1; done

