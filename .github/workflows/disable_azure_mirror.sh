#!/bin/bash
sed -e 's/azure.archive.ubuntu.com/us.archive.ubuntu.com/g' -e t -e d /etc/apt/sources.list | sudo tee /etc/apt/sources.list.d/nonazure.list
