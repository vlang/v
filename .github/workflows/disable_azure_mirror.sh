#!/bin/bash
echo "APT MIRRORS:"
cat /etc/apt/apt-mirrors.txt
echo "ls -la /etc/apt/sources.list.d/"
ls -la /etc/apt/sources.list.d/
echo "replacing azure with the US mirror ..."
sed -e 's/azure.archive.ubuntu.com/us.archive.ubuntu.com/g' -e t -e d /etc/apt/sources.list | sudo tee /etc/apt/sources.list.d/nonazure.list
echo "/etc/apt/sources.list.d/nonazure.list:"
cat /etc/apt/sources.list.d/nonazure.list
