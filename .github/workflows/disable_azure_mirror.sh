#!/bin/bash
echo "APT MIRRORS BEFORE:"
cat /etc/apt/apt-mirrors.txt
sudo sed -i 's@http://azure.archive.ubuntu.com@http://archive.ubuntu.com@gm' /etc/apt/apt-mirrors.txt
echo "APT MIRRORS AFTER:"
cat /etc/apt/apt-mirrors.txt

echo "ls -la /etc/apt/sources.list.d/"
ls -la /etc/apt/sources.list.d/
for f in /etc/apt/sources.list.d/*; do echo "####### $f ######"; cat $f; done
