#!/bin/bash
set -u # To deal with unset variables

cd ~/TFE/hera
pwd
git add *
git commit -m "$1"  # This gets the message from the "COMMIT" variable.
git push https://Neackow:$NICOGHPWD@github.com/Neackow/hera.git

cd ~/TFE/movement_detection
pwd
rm -rf ./_build
rm -rf ./_grisp