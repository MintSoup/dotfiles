#!/bin/fish

set file_name (mktemp)
make
hyprctl plugin unload $(cat old_name)
cp ./libhypr-grid-nav.so $file_name
echo $file_name > old_name
hyprctl plugin load $file_name
