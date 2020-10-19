set root (realpath (dirname (status --current-filename)))/..
cd $root/examples
spago docs
