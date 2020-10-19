set root (realpath (dirname (status --current-filename)))/..
echo Deleting old documentation in $root/docs
rm -rf $root/docs
cd $root/examples
spago docs
echo Copying generated documentation to $root/docs
rsync -a $root/examples/generated-docs/html/ $root/docs/
echo Done.
