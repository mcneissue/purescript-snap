set root (realpath (dirname (status --current-filename)))/..
set currentBranch (git branch --show-current)
set msg "Generated docs "(date "+%m/%d/%y %T")" for branch $currentBranch"

git checkout gh-pages
or exit

echo Deleting old documentation in $root/docs

rm -rf $root/docs

cd $root/examples
spago docs

echo Copying generated documentation to $root/docs
rsync -a $root/examples/generated-docs/html/ $root/docs/

git add -f $root/docs/.

git commit -m $msg
and git push

git checkout $currentBranch
