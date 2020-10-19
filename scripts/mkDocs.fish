set root (realpath (dirname (status --current-filename)))/..
cd $root

set changes (git status --porcelain)
test -z "$changes"
or begin; echo Working branch must be clean to generate docs.; exit; end

set currentBranch (git branch --show-current)
set msg "Generated docs "(date "+%m/%d/%y %T")" for branch $currentBranch"

echo Deleting old documentation in $root/docs

rm -rf $root/docs

cd $root/examples
spago docs

git stash
git checkout gh-pages
or exit
git stash pop

echo Copying generated documentation to $root/docs

rsync -a $root/examples/generated-docs/html/ $root/docs/

git add -f $root/docs/.

git commit -m $msg
and git push

git checkout $currentBranch
