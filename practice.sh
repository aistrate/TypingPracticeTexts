echo Status Before:
git status

echo
echo Add:
git add .

echo
echo Commit:
git commit -am '(practice)'

# To set the default as in Git 2.0, run:
#     git config --global push.default simple

# echo
# echo Push:
# git push origin master

echo
echo Status After:
git status

echo; echo; echo
echo Log:
git log -7
