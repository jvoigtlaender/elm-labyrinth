rm -rf out || exit 0;

npm install -g elm@0.16.0

elm-package install --yes
elm-make --yes

mkdir out
elm-make labyrinth.elm --output out/index.html --yes
cd out

git init
git config user.name "Travis CI"
git config user.email "jvoigtlaender@users.noreply.github.com"
git add .
git commit -m "Travis deploy to gh-pages"
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages >/dev/null 2>&1
