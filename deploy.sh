rm -rf out || exit 0;

npm install -g elm@0.16

elm-package install --yes
elm-make --yes

mkdir out
mv ll1.html out
elm-make Main.elm --output out/ll1.js --yes
cd out
