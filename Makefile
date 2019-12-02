all: elm

elm:
	yarn install; elm make --optimize ./src/Main.elm --output ./docs/script.js;