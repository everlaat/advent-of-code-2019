all: elm

elm:
	elm make --optimize ./src/Main.elm --output ./app/script.js;