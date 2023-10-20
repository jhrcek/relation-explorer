.PHONY: build
build:
	elm make src/Main.elm --output=dist/js/elm.js

.PHONY: live
live:
	elm-live --open --dir=dist -- src/Main.elm --output=dist/js/elm.js --debug

.PHONY: format
format:
	elm-format --yes src

.PHONY: review
review:
	elm-review
