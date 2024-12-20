.PHONY: dist
dist:
	npm run build
	cp dist/* ../koyo-lib/koyo/job/assets/
	gzip -f ../koyo-lib/koyo/job/assets/app.{css,js}{,.map}
