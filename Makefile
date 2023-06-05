hpip:
	mkdir -p build
	mkdir -p images
	ghc -package JuicyPixels -Wall --make -outputdir build -o $@ -isrc/ Main.hs

clean:
	rm -rf build images hpip

.PHONY: clean