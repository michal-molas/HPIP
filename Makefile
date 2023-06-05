hpip:
	mkdir -p build
	ghc -package JuicyPixels -Wall --make -outputdir build -o $@ Main.hs

clean:
	rm -rf build hpip

.PHONY: clean