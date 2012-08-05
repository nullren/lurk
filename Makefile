all:
	ghc -O --make -o bruinbot bruinbot.hs

clean:
	find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	rm bruinbot
