all:
	ghc -O --make -o lurk lurk.hs

clean:
	find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	rm lurk
