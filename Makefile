all:
	ghc -O --make -o bruinbot bruinbot.hs

clean:
	rm bruinbot.o
	rm bruinbot.hi
	rm bruinbot
