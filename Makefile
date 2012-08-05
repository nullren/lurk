all:
	ghc -O -o bruinbot bruinbot.hs

clean:
	rm bruinbot.o
	rm bruinbot.hi
	rm bruinbot
