all:
	ghc --make Interpret.hs -o interpreter
clean:
	-rm -f interpreter
