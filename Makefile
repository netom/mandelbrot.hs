mandelbrot: mandelbrot.hs
	ghc -O2 -rtsopts -threaded --make mandelbrot.hs

.PHONY: clean

clean:
	rm mandelbrot mandelbrot.hi mandelbrot.o
