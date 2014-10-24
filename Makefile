ex:
	ghc --make -O3 exadapton.hs -fforce-recomp -isrc
	./exadapton --output exadapton.html
exfast:
	ghc --make -O0 exadapton.hs -isrc
	./exadapton --output exadapton.html
exprof:
	ghc --make -O3 exadapton.hs -fforce-recomp -rtsopts -isrc
	ghc --make -O3 exadapton.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -rtsopts -isrc
	./exadapton +RTS -pa -s
exprofspace:
	ghc --make -O3 exadapton.hs -fforce-recomp -isrc
	ghc --make -O3 exadapton.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./exadapton +RTS -hc -p -s
	hp2ps -e8in -c exadapton.hp
	ps2pdf exadapton.ps > exadapton.pdf
exprofspace2:
	ghc --make -O3 exadapton.hs -fforce-recomp -isrc
	ghc --make -O3 exadapton.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./exadapton +RTS -hy -p -S 2> error.log
	hp2ps -e8in -c exadapton.hp
	ps2pdf exadapton.ps > exadapton.pdf
clean:
	find . -name "*.hi" -type f -delete
	find . -name "*.p_o" -type f -delete 
	find . -name "*.dyn_hi" -type f -delete 
	find . -name "*.dyn_o" -type f -delete 
	find . -name "*.o" -type f -delete 
	rm -rf dist