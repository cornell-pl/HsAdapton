ex:
	ghc --make -O3 exadapton.hs -fforce-recomp -isrc
	./exadapton --output exadapton.html
exi:
	ghci exadapton.hs -isrc
txi:
	ghci txadapton.hs -isrc
tx:
	ghc --make -O3 -threaded txadapton.hs -fforce-recomp -isrc
	./txadapton --output txadapton.html
txcshf:
	ghc --make -O3 -threaded txadapton.hs -fforce-recomp -isrc -DCSHF
	./txadapton --output txadapton.html
txprof:
	ghc --make txadapton.hs -fforce-recomp -rtsopts -isrc -threaded -DCSHF
	ghc --make txadapton.hs -osuf p_o -prof -auto -fforce-recomp -rtsopts -isrc -threaded -DCSHF
	./txadapton +RTS -pa -s
benchprof:
	ghc -O3 --make bench.hs -fforce-recomp -rtsopts -isrc -threaded
	ghc -O3 --make bench.hs -osuf p_o -prof -auto -fforce-recomp -rtsopts -isrc -threaded
	./bench +RTS -pa -s
benchprofspace:
	ghc --make -O3 bench.hs -fforce-recomp -isrc
	ghc --make -O3 bench.hs -osuf p_o -prof -auto -fforce-recomp -isrc
	./bench +RTS -hc -p -s
	hp2ps -e8in -c bench.hp
	ps2pdf bench.ps > bench.pdf
benchprofspacefast:
	ghc --make bench.hs -fforce-recomp -isrc
	ghc --make bench.hs -osuf p_o -prof -auto -fforce-recomp -isrc
	./bench +RTS -hc -p -s
	hp2ps -e8in -c bench.hp
	ps2pdf bench.ps > bench.pdf
benchprofspace2:
	ghc --make -O3 bench.hs -fforce-recomp -isrc
	ghc --make -O3 bench.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./bench +RTS -hy -p -S 2> error.log
	hp2ps -e8in -c bench.hp
	ps2pdf bench.ps > bench.pdf
benchprofspace2fast:
	ghc --make bench.hs -fforce-recomp -isrc
	ghc --make bench.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./bench +RTS -hy -p -S 2> error.log
	hp2ps -e8in -c bench.hp
	ps2pdf bench.ps > bench.pdf
testprofspacefast:
	ghc --make test.hs -fforce-recomp -isrc
	ghc --make test.hs -osuf p_o -prof -auto -fforce-recomp -isrc
	./test +RTS -hc -p -s
	hp2ps -e8in -c test.hp
	ps2pdf test.ps > test.pdf
testprofspace2fast:
	ghc --make test.hs -fforce-recomp -isrc
	ghc --make test.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./test +RTS -hy -p -S 2> error.log
	hp2ps -e8in -c test.hp
	ps2pdf test.ps > test.pdf
benx:
	ghc --make -O3 -threaded bench.hs -fforce-recomp -isrc
	./bench --output bench.html
benxfast:
	ghc --make -threaded bench.hs -fforce-recomp -isrc
	./bench --output bench.html
txfast:
	ghc --make -O0 -threaded txadapton.hs -fforce-recomp -isrc
	./txadapton --output txadapton.html
tx1:
	ghc --make -O txadapton.hs -fforce-recomp -isrc
	./txadapton --output txadapton.html
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
txprofspace:
	ghc --make -O3 txadapton.hs -fforce-recomp -isrc
	ghc --make -O3 txadapton.hs -osuf p_o -prof -auto-all -caf-all -fforce-recomp -isrc
	./txadapton +RTS -hc -p -s
	hp2ps -e8in -c txadapton.hp
	ps2pdf txadapton.ps > txadapton.pdf
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
