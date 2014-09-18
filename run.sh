ghc --make t.hs -O2 -threaded -package parallel
time ./t 600 600 170 test2 test2 ooooooooooo.tga +RTS -N1 -RTS
eog ooooooooooo.tga
