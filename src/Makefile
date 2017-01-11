all: ParLatte.hs
	alex -g LexLatte.x
	ghc --make LatC.hs -o latc_llvm

debug: info.txt

ParLatte.y LexLatte.x: Latte.cf
	bnfc $<

ParLatte.hs: ParLatte.y
	happy -gca $<

info.txt: ParLatte.y
	happy -gca ParLatte.y

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocLatte.ps
	-rm -f latc_llvm
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd XMLLatte.*


