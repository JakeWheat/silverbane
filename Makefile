.PHONY : build
build :
	cabal build

.PHONY : test
test : 
	cabal run test:silverbane-test -- --diff-context=full

.PHONY : check-readme
check-readme :
	cabal build
	# is there a reliable way to get this path?
	# don't want to use cabal install because it takes an
	# age, even worse than most things with cabal
	mkdir -p build_exe
	cp dist-newstyle/build/x86_64-linux/ghc-9.8.1/silverbane-0.1/x/silverbane/build/silverbane/silverbane build_exe/
	PATH=build_exe:$$PATH silverbane README 

.PHONY : website
website : build_website/index.html

build_website/index.html : README
	mkdir -p build_website
	pandoc -s -f markdown README -o build_website/index.html

.PHONY : all
all : build test check-readme website

.PHONY : clean
clean : 
	rm -Rf build_exe
	rm -Rf build_website
	cabal clean
