# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: docs check clean

docs:
	R -q -e 'library("roxygen2"); roxygenise(".")'

build: docs
	cd ..;\
	R CMD build schoenberg

check: build
	cd ..;\
	R CMD check schoenberg_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran schoenberg_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL schoenberg_$(PKGVERS).tar.gz

move: check
	cp ../schoenberg.Rcheck/schoenberg-Ex.Rout ./tests/Examples/schoenberg-Ex.Rout.save

clean:
	cd ..;\
	rm -r schoenberg.Rcheck/
