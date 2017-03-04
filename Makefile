# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: docs check clean

docs:
	R -q -e 'library("roxygen2"); roxygenise(".")'

build: docs
	cd ..;\
	R CMD build tsgam

check: build
	cd ..;\
	R CMD check tsgam_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran tsgam_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL tsgam_$(PKGVERS).tar.gz

move: check
	cp ../tsgam.Rcheck/tsgam-Ex.Rout ./tests/Examples/tsgam-Ex.Rout.save

clean:
	cd ..;\
	rm -r tsgam.Rcheck/
