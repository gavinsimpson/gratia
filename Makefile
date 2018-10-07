# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: docs check clean

docs:
	R -q -e 'library("roxygen2"); roxygenise(".")'

build: docs
	cd ..;\
	R CMD build gratia

check: build
	cd ..;\
	R CMD check gratia_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran gratia_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL gratia_$(PKGVERS).tar.gz

move: check
	cp ../gratia.Rcheck/gratia-Ex.Rout ./tests/Examples/gratia-Ex.Rout.save

clean:
	cd ..;\
	rm -r gratia.Rcheck/

pkgdown:
	R -q -e 'library("pkgdown"); build_site(preview = FALSE)'
