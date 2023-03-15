# Get the version info for later
.PHONY: docs
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: docs check clean

docs:
	R -q -e 'library("roxygen2"); roxygenise(".")'

docs-devel:
	R-devel -q -e 'library("roxygen2"); roxygenise(".")'

build: docs
	cd ..;\
	R CMD build --compression=gzip --resave-data gratia

build-openblas: docs
	cd ..;\
	R-openblas CMD build gratia

build-atlas: docs
	cd ..;\
	R-atlas CMD build gratia

build-devel: docs-devel
	cd ..;\
	R-devel CMD build gratia

check: build
	cd ..; \
	export NOT_CRAN="true"; \
	echo "$${NOT_CRAN}"; \
	R CMD check gratia_$(PKGVERS).tar.gz

check-openblas: build-openblas
	cd ..; \
	export NOT_CRAN="true"; \
	echo "$${NOT_CRAN}"; \
	R-openblas CMD check gratia_$(PKGVERS).tar.gz

check-atlas: build-atlas
	cd ..; \
	export NOT_CRAN="true"; \
	echo "$${NOT_CRAN}"; \
	R-atlas CMD check gratia_$(PKGVERS).tar.gz

check-test-cran: build
	cd ..;\
	export NOT_CRAN="false"; \
	echo "$${NOT_CRAN}"; \
	R CMD check gratia_$(PKGVERS).tar.gz

check-test-cran-openblas: build-openblas
	cd ..;\
	export NOT_CRAN="false"; \
	echo "$${NOT_CRAN}"; \
	R-openblas CMD check gratia_$(PKGVERS).tar.gz

check-test-cran-atlas: build-atlas
	cd ..;\
	export NOT_CRAN="false"; \
	echo "$${NOT_CRAN}"; \
	R-atlas CMD check gratia_$(PKGVERS).tar.gz

check-as-cran: build
	cd ..;\
	export NOT_CRAN="false"; \
	echo "$${NOT_CRAN}"; \
	R CMD check --as-cran gratia_$(PKGVERS).tar.gz

check-devel: build-devel
	cd ..;\
	R-devel CMD check gratia_$(PKGVERS).tar.gz

check-as-cran-devel: build-devel
	cd ..;\
	export NOT_CRAN="false"; \
	echo "$${NOT_CRAN}"; \
	R-devel CMD check gratia_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL gratia_$(PKGVERS).tar.gz

move: ../gratia.Rcheck/gratia-Ex.Rout
	cp ../gratia.Rcheck/gratia-Ex.Rout ./tests/Examples/gratia-Ex.Rout.save

clean:
	cd ..;\
	rm -r gratia.Rcheck/

pkgdown: docs
	R -q -e 'library("pkgdown"); build_site(preview = FALSE)'
