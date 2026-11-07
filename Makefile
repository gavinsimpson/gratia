# Get the version info for later
.PHONY: docs build check check-as-cran check-release test test-full test-hgam-paper
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
TEST_CPUS ?= 4

all: docs check clean

# Routine local runs use the tests selected for CRAN, with local parallelism.
test:
	NOT_CRAN=false GRATIA_RUN_HGAM=false TESTTHAT_PARALLEL=true TESTTHAT_CPUS=$(TEST_CPUS) Rscript --vanilla tools/run-tests.R

# Include visual snapshots, keeping the paper examples separate.
test-full:
	NOT_CRAN=true GRATIA_RUN_HGAM=false VDIFFR_RUN_TESTS=true TESTTHAT_PARALLEL=true TESTTHAT_CPUS=$(TEST_CPUS) Rscript --vanilla tools/run-tests.R

# Run only the paper examples when explicitly requested.
test-hgam-paper:
	NOT_CRAN=true GRATIA_RUN_HGAM=true GRATIA_TEST_FILTER='^hgam-paper$$' VDIFFR_RUN_TESTS=false TESTTHAT_PARALLEL=false TESTTHAT_CPUS=1 Rscript --vanilla tools/run-tests.R

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
	export NOT_CRAN="false"; \
	export GRATIA_RUN_HGAM="false"; \
	export TESTTHAT_PARALLEL="true"; \
	export TESTTHAT_CPUS="$(TEST_CPUS)"; \
	echo "$${NOT_CRAN}"; \
	R CMD check --as-cran gratia_$(PKGVERS).tar.gz

# Include all local tests, visual snapshots and HGAM paper examples.
# Limit OpenMP to one thread per R process during building and checking.
check-release: export OMP_NUM_THREADS = 1
check-release: export OMP_THREAD_LIMIT = 1
check-release: build
	cd .. && \
	NOT_CRAN=true GRATIA_RUN_HGAM=true VDIFFR_RUN_TESTS=true \
	TESTTHAT_PARALLEL=true TESTTHAT_CPUS=$(TEST_CPUS) \
	R CMD check --as-cran gratia_$(PKGVERS).tar.gz

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

check-as-cran: check

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
