# Rat hormone data

`rats.txt` is a copy of
<https://fromthebottomoftheheap.net/teaching/data/rats.txt>, retrieved on
2026-09-21 for the issue #219 plotting regression test. Keeping it here lets
the test run without a network connection.

The source file is preserved verbatim, including missing responses and trailing
whitespace. It contains 350 rows, with 98 missing values in each of `response`
and `eff`. The test retains its existing parsing-warning expectation and model
and plotting assertions.
