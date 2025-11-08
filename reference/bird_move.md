# Simulated bird migration data

Data generated from a hypothetical study of bird movement along a
migration corridor, sampled throughout the year. This dataset consists
of simulated sample records of numbers of observed locations of 100
tagged individuals each from six species of bird, at ten locations along
a latitudinal gradient, with one observation taken every four weeks.
Counts were simulated randomly for each species in each location and
week by creating a species-specific migration curve that gave the
probability of finding an individual of a given species in a given
location, then simulated the distribution of individuals across sites
using a multinomial distribution, and subsampling that using a binomial
distribution to simulation observation error (i.e. not every bird
present at a location would be detected). The data set (`bird_move`)
consists of the variables `count`, `latitude`, `week` and `species`.

## Format

A data frame

## Source

Pedersen EJ, Miller DL, Simpson GL, Ross N. 2018. Hierarchical
generalized additive models: an introduction with mgcv. *PeerJ
Preprints* **6**:e27320v1
[doi:10.7287/peerj.preprints.27320v1](https://doi.org/10.7287/peerj.preprints.27320v1)
.
