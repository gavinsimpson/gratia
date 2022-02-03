#' Lead-210 age-depth measurements for Small Water
#'
#' A dataset containing lead-210 based age depth measurements for the SMALL1
#' core from Small Water.
#'
#' The variables are as follows:
#'
#' * `Depth`
#' * `Drymass`
#' * `Date`
#' * `Age`
#' * `Error`
#' * `SedAccRate`
#' * `SedPerCentChange`
#'
#' @format A data frame with 12 rows and 7 variables.
#'
#' @source Simpson, G.L. (Unpublished data).
#'
#' @keywords data
#' @name smallAges
#' @docType data
NULL

#' Simulated bird migration data
#'
#' Data generated from a hypothetical study of bird movement along a migration
#' corridor, sampled throughout the year. This dataset consists of simulated
#' sample records of numbers of observed locations of 100 tagged individuals
#' each from six species of bird, at ten locations along a latitudinal gradient,
#' with one observation taken every four weeks. Counts were simulated randomly
#' for each species in each location and week by creating a species-specific
#' migration curve that gave the probability of finding an individual of a
#' given species in a given location, then simulated the distribution of
#' individuals across sites using a multinomial distribution, and subsampling
#' that using a binomial distribution to simulation observation error (i.e.
#' not every bird present at a location would be detected). The data set
#' (`bird_move`) consists of the variables `count`, `latitude`, `week` and
#' `species`.
#'
#' @source Pedersen EJ, Miller DL, Simpson GL, Ross N. 2018. Hierarchical
#'   generalized additive models: an introduction with mgcv. *PeerJ Preprints*
#'   **6**:e27320v1 \doi{10.7287/peerj.preprints.27320v1}.
#'
#' @format A data frame
#' @name bird_move
#' @keywords data
#' @docType data
NULL

#' Madison lakes zooplankton data
#'
#' The Madison lake zooplankton data are from a long-term study in seasonal
#' dynamics of zooplankton, collected by the Richard Lathrop. The data were
#' collected from a chain of lakes in Wisconsin (Mendota, Monona, Kegnonsa,
#' and Waubesa) approximately bi-weekly from 1976 to 1994. They consist of
#' samples of the zooplankton communities, taken from the deepest point of each
#' lake via vertical tow. The data are provided by the Wisconsin Department of
#' Natural Resources and their collection and processing are fully described in
#' Lathrop (2000).
#'
#' Each record consists of counts of a given zooplankton taxon taken from a
#' subsample from a single vertical net tow, which was then scaled to account
#' for the relative volume of subsample versus the whole net sample and the area
#' of the net tow and rounded to the nearest 1000 to give estimated population
#' density per m2 for each taxon at each point in time in each sampled lake.
#'
#' @source Pedersen EJ, Miller DL, Simpson GL, Ross N. 2018. Hierarchical
#'   generalized additive models: an introduction with mgcv. *PeerJ Preprints*
#'   **6**:e27320v1 \doi{10.7287/peerj.preprints.27320v1}.
#'
#' @references Lathrop RC. (2000). Madison Wisonsin Lakes Zooplankton 1976--1994.
#'   Environmental Data Initiative.
#'
#' @format A data frame
#' @name zooplankton
#' @keywords data
#' @docType data
NULL

#' Data from the General Social Survey (GSS) from the National Opinion Research
#' Center of the University of Chicago
#'
#' A subset of the data from the `carData::GSSvocab` dataset from the
#' `carData` package, containing observations from 2016 only.
#'
#' @format A data frame with 1858 rows and 3 variables:
#'   * `vocab`: numeric; the number of words out of 10 correct on a vocabulary
#'     test.
#'   * `nativeBorn`: factor; Was the respondent born in the US? A factor with
#'     levels `no` and `yes`.
#'   * `ageGroup`: factor; grouped age of the respondent with levels `18-29`
#'       `30-39`, `40-49`, `50-59`, and `60+`.##' 
#'
#' @keywords data
#' @name gss_vocab
#' @docType data
NULL

#' Reference simulation data
#'
#' A set of reference objects for testing [data_sim()].
#'
#' @format A named list of simulated data sets created by [data_sim()].
#'
#' @keywords data
#' @name ref_sims
#' @docType data
NULL

