# Contributing

## Something isn't working right or generating an error

If something isn't working, either as you might expect/want or is contrary to the documentation, its probably a bug or missing feature. If you're getting an error while running {gratia}, that's also likely a bug, or an opportunity to catch a use-case I wasn't expecting.

First, check that your issue hasn't already been fixed in the in-development version on Github. Install the current development version of {gratia} from R-universe using
```r
install.packages("gratia", repos = c(
  "https://gavinsimpson.r-universe.dev",
  "https://cloud.r-project.org"
))
```

If the issue remains, please file an issue via the [Issues page](https://github.com/gavinsimpson/gratia/issues).

It's OK to report an issue even if you're not sure if there's a problem, but if your problem is better described as a *question*, use the [Discussions page](https://github.com/gavinsimpson/gratia/discussions) (see below).

Feature requests are welcome!

Your problem with {gratia} will hit the top of my TODO list more quickly if you cen provide a reproducible example demonstrating the problem.

## Got a question? Want to show how to do something with {gratia}?

If your issue is best described as a question, or you want to know how to do something with {gratia}, or you have a cool example of using {gratia} and want to share it, please consider using the [Discussions page](https://github.com/gavinsimpson/gratia/discussions).

If you're not sure, you can always ask about your issue as a Question (Use the *Q&A* category) and if it really is a bug, I can easily create an Issue from the discussion.

## Code contributions

Code contributions in the form of Pull Requests are always appreciated.

A suitable workflow:

* Fork this repo to your Github account
* Clone your version on your account down to your machine from your account, e.g,. `git clone https://github.com/<yourgithubusername>/gratia.git`
* Make sure to track progress upstream (i.e., on my version of `gratis` at `gavinsimpson/gratia`) by doing `git remote add upstream https://github.com/gavinsimpson/gratia.git`. Before making changes make sure to pull changes in from upstream by doing either `git fetch upstream` then merge later or `git pull upstream` to fetch and merge in one step
* Make your changes (bonus points for making changes on a new feature branch)
* Push up to your account
* Submit a pull request to home base at `gavinsimpson/gratia`

### Development tools / paradigm / ethos

Please note the following when contributing code:

* {gratia} is tightly aligned with the **tidyverse**
* I use {dplyr} and related packages a lot internally while developing the package. I do plan on replacing some of this code with lower-level code using {vctrs}, but right now development should focus on filling out the functionality of the package and not on premature optimisation.
* The original aim of {gratia} was to provide {ggplot2} plotting for smooths; please stick to this principle and only use this plotting paradigm.
* I aim the be {mgcv}-feature complete; if {mgcv} can do something in terms of plotting smooths, handling specialists smooths, etc, then in principle {gratia} should support this too.
* I do aim for general compatibility with {mgcv}; if {gratia} deviates from how {mgcv} does things, it needs a good justification; an example of where {gratia} deviates is in how multivariate isotropic smooths fitted with `s()` are plotted, because Dave Miller (@dill) argued convincingly why this is the way.
* Don't add dependencies! Unless accompanied by a strong justification, I want to reduce dependencies not increase their number.
* I'm using {styler} to style the code, using 2 spaces as indent. Most of the code was written before this change, however.
* Respect the 80 character line length limit.
* If you contribute functionality or fix a bug, please add a test using the {testthat} framework to insure the new things works correctly or the bug stays fixed.
* All contributions must *not* result in new NOTES, WARNINGS, or ERRORS when running `R CMD check --as-cran`; please check your contributions against Winbuilder for example.

## Email

I hate email!

You can email me at my GMail address &mdash; it's not like I can stop you :-) &mdash; but unless you capture my attention immediately and label your message as {gratia}-related, it will quickly get swamped never to be seen again. Even if I do tag it, that's no guarantee that I will ever get round to replying; what usually happens is that I forget about it and don't remember to check the *gratia* label that often.

The end result is that if you email me, if you get a response at all, any response from me will be tardy.

It is infinitely better to use the Discussions and Issues pages on Github to ask questions about the package or report problems.

Please don't email my work (academic) address (or any other email address you might find for me) about {gratia}.

If you have a question about GAMs, it would be much better to ask it on [CrossValidated]() or [StackOverflow]() depending on whether your question is statistical or programming related. If it relates to {gratia} you can use the [Discussions page](). Asking me a question in public allows me to reply in public and that contributes to the body of knowledge that is easily available to others.

Under no circumstances should you send you me an email to multiple of my addresses; that is the quickest way to get your message in the trash.