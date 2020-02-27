## Resubmission (27-Feb-2020) - version 0.3.1

This is a resubmission of the package. In the previous submission, there were notes when checked with the following two favors:

### r-devel-windows-ix86+x86_64

    New submission

As far as I know, this one can be ignored.

    Possibly mis-spelled words in DESCRIPTION: workflow (10:198)

Workflow is not a mis-spelled word.

### r-devel-linux-x86_64-debian-gcc

    New submission
	
Ditto

    The dataset(s) may use package(s) not declared in Depends/Imports.

I am really sorry for this. In the previous version, I had included an object `abstracts_topicmodels` produced by the package `topicmodels` and this package is only declared in `Suggested`.

I have removed this object from the package. Also, I have added `testthat::skip_on_cran()` for tests related to such object. 

I have retested the updated package on r-hub specifically for this favour and there is no notes about the `abstracts_topicmodels`. Winbuilder reported no error.

## Test environments
* local R installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
