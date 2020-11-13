## Resubmission (13-Nov-2020) - version 0.3.11

It seems that the package failed the test `checking re-building of vignette outputs` for win-builder (R-devel). I have rechecked the package with win-builder for two times and I could't reproduce such error. Also, with GitHub Actions I could't reproduce such error too. A possible explanation to this is that the vignette depends on randomness and in some weird case it will draw a weird sample that generates an error. In the meantime, I have set up a random seed in that vignette.

## Resubmission (20-Mar-2020) - version 0.3.4

    functions which are supposed to only run interactively (e.g. shiny) should be wrapped in if(interactive()). Is this the case?

With reference to similar packages (editData etc.), wrapping this example in `if(interactive())` makes more sense. I have done so for the example of `summarize_ooolong`. Thank you very much!

## Resubmission (16-Mar-2020) - version 0.3.3

    Regarding \dontrun in summarize_oolong.Rd:
    \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.
	Please fix and resubmit.

The chief reason for wrapping the example is that it can't proceed correctly without doing the human-in-the-loop validation test `oolong_test1$do_word_intrusion_test()`. Of course, users can do that test themselves so I can see your point. I have replaced `\dontrun{}\` with `\donttest{}`, let's see if it fits. Thank you very much!

## Resubmission (7-Mar-2020) - version 0.3.2

     If there are references describing (the theoretical backgrounds of) the methods in your package, please add these in the description field of your DESCRIPTION file in the form
     authors (year) <doi:...>
     authors (year) <arXiv:...>
     authors (year, ISBN:...)
     or if those are not available: <https:...>
     with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add the title as well, quote it. --> "title")

This is a resubmission of the package. The chief complaint is about the documentation. In this version, I have revised the DESCRIPTION to include the information about the two papers (Chang et al and Song et al).

     Please add \value to all .Rd files that are not data files and explain the functions results in the documentation.
     f.i.: print.oolong_gold_standard.Rd
     If a function does not return a value, please document that too, e.g. \value{None}.

     A .Rd file title should:
     - be capitalized
     - not end in a period
     - be at most 65 characters long
     - exist and stand alone (there must be exactly one title)

Almost all .Rd files for functions have been updated. I have confirmed that all titles are capitalized and not ended with a period. Also, all .Rd for functions have now a section on value, including those `print` functions.

I hope all problems have been fixed. I am looking for seeing my package on CRAN.

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
