## Test environments
* local macOS Sequoia, R 4.5.1
* win-builder (devel) using R Under development (unstable) (2026-02-26 r89489 ucrt)

## R CMD check results
0 errors | 0 warnings | 2 notes

## Resubmission
This is a resubmission. In the previous submission, two NOTEs were raised.

* NOTE: "checking examples ... [23s] NOTE"
  - I have wrapped the example for `WBI_boot()` in `\donttest{}` because the 
    bootstrap procedure naturally exceeds the 5-second threshold.
  
* NOTE: "checking CRAN incoming feasibility ... NOTE (New submission)"
  - This is a new submission.
  - Regarding the "possibly misspelled words": Bipolarization, Sobel, and 
    Wasserstein are technical terms and names essential to the methodology. 
    They have been added to the WORDLIST.

## Downstream dependencies
There are currently no downstream dependencies for this package.
