## Test environments
* local macOS Sequoia, R 4.5.1
* win-builder (devel) using R Under development (unstable) (2026-03-05 r89546 ucrt)

## R CMD check results
0 errors | 0 warnings | 1 note

## Resubmission
This is a resubmission. I have addressed the following issues raised by the reviewer:

* **References** References have been added to the Description field where applicable in the required format.

* **Examples wrapped in \donttest{}:** I have unwrapped the examples for all functions. To ensure they pass the 5-second 
  execution threshold, I have reduced the original examples (e.g., using fewer 
  bootstrap iterations) to run quickly during testing.

* **Spelling NOTE:** Technical terms and names ("Bipolarization", "Sobel", "Wasserstein", 
  "Sommerfeld", "Efron", "Narasimhan") have been verified and added to 
  `inst/WORDLIST`.

## R CMD check results (detailed)
* checking CRAN incoming feasibility ... NOTE (New submission)
  - This is a new submission.
  - Spelling notes are for technical terms/names included in the WORDLIST.

## Downstream dependencies
There are currently no downstream dependencies for this package.
