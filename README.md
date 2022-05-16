
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Benefits and risks associated with children’s and adolescents’ interactions with electronic screens: An umbrella review

<!-- badges: start -->
<!-- badges: end -->

Contains all of the project files to conduct the analysis and generate
the manuscript for the umbrella review.

## Using this Repo

1.  Clone the repository, ensuring that you are using the latest
    version.
2.  Open directory and run `renv::restore()` to download/update the
    required packages.
3.  You must run `googledrive::drive_auth()` and
    `googlesheets4::gs4_auth()` before running targets in order the
    cache your oAuth token.
4.  To build the project, run `targets::tar_make()`.

### Notes

You should be able to run the analysis to examine the reproducability.
However, some of the pipeline targets also write data to Google Sheets,
which will cause these targets to fail. If you are not a project
contributor, you may need to manually exclude these targets.

You can also run the build in parallel using
`targets::tar_make_clustermq(workers=<num_workers>)`. This can be
faster, but not always. There’s usually no benefit beyond 3 or 4
workers.

## Links

-   [PROSPERO
    Registration](https://www.crd.york.ac.uk/prospero/display_record.php?RecordID=76051)
-   [Open Science Framework](https://osf.io/3ubqp/)
