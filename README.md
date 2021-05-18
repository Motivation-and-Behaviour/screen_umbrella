# Benefits and risks associated with children’s and adolescents’ interactions with electronic screens: An umbrella review

## Using this Repo

1. Make sure you are working off the latest version.
1. Open the Rproj file and run `renv::restore()` to download/update the required packages.
1. You must run `googledrive::drive_auth()` before running targets in order the cache your oAuth token.
1. To build the project, run `targets::tar_make()`.

### Tips

1. Make any major changes on a new branch and pull into main.
1. If you add any new dependencies, make sure they are added to the lock file (`renv::snapshot()`).
1. If `targets::tar_make()` fails, try rerunning `googledrive::drive_auth()`, or checking that the Google account you linked has access to the data sheet.
1. Always rerun `targets::tar_make()` before commiting changes, and include the updated `index.html` in the commit. This keeps the report up to date.

## Links

* [PROSPERO Registration](https://www.crd.york.ac.uk/prospero/display_record.php?RecordID=76051)
* [View the report](https://motivation-and-behaviour.github.io/screen_umbrella/)
