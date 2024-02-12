# rcmip6 (development version)

## New features

-   `cmip_download()` will check available space and stop with an error if there isn't enough. You can bypass this by setting `check_diskspace = FALSE`.
-   Download parameters (maximum concurrent, downloads, maximum number of retries, etc.) can be changed with the new `download_config` argument in `cmip_download()`.

## Breaking changes

-   The database of available files is now somewhat centralised at the root of the download folders. Each "download event" (a call to `cmip_download()`) will write its own json file with the information of the downloaded files (this is to prevent multiple concurrent downloads to try to read and write the same file). Having to read just a few files in a single folder should speed up `cmip_available()` tremendously because it doesn't need to list potentially tens of thousands of files in a deeply-nested folder structure. However, this completely breaks backwards compatibility. The `cmip_avaiable_legacy()` function will read data from the previous versions.
-   The return value of the new `cmip_available()` is not compatible with the old one. The previous version returned a data.frame with one row per model, with a nested list column with the various files associated with it. The new version returns a data.frame with one column per file.

## Bugfixes

-   Fixed error when a download fails without saving any data.

# rcmip6 0.0.2

## New Features

-   To speed up downloads, `cmip_download()` will try to read the checksum of each file from a previously-saved file with extension `.chksum` and it will create it if it doesn't exist.
-   `cmip_download()` is now rewritten using concurrent downloads.
-   `cmip_search()` now allows to search for replicas.
-   `filter_replicas()` will check online status of the datanodes and return a single replica per result that's online.
-   `cmip_search()` now turns most columns into vector columns instead of list columns.
-   `cmip_search()` accepts a character vector query. It will be interpreted as a list of instances.
-   `cmip_download()` will only write instance information if at least one file was downloaded successfully and it will include a `complete` column which records if all files assoiated with an instance were downloaded successfully.
-   `cmip_download()` gains a `year_range` argument to only download files corresponding to a specific range of years. A fair number of models split their data into several files by year, decade, 50 year intervals, etc.... With this argument you can download only the files that have the data you're interested in. (#7 and #8. Thanks @jcvdav)
-   New `cmip_urls()` to obtain the URLs that would be downloaded by `cmip_download()`.

## Breaking changes

-   Now `cmip_search()` returns a data.frame instead of a list.
    Therefore, the `as.data.frame()` method is deprecated.
    To get the same result, use `cmip_simplify()`, which just removes a lot of potentially unnecessary columns.

-   The information of downloaded files is now saved in a different format which is not compatible with previous versions.
    Therefore, `cmip_available()` will fail (it failed anyway due to a bug).
    There currently no way of migrating to the new format, but if you run your previous searches, `cmip_download()` will skip already-downloaded files and update the information.
    This way of storing information about available files will most likely change, since it's very inefficient for large datasets of thousands of files.
    Sorry for the inconvenience, but this is an experimental and under-development package!

-   `cmip_download()` now uses `httr::RETRY()` to retry download.
    The `…` arguments that where passed to `download.file()` now are silently ignored.

-   Added a `NEWS.md` file to track changes to the package.
