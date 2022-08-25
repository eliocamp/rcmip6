# rcmip6 0.0.1.9000

## New Features

-   To speed up downloads, `cmip_download()` will try to read the checksum of each file from a previously-saved file with extension `.chksum` and it will create it if it doesn't exist.
-   `cmip_download()` now doesn't stop after a failed download. Instead it throws a warning and continues with the next files.
-   `cmip_search()` now allows to search for replicas.
-   `filter_replicas()` will check online status of the datanodes and return a single replica per result that's online.
-   `cmip_search()` now turns most columns into vector columns instead of list columns.
-   `cmip_search()` accepts a character vector query. It will be interpreted as a list of instances.

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
