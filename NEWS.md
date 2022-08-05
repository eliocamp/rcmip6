# rcmip6 0.0.1.9000

## Breaking changes

-   Now `cmip_search()` returns a data.frame instead of a list.
    Therefore, the `as.data.frame()` method is deprecated.
    To get the same result, use `cmip_simplify()`, which just removes a lot of potentially unnecessary columns.

-   The information of downloaded files is now saved in a different format which is not compatible with previous versions.
    Therefore, `cmip_available()` will fail (it failed anyway due to a bug).
    There currently no way of migrating to the new format, but if you run your previous searches, `cmip_download()` will skip already-downloaded files and update the information.
    This way of storing information about available files will most likely change, since it's very inefficient for large datasets of thousands of files.
    Sorry for the inconvenience, but this is an experimental and under-development package!

-   Added a `NEWS.md` file to track changes to the package.
