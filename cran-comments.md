## R CMD check results

* Update patch - solves an intermittent CRAN check issue related to the vignette run time.

No Error or Warnings.

One NOTE found:

* lastMiKTeXException Note:

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can be ignored.
