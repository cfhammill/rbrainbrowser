RBrainBrowser
-------------

An R interface to the
[brainbrowser](https://github.com/aces/brainbrowser) javascript library.

Example:

    devtools::install_github("cfhammill/rbrainbrowser")

    library(rbrainbrowser)
    library(jsonlite)

    obj_file <- fromJSON(gzcon(url("https://github.com/aces/brainbrowser/raw/master/examples/models/dbs-vat.json.gz")))

    brainbrowser(obj_file)

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)
