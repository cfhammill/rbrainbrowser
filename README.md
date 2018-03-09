RBrainBrowser
-------------

An R interface to the
[brainbrowser](https://github.com/aces/brainbrowser) javascript library.

Example:

    devtools::install_github("cfhammill/rbrainbrowser")

    library(rbrainbrowser)
    library(jsonlite)

    obj <-
      fromJSON(
        gzcon(
          url("https://github.com/aces/brainbrowser/raw/master/examples/models/dbs-vat.json.gz"))
      , simplifyDataFrame = FALSE)

    brainbrowser(obj_file)
