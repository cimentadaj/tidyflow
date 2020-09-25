
## To add a new presentation

* Create a new folder with the name of the presentation. 

* Put the `Rmd` file inside the folder and change the name of the `Rmd` to have a `_` in front (this makes sure that `build_site` ignores it; we don't want it to be rebuilt)

* `knit` the presentation to produce an `html`. 

* Once you have your final `html`, open the `html` in Chrome/Chromium and press `CTRL-SHIT-p`. This prompts you to save the presentation as `pdf`. Save the presentation but as a `.pdf`. Note that this doesn't have to have the `_` in front.

* Add an option to the `Rmd` file to `eval = FALSE` all of the code chunks. This makes sure that if the presentation takes long, it's not knitted every time we run `pkgdown`. In any case, the website will only grab all `pdf` files, so it doesn't matter that it rewrites the `html` file.

* Add the presentation `whatever.pdf` to `_pkgdown.yaml`


## To rerun a presentation

* Go to the `Rmd` file and exclude the `eval = FALSE` option from `knitr` options.

* Knit again and the `html` should be up to date.

* Remember to update the `pdf` if you changed something with the indications from above.
