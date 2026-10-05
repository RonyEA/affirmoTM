# affirmoTM tutorials

Six short tutorials, each a PDF that can be read on its own. Start with the first; the others can be read in any
order.

| Tutorial | What it covers |
|---|---|
| [1. Getting started](1-getting-started.pdf) | What the model does, the health states, installing affirmoTM, an analysis folder, and a first run |
| [2. Input files](2-input-files.pdf) | Each input file: its layout, its rules, and how to look at it; values that change over time, and costs and utilities that differ between patients |
| [3. Settings](3-settings.pdf) | Each setting in `settings.yaml`, and the effect of changing them |
| [4. Running the model](4-running.pdf) | What `tm_run()` does, and the steps of a run one at a time |
| [5. Results](5-results.pdf) | The results object, the comparison and its quadrant, the cost-effectiveness plane, and the output files |
| [6. Walkthrough](6-walkthrough.pdf) | One run followed in detail: five patients year by year, their costs and QALYs worked out by hand, and how the two arms behave over time |

Every tutorial uses the example input files that come with the package, so running its code gives the same results.

## Rebuilding the PDFs

Each PDF is made from the `.qmd` file of the same name. From the top of the repository:

```sh
quarto render tutorials/1-getting-started.qmd
```

This needs [Quarto](https://quarto.org), a LaTeX installation (for example `quarto install tinytex`), and the R
packages `devtools`, `rprojroot`, `knitr`, `data.table` and `ggplot2`. The tutorials load affirmoTM from the
repository, so they always show the current version of the package.
