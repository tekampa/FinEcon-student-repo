# FinEcon Student Repo

This repository has guides and code to complement course material for ECON 3025 (Financial Economics).

## Setup (do this once)

1. **Clone or download** this repository. Keep the folder structure intact.
2. **Install R packages** (one time, in the R console):
   ```r
   install.packages(c("tidyverse", "here", "scales", "assertthat"))
   ```
3. **Open the project so R's working directory is inside the repo.** Pick one:
   - **VS Code (recommended):** open the `FinEcon-student-repo` folder as your workspace.
   - **RStudio:** double-click `FinEcon-student-repo.Rproj`. RStudio
     opens with the working directory set to the repo root automatically.
   - **Plain R / terminal:** start R from inside the repo folder.

With that set, every script uses the [`here`](https://here.r-lib.org) package to find
files relative to the repo root, so paths work no matter which sub-folder you run
from. You never edit file paths.

> If you get `Error: Could not find associated project...`, your R session started
> **outside** the repo. Reopen via opening the folder first in VS Code or the `.Rproj` and re-run.

## Layout

```
teaching-materials/
  scripts/   R scripts that build the figures used in lecture
  data/      input data (CSV; sourced from public data, e.g. SIFMA)
  output/    figures written by the scripts (PNG/PDF)
bond-assignment/       your mini-project work (bonds)
portfolio-assignment/  your mini-project work (stocks/portfolio)
```

## Running a script

From the opened project, run e.g.:
```r
source("teaching-materials/scripts/AverageDailyVolumeTrading_byAssetClass.r")
```
It reads from `teaching-materials/data/`, writes figures to
`teaching-materials/output/`, and prints where it saved them.
