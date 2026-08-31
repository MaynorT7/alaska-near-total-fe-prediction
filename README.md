# Spatial Prediction of Near-Total Soil Iron Concentration in Western Alaska

Random-forest digital soil mapping of near-total soil iron (Fe) concentration
across a permafrost-affected region of western Alaska (60–63° N, 165–156° W),
using legacy geochemical observations from the USGS Alaska Geochemical Database
Version 4.0. Four cumulative predictor specifications are evaluated on the same
comparison cohort under three five-fold cross-validation (CV) designs; model
selection is based solely on pooled spatially blocked out-of-fold (OOF) R².


## Repository structure

```text
alaska-near-total-fe-prediction/
├── R/
│   ├── data_preprocessing.R                 — data loading, QC and covariate construction
│   ├── create_spatial_folds.R               — shared spatial, grid-grouped and observation-level folds
│   ├── model_config.R                       — shared configuration, feature sets and paths
│   └── model_functions.R                    — random-forest fitting, evaluation and prediction
├── Model_V1/
│   └── Model_V1.R                           — V1 base specification
├── Model_V2/
│   └── Model_V2.R                           — V2 environmental increment
├── Model_V3/
│   └── Model_V3.R                           — V3 geological increment and selected specification
├── Model_V4/
│   └── Model_V4.R                           — V4 fine-terrain and drainage increment
├── preprocess_all.R                         — staged preprocessing and shared-fold orchestration
├── compare_models.R                         — paired model comparison and CSV export
├── supplementary_analysis_and_figures.R     — figures, numerical verification and Appendix B
├── data/                                    — local source and generated data; not tracked
├── README.md
└── .gitignore
```

The directory name created by cloning the repository is not used by the code.
Project discovery is based on the repository structure.


## Requirements

R ≥ 4.3 with the following packages:

`readr`, `dplyr`, `tidyr`, `sf`, `ggplot2`, `terra`, `ranger`, `blockCV`,
`RANN`, `scales`, `cowplot` and `systemfonts`.

`knitr` and `rmarkdown` are required only to render the optional HTML audit.
Times New Roman must be installed to reproduce the methodology diagrams in
their specified typeface.


## Source data

Third-party source datasets are not distributed with this repository.

| Dataset | Role |
|---|---|
| Alaska Geochemical Database Version 4.0 | Soil, sediment and rock geochemistry |
| SIM 3340 geodatabase | 1:1,600,000 geological map units |
| GMTED2010 and USGS 3DEP 60 m DEM | Terrain derivatives |
| Daymet V4, 1981–2010 | Mean annual air temperature and precipitation |
| Pastick et al. (2015) | Near-surface permafrost probability |
| USGS 3D Hydrography Program | Channel-line drainage proximity |

Place the required files under `data/` using the filenames and subdirectories
defined by `project_input_paths()` in `R/model_config.R` and by the
input-discovery functions in `R/data_preprocessing.R`. The complete `data/`
tree is deliberately excluded from version control.


## Execution order

Run the workflow from the repository root:

```bash
# 1. Preprocessing, cohort definition and shared fold construction
Rscript preprocess_all.R

# 2. Model fitting; each specification uses the saved cohort and folds
Rscript Model_V1/Model_V1.R
Rscript Model_V2/Model_V2.R
Rscript Model_V3/Model_V3.R
Rscript Model_V4/Model_V4.R

# 3. Paired comparison and CSV export
Rscript compare_models.R

# 4. Figures, numerical verification and Appendix B
Rscript supplementary_analysis_and_figures.R
```

The four model-fitting scripts are order-independent once preprocessing has
completed. `compare_models.R` must be run after all four model outputs exist,
and the supplementary script must be run last.


## Supplementary analysis and figures

`supplementary_analysis_and_figures.R` consolidates the figure-generation and
numerical-audit workflow supporting Figures 3.1–3.3 and 4.1–4.5, the reported
Results and Discussion values, and Appendix B. It reads the fixed comparison
cohort, saved fold assignments, processed artefacts and V1–V4 outputs generated
by the main workflow; it does not redefine the fitted candidate models.

It can also be sourced within R:

```r
source("supplementary_analysis_and_figures.R")
```

An optional HTML audit can be generated with:

```r
knitr::spin(
  "supplementary_analysis_and_figures.R",
  knit = FALSE
)

rmarkdown::render(
  "supplementary_analysis_and_figures.Rmd"
)
```

All paths are resolved relative to the repository root. No path configuration
is required when the script is run from within the repository. If it is run
externally and multiple valid local copies are discoverable, select the intended
repository for the current R session before sourcing the script:

```r
options(
  alaska_fe.project_root = normalizePath(
    "path/to/local/repository",
    winslash = "/",
    mustWork = TRUE
  )
)
```

This option is session-specific and does not modify the project files. It can
be removed with:

```r
options(alaska_fe.project_root = NULL)
```

Generated artefacts are written to:

- `supplementary/Methodology/`
- `supplementary/Results/`
- `supplementary/Discussion/`
- `supplementary/Appendix_B/`

Figure 4.2 reports pooled log₁₀-scale R² for V1–V4 under all three five-fold CV
designs. This presentation does not alter the pre-specified selection rule:
model selection uses only pooled spatially blocked OOF R²; grid-grouped and
observation-level random CV serve to contrast validation tasks.

The two reported Appendix B items are:

- **Table B1:** fold-wise spatial CV composition and performance, recalculated
  from the saved V3 spatial OOF predictions.
- **Figure B1:** cell-mean signed spatial OOF error, defined as predicted minus
  observed (`ŷ − y`), for occupied 1-km cells.

Positive signed error denotes overprediction. Figure B1 is a descriptive
diagnostic of the saved spatial OOF predictions, not an independent spatial
validation. Its machine-readable plotting data are exported for reproducibility
but do not constitute an additional Appendix item.


## Temporary-file configuration

By default, disk-backed `terra` temporary files are written to the R-session
temporary directory. When a persistent location is required, set
`ALASKA_TERRA_TEMP` for the R session used to run preprocessing:

```r
Sys.setenv(
  ALASKA_TERRA_TEMP = "path/to/terra-temp"
)

source("preprocess_all.R")
```

This optional setting changes only the temporary-file location; it does not
change the analytical inputs or model specification.


## Reproducibility

All model fits use a fixed seed (`seed = 40`), single-threaded `ranger`
execution (`num.threads = 1`) and saved fold assignments. Given the same source
datasets, input structure, R version and package versions, the workflow
reproduces the reported comparison metrics; `compare_models.R` writes these
metrics to `model_comparison.csv`.
