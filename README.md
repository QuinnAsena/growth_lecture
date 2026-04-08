# Population Growth Lecture

Teaching materials for an introductory lecture on population ecology covering exponential growth, logistic (density-dependent) growth, and stochastic population dynamics.

## Contents

- **`pop_growth.qmd`** — Lecture slides as a [Quarto](https://quarto.org/) revealjs presentation covering:
  - Part 1: Exponential (density-independent) growth
  - Part 2: Logistic (density-dependent) growth
  - Part 3: Stochastic logistic growth
- **`growth_app/`** — An interactive [Shiny](https://shiny.posit.co/) application for exploring discrete population growth models (exponential, logistic, and stochastic).
- **`pop_growth_files/`** — Images used in the lecture slides.

## Requirements

- [R](https://www.r-project.org/) (≥ 4.0)
- [Quarto](https://quarto.org/) (for rendering the slides)
- R packages: `tidyverse`, `shiny`, `ggplot2`, `data.table`, `knitr`

## Rendering the slides

```bash
quarto render pop_growth.qmd
```

## Running the Shiny app

```r
shiny::runApp("growth_app")
```

Or open `growth_app/app.R` in RStudio and click **Run App**.

## License

See [LICENSE](LICENSE) for details.
