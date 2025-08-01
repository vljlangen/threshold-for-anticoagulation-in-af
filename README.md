# Analysis code for the article "What is the stroke risk threshold for anticoagulation to provide net benefit in atrial fibrillation? A Markov decision model analysis"

A comprehensive decision-analytic model examining direct oral anticoagulant (DOAC) therapy in patients with atrial fibrillation, focusing on treatment thresholds and net benefit analysis across varying stroke risk levels.

## Project Overview

While randomized trials have clearly demonstrated the benefits of anticoagulant therapy in atrial fibrillation (AF) patients at high risk of ischemic stroke, less is known about the benefit in lower-risk patients. This project addresses the critical question: exactly how low baseline stroke risk justifies attempts to reduce it with oral anticoagulants?

This study develops a Markov decision model to estimate the impact of initiating direct oral anticoagulants (DOACs) on quality-adjusted life-years (QALYs) in AF patients across a range of non-anticoagulated ischemic stroke risk levels. The analysis includes:

- **Main analysis** for 60, 70, and 80-year-old patient cohorts
- **Probabilistic sensitivity analysis** with Monte Carlo simulations
- **Tipping point analysis** to identify treatment thresholds
- **Comprehensive figure generation** for manuscript publication

## Project Structure

### Main Analysis Scripts
- `01-main-analysis-70-year-olds.R` - Primary analysis for 70-year-old cohort
- `02-main-analysis-60-year-olds.R` - Analysis for 60-year-old cohort  
- `03-main-analysis-80-year-olds.R` - Analysis for 80-year-old cohort

### Sensitivity Analysis
- `04-probabilistic-sensitivity-analysis-simulation.R` - Monte Carlo simulations
- `05-merge-probabilistic-sensitivity-analysis-results.R` - Results aggregation

### Tipping Point Analysis (60, 70, and 80-year-olds)
- `06-tipping-point.R` - Treatment threshold analysis for 70-year-old cohort
- `07-tipping-point-60-year-olds.R` - Treatment threshold analysis for 60-year-old cohort
- `08-tipping-point-80-year-olds.R` - Treatment threshold analysis for 80-year-old cohort

### Figure Generation
- `09-figure-1.R` - Main manuscript Figure 1
- `10-figure-S1.R` - Supplementary Figure S1
- `11-figure-S2.R` - Supplementary Figure S2
- `12-figure-S3.R` - Supplementary Figure S3
- `13-figure-S4.R` - Supplementary Figure S4
- `14-figure-S5.R` - Supplementary Figure S5
- `15-figure_S6.R` - Supplementary Figure S6
- `16-figure_S7.R` - Supplementary Figure S7


## Getting Started

### Prerequisites

This project requires R with the following packages:
- `ggplot2` - Data visualization
- `dplyr` - Data manipulation
- `showtext` - Font management for plots
- `magick` - Image processing
- `grid` - Graphics layout
- `gt` - Table generation
- `ggthemes` - Additional plot themes
- `pacman` - Package management

### Installation

1. Clone this repository
2. Install required packages:
```r
install.packages("pacman")
library(pacman)
p_load(ggplot2, dplyr, showtext, magick, grid, gt, ggthemes)
```

### Running the Analysis

#### Complete Analysis Pipeline
Run scripts in numerical order for full analysis:

1. **Main Analyses** (01-03): Generate core results for each age group
2. **Sensitivity Analysis** (04-05): Perform probabilistic sensitivity analysis
3. **Tipping Point Analysis** (06-08): Calculate treatment thresholds for all age groups
4. **Figure Generation** (09-16): Create all manuscript figures

#### Individual Components
Each script can be run independently if specific outputs are needed.

## Key Outputs

### Main Figures
- **Figure 1**: Cumulative QALYs by annual stroke risk (0–3%) for 70-year-olds, highlighting the tipping point for DOAC therapy.

### Supplementary Figures
- **Figure S1**: Markov model structure — Health states and transitions.
- **Figure S2**: Major bleeding and mortality rates by stroke risk.
- **Figure S3**: Cumulative QALYs by annual stroke risk (0–10%) for 70-year-olds (full risk spectrum).
- **Figure S4**: Event composition (death, stroke, bleeding) after 20 years by stroke risk.
- **Figure S5**: Cumulative life years without severely disabling events by annual stroke risk.
- **Figure S6**: Probability that DOAC therapy leads to more QALYs (probabilistic sensitivity analysis).
- **Figure S7**: Mean 20-year cumulative QALYs by annual stroke risk (probabilistic sensitivity analysis).

### Key Metrics
- **Treatment Tipping Points**: Annual non-anticoagulated ischemic stroke risk thresholds where DOAC therapy provides net benefit
- **QALY Differences**: Quality-adjusted life year gains/losses comparing DOAC therapy vs. withholding treatment
- **Probability Analysis**: Likelihood of treatment benefit across varying stroke risk levels in sensitivity analyses
- **Risk-Stratified Outcomes**: Benefits quantified across different baseline stroke risk categories

## Analysis Methodology

The analysis employs a **Markov decision model** incorporating:

### Data Sources
- **Randomized controlled trial data** on DOAC effects on:
  - Severity and risk of ischemic stroke
  - Major bleeding events
  - Mortality outcomes
- **Quality of life impact data** from previous evidence
- **Non-anticoagulated event rates** averaged from observational studies

### Statistical Methods
- **Markov modeling** for long-term outcome projection across AF patient populations
- **Monte Carlo simulation** for uncertainty analysis
- **Probabilistic sensitivity analysis** with 1,000 iterations to account for uncertainty in treatment effects
- **Linear regression** for tipping point estimation
- **Quality-adjusted life years (QALYs)** as primary outcome measure

### Clinical Focus
- **Treatment threshold identification** for when DOACs provide net benefit
- **Risk-stratified analysis** across varying non-anticoagulated ischemic stroke risk levels
- **Age-specific modeling** for 60, 70, and 80-year-old patient cohorts

## Research Keywords

Atrial fibrillation • Anticoagulation • Stroke risk • Treatment threshold • Net benefit • Quality of life • DOACs • Decision analysis

## Citation

If you use this code or methodology, please cite the associated manuscript or the Zenodo page.

## Contributors

- Winstén, Aleksi
- Langén, Ville
- Teppo, Konsta

## License

MIT

## Related Resources

- [Manuscript (in preparation)](TBD)
- [Zenodo repository (DOI: 10.5281/zenodo.14941360)](https://doi.org/10.5281/zenodo.14941360)

---

*Last updated: July 1, 2025* 