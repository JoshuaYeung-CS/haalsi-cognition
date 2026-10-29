# Money, medicine or therapy? Cognitive impairment in rural South Africa

My analysis for the HAALSI data hackathon (October 24, 2026), drafted before teams were assigned and used as the base of our presentation. The question: which of three
interventions (cash transfers, hypertension treatment or psychotherapy for depression) could do
the most to reduce cognitive impairment among older adults in Agincourt, South Africa?

**Answer in one line:** household wealth shows a steady gradient with cognition (OR 0.70 per SD
of assets, 95% CI 0.63 to 0.78), while the apparent benefit of hypertension treatment is mostly
selection: the people who get treated differ from those who don't before treatment starts.

## What is here

| File | What it is |
|---|---|
| `HAALSI_hackathon_joshua.Rmd` | The full analysis in R: data cleaning, Wave 1 baseline, the Q1 models, scenarios and robustness checks |
| `HAALSI_hackathon_joshua.html` | The knitted report |
| `q1_extras.R` | Scenario table (what each intervention could achieve) and robustness checks (E-value, extra controls) |
| `HAALSI_brief.pdf` / `.tex` | One-page brief of the findings |
| `slides/HAALSA_Q1_deck.pptx` | The 9-slide presentation |
| `slides_fig/` | Figures used in the deck |

## Methods in brief

- Outcome: cognition score (orientation plus word recall), with "low cognition" as the bottom 20% at Wave 1.
- Wealth: asset-index quintiles and per-SD odds ratios. BMI, hypertension and depression are left
  out of the wealth model because they sit on the pathway, not before the exposure.
- Hypertension: treated vs untreated compared with untreated vs normotensive, to separate a
  treatment effect from who gets treated.
- Sensitivity: E-value for unmeasured confounding (1.67 for the wealth gradient) and models with extra controls.
- Change over waves is read with care: practice effects and regression to the mean dominate raw change scores.

## Reproducing it

The data are not in this repository. HAALSI is distributed by Harvard Dataverse under a data
use agreement that does not allow redistribution. Request access at
<https://doi.org/10.7910/DVN/Q2JFPV>, put `HAALSI_W1-3_Longitudinal_Dataset.dta` in this folder,
then knit:

```r
rmarkdown::render("HAALSI_hackathon_joshua.Rmd")
```

Packages: dplyr, tidyr, ggplot2, haven, broom, lme4, lmerTest, knitr.

## Data citation

HAALSI (Health and Aging in Africa: A Longitudinal Study of an INDEPTH Community in South Africa)
is sponsored by the National Institute on Aging (P01 AG041710) and conducted by the Harvard
Center for Population and Development Studies in partnership with the MRC/Wits Rural Public
Health and Health Transitions Research Unit (Agincourt). Waves 1 to 3, Harvard Dataverse,
doi:10.7910/DVN/Q2JFPV.
