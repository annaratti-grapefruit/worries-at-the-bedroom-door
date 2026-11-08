## Overview

Self-reported and actigraphic sleep measures may capture partly distinct aspects of sleep. This study examined whether pre-sleep arousal (PSA) relates to perceived sleep quality beyond actigraphic sleep efficiency (SE), and to the discrepancy between the two measures.

- **Design:** ecological momentary assessment (EMA) combined with wrist actigraphy, for 14 nights.
- **Sample:** 74 young adults, 944 nights with complete data.
- **Measures:**
  - perceived sleep quality (`EMAq`, 0–8), rated in the morning;
  - pre-sleep arousal of the previous evening (six items, 1–5; cognitive and somatic subscales), rated in the morning;
  - actigraphic sleep efficiency (`SE`, %).
- **Analysis:** linear mixed models with a random intercept per participant. Predictors are split into within-person (`_w`, night vs. the participant's usual level) and between-person (`_b`, participant vs. the sample) components. Models are compared with likelihood ratio tests and Akaike weights.


The script is organised in eight sections:

1. Data preparation (morning EMA and actigraphy, matched by participant and study day)
2. Within- / between-person decomposition and perceived–actigraphic discrepancy index, z(EMAq) − z(SE)
3. Descriptive statistics
4. Agreement between perceived sleep quality and SE (within- and between-person correlations)
5. Model comparison:

   | Model | Fixed effects |
   |---|---|
   | M0 | null model |
   | M1 | SE |
   | M2 | SE + PSA |
   | M3 | SE + cognitive and somatic PSA |
   | M4 | PSA only |

6. Coefficients of the selected model (M2, REML)
7. Standardised effects of PSA on perceived sleep quality, SE and their discrepancy
8. Figures

## Data availability

The data are not included in this repository because they contain individual participant information. [TO BE SPECIFIED: data availability statement, e.g. available from the authors upon reasonable request]

The script expects two Excel files, whose paths are set at the top of the script (`file_ema`, `file_act`):

- **EMA file:** one row per EMA prompt.
  - Required columns: `ID`, `DayNumber`, `time`, `survey`, `survey_completed`, `SLEEPQ`, and the arousal items `AR_EVEN`, `AR_OFF`, `AR_THOU` (cognitive) and `AR_MUS`, `AR_JITT`, `AR_BREA` (somatic).
- **Actigraphy file:** sheet `EMA_act_baseline`, one row per participant × study day.
  - Required columns: `ID`, `DayNumber`, `SE`, `age`, `gender`, `PSQI_Totale`.




