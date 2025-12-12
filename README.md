# Development and validation of EWS systems 🚑

## Installation

Install [pixi](https://pixi.sh/latest/installation) to install the dependencies
necessary to run the project.

Once pixi is installed, clone the repository and run the following inside the
project directory:

```bash
# Install all pixi-friendly dependencies
pixi install
# Install missing R dependencies
pixi run post_install
```

To run the python notebooks, it is recommended to install cuda 12.6. After
installation, verify GPU availability in PyTorch:

```python
import torch
print(torch.cuda.is_available())
```

## Contents

All main scripts can be found in the `pipeline` directory:

<!-- prettier-ignore -->
| Script | Description | Command |
|--------|-------------|---------|
| `preprocessing.R` | Initial pre-processing of Electronic Health Records consisting of early warning score measurements and vital signs for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023. | `pixi run preprocessing` |
| `extract_metadata.R` | Addition of other clinical data, consisting of procedures, diagnoses, blood tests, and ITA information. | `pixi run extract_metadata` |
| `extract_embeddings.py` | Addition of text embeddings from the metadata using static embeddings. | `pixi run extract_embeddings` |
| `analysis_main.R` | Comparison of various models and algorithms for early warning systems:<br>• Implementation of the weighting model (CBPS) for the individuals<br>• 🔗 [NEWS](https://www.england.nhs.uk/ourwork/clinical-policy/sepsis/nationalearlywarningscore) (National Early Warning Score)<br>• 🔗 Simplified NEWS: NEWS2 - Blood Pressure - Temperature<br>• 🔗 [DEWS](http://doi.org/10.1097/CCM.0000000000005842) (Demographic Early Warning Score): Simplified NEWS + Age + Sex<br>• 🔗 XGB-EWS: Age + Sex + Vital Signs + Number of Previous Hospitalizations + Embeddings of Previous Medical Procedures and Diagnoses + historical averages of blood test values + time-related recording information<br>• Grouped Cross-Validation based on hospitals<br>• AUC, Brier Score, Calibration, Net Benefit (Differences) | `pixi run analysis_main` |
| `analysis_composite_outcome.R` | Analysis of composite outcomes (ICU + Death). | `pixi run analysis_composite` |

### EWS models

- [XGBoost](https://xgboost.readthedocs.io/en/stable)
- [Logistic regression](https://parsnip.tidymodels.org/reference/logistic_reg.html)

### Embedding models

- Static embeddings of medical procedures/diagnoses trajectories using
  [model2vec](https://github.com/MinishLab/model2vec)'s
  [potion-multilingual-128M](https://huggingface.co/minishlab/potion-multilingual-128M)
  model
- Logistic regression for Covariate Balancing Propensity Score (CBPS) using the
  [weightit](https://ngreifer.github.io/WeightIt) R package

## Summary

- Assessment of NEWS current system based on predictive performance metrics
  using data-splitting techniques ✅.

- De-biasing the dataset with IPW (Inverse Probability Weighting) based on
  intervention scenarios ✅

- Development of alternative early warning score systems and model comparison ✅

- Outcome: 24-hour mortality prediction after initial NEWS score ✅

- Used scores: Initial score at admission ✅

- Assess calibration and net benefit on various strata of target population ✅
