# How Bad Can Informative Missing Values Be?
### A Sensitivity Analysis of MNAR Data in Predictive Modeling

## Overview
This repository presents a simulation-based case study investigating the impact of **Missing Not At Random (MNAR)** mechanisms on predictive performance. The central research question is:

> *How bad can informative missing values be, and under what conditions does standard imputation fail or succeed?*

Using a Monte Carlo framework grounded in a real student performance dataset, we evaluate how predictive degradation depends on:
- the **importance of the missing feature**, and
- the **intensity of missingness**.

We further assess whether **domain-informed sensitivity analysis (delta adjustment)** can recover lost signal—and how incorrect assumptions can worsen model bias beyond the missingness itself.

---

## Key Contributions
- Simulation framework for controlled MNAR experiments
- Feature-importance-aware missingness scenarios
- Empirical comparison of PMM vs. sensitivity-based delta adjustment
- Quantification of risk from mis-specified MNAR assumptions

---

## Provenance & Ethics Statement
⚠️ **Important note on authorship and provenance**

This repository is a **cleaned and restructured case study** derived from a collaborative group project on informative missingness (MNAR).  
All refactoring, organization, interpretation, and documentation in this repository reflect my **personal work and understanding**.

The original group repository is acknowledged and will be referenced for transparency.

---

## Repository Structure (to be finalized)
```text
analysis/    # Narrative RMarkdown (paper-style)
R/           # Modular simulation and modeling code
data/        # Seed data or data loaders
figures/     # Generated figures
results/     # Simulation outputs

