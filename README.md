
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Context

Consider multivariate numeric data with the goal of applying machine
learning. `Trees of Cheem` applies tree based models and explores the
space around the “shapely values” (see [Explanatory Model Analysis,
SHAP](https://ema.drwhy.ai/shapley.html#SHAPRcode)) of a new, out of
sample, observation. To do so we want to view a *tour*, an animation of
linear projections as the projection basis changes to nearby frames.

In terms of workflow and model specificity:

<img src="./images/cheem_workflow.png" width="654" />

## Repo Structure

The case studies that produce the videos and still images found in the
paper can be reproduced in R via running the files found in the
`scripts` directory.

    R> .
    R> +-- apps
    R> |   \-- poc
    R> |       +-- app.r
    R> |       +-- ggproto_screeplot_pca.r
    R> |       +-- rsconnect
    R> |       |   \-- shinyapps.io
    R> |       |       \-- ebsmonash
    R> |       |           \-- ML_EDA__PoC.dcf
    R> |       +-- trees_of_cheem.r
    R> |       +-- ui.r
    R> |       \-- www
    R> |           \-- ML_EDA.PNG
    R> +-- images
    R> +-- LICENSE
    R> +-- README.md
    R> +-- README.rmd
    R> +-- trees_of_cheem.Rproj
    R> +-- zDalex
    R> |   \-- sources.r
    R> +-- zDesignDrafts
    R> |   +-- EMA.PNG
    R> |   +-- MEDAL pitch and sketch.pptx
    R> |   +-- ML_EDA.PNG
    R> |   +-- ML_EDA_sideinfo.PNG
    R> |   +-- r4ds.PNG
    R> |   +-- Trees_of_Cheem_pitch.pptx
    R> |   \-- ~$Trees_of_Cheem_pitch.pptx
    R> \-- zDev
    R>     +-- PPforest_vignette.r
    R>     +-- zbenchmarking.r
    R>     +-- zbenchmarking_est_idd.r
    R>     +-- zbenchmarking_rbind.r
    R>     +-- zwithSpinner.r
    R>     +-- z_feeze_tourr.r
    R>     \-- z_feeze_tourr_reprex.r

## Namesake

<img src="./images/cheem_namesake.png" width="385" />
