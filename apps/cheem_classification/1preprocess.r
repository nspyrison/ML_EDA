## Creates objs for use in shiny app

## Dependencies ------
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
require("tictoc")
require("dplyr")
##
require("palmerpenguins") ## data
require("caret") ## One hot encoding class.
require("plotly") ## Linked brushing
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cheem_classification/spinifex_ggproto.r") ## New spinifex ggproto_* api
if(F){ ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")
  file.edit("./apps/cheem_classification/spinifex_ggproto.r")
}

## Setup ------
## Data setup, palmerpenguins::penguins
raw <- palmerpenguins::penguins ## Missing values, visdat::vis_miss(raw)
raw_rmna <- raw[!is.na(raw$sex), ]
lvls <- levels(raw_rmna$species)
## Filter to closest 2 classes
raw_rmna <- raw_rmna[raw_rmna$species %in% lvls[1:2], ]
# ## Normalize each column by its ROW NORM?!, not applied yet
# scale_row_norm <- function(data){
#   return(t(apply(data, 1L, function(r){r / norm(matrix(r, nrow = 1))})))
# }
dat <- spinifex::scale_sd(raw_rmna[, 3:6]) %>% as.data.frame()
clas1 <- factor(raw_rmna$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
clas2 <- raw_rmna$sex
if(F){
  table(clas1) ## Unbalenced island obs
  table(clas2) ## Balanced sex
}

# ## Additional setup not used in local attribution matrix
# new_obs_x <- rnorm_observation(xdat)

# ## Encode classes {caret}
# require("caret")
# dv <- caret::dummyVars(" ~ .", data = raw_rmna)
# encoding <- data.frame(predict(dv, raw_rmna))
# detach("package:caret", unload = TRUE)
# encoding <- encoding[, c(-7:-10, -13)]
# str(encoding)

## Random forest model {randomForest} -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
.lvls <- levels(clas1)
tic("RF fit")
for(i in 1:length(.lvls)){
  test_i <- as.integer(clas1 == .lvls[i]) ## treeshap needs integer RF, not bool.
  assign(x = paste0(".rf", i),
         randomForest::randomForest(test_i~.,
                                    data = data.frame(test_i, dat),
                                    mtry = .rf_mtry),
         envir = globalenv()
  )
}
toc()


## shap_df {treeshap} ------
gc()
tic("treeshap")
for(i in 1:length(.lvls)){
  .sub <- dat[clas1 == .lvls[i], ]
  .rf <- get(paste0('.rf', i))
  assign(paste0("treeshap", i),
         treeshap_df(.rf, .sub),
         envir = globalenv())
}
shap_df <- rbind(treeshap1, treeshap2)
attr(shap_df, "data") <- dat
toc()

## Create spaces! ------


### nMDS
.n <- nrow(dat)
nmds_dat  <- as.data.frame(MASS::isoMDS(dist(dat))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "nMDS")
nmds_shap <- as.data.frame(MASS::isoMDS(dist(shap_df))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "nMDS")

### PCA
pca_dat  <- as.matrix(dat) %*% spinifex::basis_pca(dat) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "PCA")
pca_shap <- as.matrix(shap_df) %*% spinifex::basis_pca(shap_df) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "PCA")

### oLDA
olda_dat  <- as.matrix(dat) %*% spinifex::basis_olda(dat, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "oLD")
olda_shap <- as.matrix(shap_df) %*% spinifex::basis_olda(shap_df, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "oLD")

### Combine
names(nmds_dat) <- names(nmds_shap) <- names(pca_dat) <- names(pca_shap) <-
  names(olda_dat) <- names(olda_shap) <- c(paste0("V", 1:2), "rownum", "data", "space")
bound_spaces_df <- rbind(nmds_dat, nmds_shap, pca_dat, pca_shap, olda_dat, olda_shap)
beepr::beep(4)

## EXPORT OBJECTS ----
if(F){
  save(raw_rmna,
       clas1, clas2,
       bound_spaces_df,
       file = "1preprocess.RData")
  file.copy("./1preprocess.RData", to = "./apps/cheem_classification/data/1preprocess.RData", overwrite = TRUE)
  file.remove("./1preprocess.RData")
}
if(F)
  load("./apps/cheem_classification/data/1preprocess.RData")


if(F){
  ## Mock-up visual ------
  ## Call in app or local with:
  ## Click select
  require("ggplot2")
  tic("prep ggplot ")
  str(bound_spaces_df)
  .nn <- nrow(bound_spaces_df)
  species <- rep_len(clas1, .nn)
  sex <- rep_len(clas2, .nn)
  
  hk <- bound_spaces_df %>%
    highlight_key(~rownum)
  g <- ggplot(hk, aes(V1, V2, rownum = rownum,
                      color = species, shape = sex)) +
    geom_point() +
    facet_grid(rows = vars(data), cols = vars(space)) +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_discrete(name = "") + ## Manual legend title
    scale_shape_discrete(name = "") ## Manual legend title
  
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
}

## Other Selection options
if(F){
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
  ## CLICK SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = FALSE) %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = 'plotly_click', off = "plotly_doubleclick",
              persistent = TRUE) ## Allow selection of many points?
  ## LASSO SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "lasso") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = 'plotly_selected', off = "plotly_deselect")
}