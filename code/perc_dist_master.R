# -----------------------------------------------------------------------------
# Master File
# 
# Perceiving Distances and Construing Targets Based on Communication 
# Chris Arnold, Cardiff University
# February 2024
# -----------------------------------------------------------------------------

# Short sessionInfo()
# (see full specs in the end of the document)
# R version 4.3.0 (2023-04-21)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 14.2.1

# -- Housekeeping --------------------------------------------------------------
start_time <- Sys.time()

# Packages
library(fastDummies)
library(sp)
library(here)
library(openxlsx)
library(readxl)
library(stargazer)

# Visuals
library(scales)
library(viridis)
library(viridisLite)
library(grDevices)
library(leaflet)

# Text Processing
library(stringr)
library(qdapRegex)
library(doc2concrete)
library(xml2)
library(spacyr)
# Note for implementation of spacy and its use via spacyr:
# spacy_install() does not necessarily work. Better construct by hand:
# create an environment for spacy that is called 'spacy_condaenv'
# > conda create -n 'spacy_condaenv'
# there, install spacy:
# > conda install -c conda-forge spacy 
# then also install en_core_web_sm in the environment:
# > python -m spacy download en_core_web_sm


source('utils/annotate_concreteness.r')
setwd(here())
run <- TRUE


# -- Study 1: The Level of Agency in Time Dependent Construals -----------------
if (run){
  source("exp_1_BIF_task_paper_production.r")  
}


# -- Study 2: Time-Dependent Construal in Language -----------------------------
if (run){
  source("exp_2_task_description.r")  
}

# -- Study 3: Distance, Resolution and Concreteness in NYT Articles ------------
# 
if (run){
  source("exp_3_NYT_corpus.r")
}

# -- Study 0: Meta Study on Time Treatment Annotations -------------------------
if (run){
  source("exp_0_metastudy.r")  
}




# -- Appendix -----------------------------------------------------------------
# Measure runtime
end_time <- Sys.time()
cat("
Now all analyses done.
Runtime of the script on our system was about 3.65 mins.
Full runtime of the script on your system:", 
round((end_time - start_time), 2), 'minutes')



# Long sessionInfo()

# 
# R version 4.3.0 (2023-04-21)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 14.2.1
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/London
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] spacyr_1.2.1       xml2_1.3.4         doc2concrete_0.5.6 qdapRegex_0.7.5   
#  [5] stringr_1.5.0      leaflet_2.2.1      viridis_0.6.3      viridisLite_0.4.2 
#  [9] scales_1.2.1       stargazer_5.2.3    readxl_1.4.2       openxlsx_4.2.5.2  
# [13] here_1.0.1         sp_2.1-3           fastDummies_1.6.3 
# 
# loaded via a namespace (and not attached):
#  [1] fastmatch_1.1-3      gtable_0.3.3         shape_1.4.6          ggplot2_3.4.2       
#  [5] htmlwidgets_1.6.4    quanteda_3.3.1       lattice_0.21-8       vctrs_0.6.2         
#  [9] tools_4.3.0          crosstalk_1.2.1      generics_0.1.3       parallel_4.3.0      
# [13] tibble_3.2.1         fansi_1.0.4          sylly.en_0.1-3       pkgconfig_2.0.3     
# [17] Matrix_1.5-4.1       data.table_1.14.8    RcppParallel_5.1.7   lifecycle_1.0.3     
# [21] compiler_4.3.0       munsell_0.5.0        codetools_0.2-19     carData_3.0-5       
# [25] SnowballC_0.7.1      htmltools_0.5.7      glmnet_4.1-7         pillar_1.9.0        
# [29] car_3.1-2            jquerylib_0.1.4      lexicon_1.2.1        koRpus.lang.en_0.1-4
# [33] koRpus_0.13-8        iterators_1.0.14     abind_1.4-5          foreach_1.5.2       
# [37] textstem_0.1.4       stopwords_2.3        tidyselect_1.2.0     zip_2.3.0           
# [41] digest_0.6.31        stringi_1.7.12       slam_0.1-50          dplyr_1.1.2         
# [45] splines_4.3.0        rprojroot_2.0.3      fastmap_1.1.1        grid_4.3.0          
# [49] colorspace_2.1-0     cli_3.6.1            magrittr_2.0.3       survival_3.5-5      
# [53] utf8_1.2.3           withr_2.5.0          english_1.2-6        sylly_0.1-6         
# [57] reticulate_1.28      gridExtra_2.3        cellranger_1.1.0     png_0.1-8           
# [61] NLP_0.2-1            tm_0.7-11            rlang_1.1.1          syuzhet_1.0.6       
# [65] Rcpp_1.0.10          glue_1.6.2           jsonlite_1.8.4       rstudioapi_0.14     
# [69] R6_2.5.1            
