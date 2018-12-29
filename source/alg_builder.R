#######################################################################################

##################            BUILDER              ####################################

#######################################################################################

if (require(pacman) == F) install.packages("pacman")
library("pacman")
p_load("rPref", "plotly", "xgboost", "mlr" ,'EMP', 'parallelMap' )

cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(cd))


code.main <- "alg/source/main"
code.selection <- "alg/source/selection"



source(file.path(code.main, "initial_population.R"))
source(file.path(code.main, "crossover_and_mutation.R"))
source(file.path(code.main, "evaluation.R"))

source(file.path(code.selection, "normalisation.R"))
source(file.path(code.selection, "reference_points.R"))
source(file.path(code.selection, "niching.R"))
source(file.path(code.selection, "next_generation_selection.R"))

source(file.path(code.main, "output_preparation.R"))
source(file.path(code.main, "algorithm_wrap.R"))



