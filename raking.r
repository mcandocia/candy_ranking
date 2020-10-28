# scraping
#library(plyr)
library(dplyr)
library(scales)
library(reshape2)
library(ggplot2)

# will make sure a list's elements match the levels of
# the data it corresponds to
reorder_list <- function(x, reference_data){
  new_list = list()
  data_levels = levels(reference_data)
  for (level in levels(as.factor(reference_data)))
    new_list[[level]] = x[[level]]
  return(new_list)
}

# calcualtes weights for data based on selected variables and their "true" margins
rake_data <- function(data, variables, true_or_estimated_margins,
                      max_iter=50, truncate_extreme_weights=TRUE, verbose=FALSE){
  weights = rep(1, nrow(data))

  # calculate the sum and proportions of each variable + level in advance
  # and make sure that order matches the same order in the data
  total_margins = list()
  for (variable in variables){
    # remove non-present levels
    for (level in names(true_or_estimated_margins[[variable]])[!names(true_or_estimated_margins[[variable]]) %in% data[,variable,T]]){
      if (verbose){
        print(sprintf('Removing %s from %s list', level, variable))
        #stop()
      }
      true_or_estimated_margins[[variable]][[level]] = NULL
    }
    uvar = unique(data[,variable,T])
    for (level in uvar[!uvar %in% names(true_or_estimated_margins[[variable]])]){
      stop(sprintf('Variable %s is missing from margin data', uvar))
    }

    original_margins = true_or_estimated_margins[[variable]]
    # non-factors cause problems bc they produce no levels
    if (!is.factor(data[,variable]))
      data[,variable] = factor(data[,variable,T])
    reordered_margins = reorder_list(original_margins, data[,variable,T])
    valid_levels = levels(data[,variable])
    #print(variable)
    #print(reordered_margins)

    total_margin =  sum(unlist(reordered_margins))
    total_margins[[variable]] = total_margin
    #print(total_margins)
    for (level in names(true_or_estimated_margins[[variable]])){
      # remove unused levels
      if (!level %in% valid_levels)
        true_or_estimated_margins[[variable]][[level]]=NULL
      else{
        reordered_margins[[level]] =
          reordered_margins[[level]]/total_margin
      }
    }
    # fix for large numbers
    #print(variable)
    #print(reordered_margins)

    true_or_estimated_margins[[variable]]=reordered_margins

  }
  if (verbose)
    print(true_or_estimated_margins)
  # create design matrices (columns of 1s and 0s in this case) for faster calculations
  design_matrices = list()
  for (variable in variables){
    # create model matrix with 0-intercept, which removes the concept of "reference variable"
    design_matrices[[variable]] = as.data.frame(model.matrix(~.+0, data=data[,variable,drop=FALSE]))
    # remove variable name from column name so only level remains
    colnames(design_matrices[[variable]]) = names(true_or_estimated_margins[[variable]])#substr(colnames(design_matrices), 1, nchar(variable))
  }
  if (verbose)
    print(design_matrices)

  # perform raking
  for (i in 1:max_iter){
    if (verbose)
      print(paste('iteration',i))
    for (variable in variables){
      weighted_margins = colSums(weights * design_matrices[[variable]])
      if (verbose){
        print(variable)
        names(weighted_margins) = names(true_or_estimated_margins[[variable]])
        print(weighted_margins)
      }
      level_weight_modifications = unlist(true_or_estimated_margins[[variable]])/weighted_margins
      if (verbose){
        print('Modifications:')
        names(level_weight_modifications) = names(true_or_estimated_margins[[variable]])
        print(level_weight_modifications)
      }

      # this multiplies each column of the design matrices by the corresponding weight change factor
      # then each column is multiplied by the weights, and the rows are added up, since each row only
      # has one non-zero value
      weights = rowSums(weights * mapply(`*`, design_matrices[[variable]], level_weight_modifications))

      #weights = weights*length(weights)/(sum(weights))
      if (verbose){
        print(summary(weights/sum(weights) * length(weights)))
        print(which.max(weights))
      }
    }
  }

  # limits extreme weights to median plus 6 times inter-quartile range
  # IQR = difference between 75th percentile and 25th percentile of data
  if (truncate_extreme_weights){
    weight_threshold = median(weights) + 6*IQR(weights)
    weights = pmin(weights, weight_threshold)
  }
  #normalize to population size
  weights = weights*length(weights)/(sum(weights))
  return(weights)
}

# formula in http://www.analyticalgroup.com/download/WEIGHTED_MEAN.pdf
weighted.var.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  x_weighted_mean = sum(x*w)/sum(w)
  sum(w*(x-x_weighted_mean)^2)/(sum(w)-1)

}

weighted.var.sigma <- function(x, w){
  root_b = sqrt(sum(w)^2/sum(w^2))
  weighted.var.se(x,w)/root_b
}

