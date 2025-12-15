# Function to reformat the data
gen_datalist <- function(data) {
  feature_table <- data$feature_table
  sample_metadata <- data$sample_metadata
  feature_metadata <- data$feature_metadata

  ## Separate omics layers
  feature_metadata$featureType <- as.factor(feature_metadata$featureType)
  name_layers <- with(droplevels(feature_metadata),
                      list(levels = levels(featureType)),
                      nlevels = nlevels(featureType))$levels

  ## Extract y
  y <- as.matrix(sample_metadata$Y)

  ## Construct xList
  xList <- vector("list", length = length(name_layers))
  names(xList) <- name_layers

  ## Store data matrices
  for (i in seq_along(name_layers)) {
    ## Filter genes which belong to featureType[i]
    include_list <- dplyr::filter(
      feature_metadata,
      feature_metadata$featureType == name_layers[i]
    )

    ## Subset feature rows for features in Type[i]
    t_dat_slice <- feature_table[rownames(feature_table) %in% include_list$featureID, ]

    ## Assign feature rows for Type[i] to dataList[i]
    xList[[i]] <- as.data.frame(t(t_dat_slice))
    rm(t_dat_slice); rm(include_list)
  }

  list(xList = xList, y = y)
}

# Function to reconstruct data object from (xList, y)
reconstruct_data <- function(xList, y) {
  # Concatenate features across all omics layers
  feature_table <- as.data.frame(do.call(cbind, xList))

  # --- feature_metadata ---
  feature_metadata <- data.frame(
    featureID   = colnames(feature_table),
    featureType = rep(names(xList), times = sapply(xList, ncol))
  )

  rownames(feature_metadata) <- colnames(feature_table)

  # --- sample_metadata ---
  sample_metadata <- data.frame(
    subjectID = rownames(xList[[1]]),
    Y        = as.vector(y)
  )

  rownames(sample_metadata) <- rownames(xList[[1]])

  # Return full data object
  list(
    feature_table   = t(feature_table),
    sample_metadata = sample_metadata,
    feature_metadata = feature_metadata
  )
}

# Function to filter features according to a fixed abundance and prevalence threshold
filter_features <- function(x, abd_threshold = 0, prev_threshold = 0.1) {
  # x is a data frame
  x <- x[, colMeans(x > abd_threshold) > prev_threshold]
  nzv_x <- caret::nearZeroVar(x, names = TRUE)
  x <- x[, setdiff(names(x), nzv_x)]
  return(x)
}
