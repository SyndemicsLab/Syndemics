#' A Function to Change RESPOND inputs from base-case by some percent change
#' @param data dataframe: dataframe from RESPOND base input
#' @param filter string: treatment block to target the change
#' @param cycle list: cycles to edit
#' @param pct_change num: percent to change transition state, +3\% -3\% would be 1.03, .97 respectively
#'
#' @import data.table
#' @export
DSA <- function(data, filter, cycle, pct_change){
  data <- as.data.table(data)
  for(c in cycle){
    DT <- data[initial_block == filter, paste0("to_", filter, "_cycle", c) := get(paste0("to_", filter, "_cycle", c))*pct_change
    ][, paste0("to_corresponding_post_trt_cycle", c) := 1 - get(paste0("to_", filter, "_cycle", c))]
  }
  return(DT)
}

#' A function to change default age groups from RESPOND shell tables
#' @param data dataframe or datatable: A RESPOND table
#' @param size.out Integer: age-group size out
#' @param transformation String: either "mean" or "sum" defining the transformation to new age groups, eg. counts would be summed, transition probabilities would be averaged
#' @param cols List: columns to transform
#' @param grouping List: columns to group on, the default is any columns not listed in \code{cols}
#'
#' @import data.table
#' @export
change_agegrp_chunk <- function(data, size.out, transformation, cols, grouping){
  DT <- as.data.table(data)
  if(!cols %in% names(DT)) stop("Column names in 'cols' do not match names in 'data'")
  if(missing(grouping)) grouping <- names(DT)[c(names(DT) %ni% c(cols, "agegrp"))]

  DT <- DT[, `:=` (age_min = as.integer(sub("_(.*)", "", agegrp)),
                   age_max = as.integer(sub("^(.*?)_", "", agegrp)),
                   agegrp = NULL)]

  chunk.diffs <- unique(DT[["age_min"]] - DT[["age_max"]])
  if(length(chunk.diffs) > 1){
    stop("Chunk differences non-uniform")
  } else size.in <- chunk.diffs*-1 + 1

  # A more efficient method would be to Map(seq, age_min, age_max),
  # although unlisting seems to be a problem.... will work on this in the future

  expanded <- merge(DT, expand.grid(block = unique(DT[["block"]]),
                                    sex = unique(DT[["sex"]]),
                                    oud = unique(DT[["oud"]]),
                                    age = as.integer(c(min(DT[["age_min"]]):max(DT[["age_max"]])))),
                    by = grouping,
                    allow.cartesian = TRUE)[age >= age_min & age <= age_max,
                    ][, `:=` (age_min = NULL,
                              age_max = NULL,
                              agegrp = cut(age, seq(9, 100, size.out),
                                           labels = paste0(seq(10, 98, size.out),
                                                           "_",
                                                           seq(11, 99, size.out))),
                              age = NULL)
                    ]
  if(transformation == "sum"){
    out <- expanded[, (cols) := lapply(cols, function(x) get(x)/size.in),
    ][, (cols) := lapply(cols, function(x) sum(get(x))), by = c("agegrp", "block", "oud", "sex")]
    return(unique(out))

  } else if(transformation == "mean"){
    out <- expanded[, (cols) := lapply(cols, function(x) mean(x))]
    return(unique(out))
  }
}

#' A function to create new blocks for RESPOND (clones 'No_Treatment' blocks and reassigns the name)
#' @param data DataFrame: data to manipulate
#' @param names Str list: name for the new blocks
#' @param column str: column name for treatment blocks
#'
#' @import data.table
#' @export
new_block <- function(data, names, column="block"){
  if(!column %in% names(data)) stop(paste(column, "is not in original data"))
  data <- as.data.table(data)
  data_list <- list()

  for(n in seq_along(names)){
    DT <- copy(data)[get(column) == "No_Treatment", ]
    data_list[[n]] <- DT[, (column) := rep.int(names[n], .N)]
  }
  out <- data.table::rbindlist(append(data_list, list(data)))
  return(out)
}

#' A function to change values to another block; follows the format \code{data$x[column == "a", ] <- data$y[column == "b", ]}
#' @param data DataFrame
#' @param column str: column name
#' @param x str
#' @param y str
#' @param a str
#' @param b str
#' @import data.table
#' @export

replace_vals <- function(data, column="block", x, y, a, b) {
  if (length(a) != length(b)) {
    stop("Vectors 'a' and 'b' must be of the same length")
  }
  setDT(data)
  for (i in seq_along(a)) {
    data[, (x) := as.numeric(get(x))
    ][get(column) == a[i], (x) := data[get(column) == b[i], y, with = FALSE][[1]]]
  }

  return(data)
}
