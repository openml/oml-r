.listOMLSetup = function(setup.id = NULL, flow.id = NULL,
  limit = 1000, offset = NULL, verbosity = NULL) {
  # FIXME: this function is very ugly due to different sturctures returned from server
  api.call = generateAPICall(api.call = "json/setup/list",
    setup.id = setup.id, flow.id = flow.id, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.table())

  # Get entries, which are grouped by setup.id
  setup = fromJSON(txt = content)$setups$setup
  sid = data.table(join_id = sprintf("%d", 1:length(setup$setup_id)), setup_id = setup$setup_id)

  # Get parameters and clean them up
  param = setup$parameter
  if (!is.null(names(param))) {
    # if elements have a name, it refers to parameter
    param = param[!vlapply(param, function(x) length(x) == 0)]
    param = as.data.table(param, stringsAsFactors = FALSE)
    param[, join_id := 1]
  } else {
    # add names
    param = setNames(param, 1:length(param))
    # filter out NULL or empty elements
    param = param[!vlapply(param, function(x) length(x) == 0)]
    # inside each element, replace empty values with NA
    param = lapply(param, function(x) {
      replace(x, which(vlapply(x, function(i) length(i) == 0)), NA_character_)
    })
    param = rbindlist(param, fill = TRUE, idcol = "join_id")
  }

  list.cols = colnames(param)[param[, vlapply(.SD, is.list)]]
  for (col in list.cols) {
    ind = which(vlapply(param[[col]], function(i) length(i) == 0))
    param[[col]][ind] = NA_character_
    param[[col]] = unlist(param[[col]], recursive = FALSE)
  }

  ret = merge(param, sid, by = "join_id")
  ret[, c("id", "join_id") := NULL]

  cn = c("setup_id", "flow_id", "full_name", "parameter_name", "data_type", "default_value", "value")
  select_cols = cn[cn %in% colnames(ret)]
  ret = ret[, ..select_cols]
  setnames(ret, convertNamesOMLToR(names(ret)))
  ret[]
}

#' @title List hyperparameter settings
#'
#' @description
#' Each run has a \code{setup.id}, i.e. an ID for the hyperparameter settings of the flow that produced the run.
#' This function allows the listing of hyperparameter settings.
#'
#' @template note_memoise
#'
#' @param setup.id [\code{integer(1)}]\cr
#'   ID of the setup (which is basically an ID for the parameter configuration).
#' @template arg_flow.id
#' @template arg_limit
#' @template arg_offset
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLSetup.R
listOMLSetup = memoise(.listOMLSetup)
