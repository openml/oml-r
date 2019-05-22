.listOMLEstimationProcedures = function(verbosity = NULL) {
  content = doAPICall(api.call = "json/estimationprocedure/list", file = NULL,
                      verbosity = verbosity, method = "GET")
  res = fromJSON(txt = content)$estimationprocedures$estimationprocedure
  task.types = listOMLTaskTypes(verbosity = 0)
  setnames(task.types, c("ttid", "task.type"))
  task.types[, "ttid" := as.character(ttid)]
  ret = merge(task.types, res, by = "ttid")
  ret[, c("id", "ttid") := .(as.integer(id), NULL)]
  setnames(ret, "id", "est.id")
  setkeyv(ret, "est.id") # sort by est.id...
  setcolorder(ret) # ...and get est.id column in first position
  setnames(ret, convertNamesOMLToR(names(ret)))
  type.convert(ret, as.is = TRUE, how = "replace")
}

#' @title List available estimation procedures.
#'
#' @description
#' The returned \code{data.frame} contains the \code{est.id} and the corresponding
#' name of the estimation procedure.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLEstimationProcedures.R
listOMLEstimationProcedures = memoise(.listOMLEstimationProcedures)
