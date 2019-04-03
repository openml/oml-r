.listOMLEstimationProcedures = function(verbosity = NULL) {
  content = doAPICall(api.call = "json/estimationprocedure/list", file = NULL,
                      verbosity = verbosity, method = "GET")
  res = setDT(fromJSON(txt = content)$estimationprocedures$estimationprocedure)
  task.types = listOMLTaskTypes(verbosity = 0)
  res[, c("id", "ttid") := .(as.integer(res$id), task.types[as.integer(res$ttid), as.character(name)])]
  setnames(res, old = c("id", "ttid"), new = c("est.id", "task.type"))
  setnames(res, convertNamesOMLToR(names(res)))
  type.convert(res, as.is = TRUE, how = "replace")
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
