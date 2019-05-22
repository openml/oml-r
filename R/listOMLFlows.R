.listOMLFlows = function(tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  api.call = generateAPICall("json/flow/list", tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.table())
  flows = setDT(fromJSON(txt = content)$flows$flow)

  # type conversions
  cols = c("id", "version", "uploader")
  flows[, (cols)  := lapply(.SD, as.integer), .SDcols = cols]
  #flows$tags = vcapply(flows$tags, function(x) collapse(x, ", "))

  setnames(flows, convertNamesOMLToR(names(flows)))
  setnames(flows, gsub("^id$", "flow.id", names(flows)))
  flows[]
}

listOMLFlows = memoise(.listOMLFlows)
