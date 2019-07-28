.listOMLBStudies = function(alias = TRUE, status = "all", limit = NULL, offset = NULL,
                            main_entity_type = "task", uploader.id = NULL, verbosity = NULL) {
  
  api.call = generateAPICall("xml/study/list", 
                             uploader.id = uploader.id, limit = limit, offset = offset,
                             status = status, main_entity_type = main_entity_type)
  
  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.table())
  
  doc = read_xml(paste(content, collapse = " "))
  
  args_names = c("name", "id")
  if (alias) args_names = c(args_names, "alias")

#   ** Might suffice to just use this commented block instead of remaining part. It suffices, 
#      if each study ALWAYS has at least the nodes/elements (which can be empty) "name", "id", "alias" **
#
#   args = lapply(seq_along(args_names), function(i) xml_query(doc, args_xpQ[i], FALSE, TRUE, "S"))
#   names(args) = convertNamesOMLToR(args_names)
# 
#   res = setDT(args)
#   res[]
  
  ns = xml_children(doc)
  get_text = function(node, xpQ) {
    txt = xml_text(xml_contents(xml_child(node, xpQ)))
    if(length(txt) == 0) txt = ""
    return(txt)
  }
  get_data = function(node, args_names) {
    dt = lapply(args_names, function(x) get_text(node, sprintf("oml:%s", x)))
    names(dt) = args_names
    return(dt)
  }
  studies_list = lapply(seq_along(ns), function(i) get_data(ns[[i]], args_names))
  res = rbindlist(studies_list)
  return(res)
}
listOMLBStudies = memoise(.listOMLBStudies)