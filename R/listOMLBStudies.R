.listOMLBStudies = function(status = NULL, limit = NULL, offset = NULL,
                            main_entity_type = "run", uploader.id = NULL, verbosity = NULL) {
  
  api.call = generateAPICall("xml/study/list", 
                             uploader.id = uploader.id, limit = limit, offset = offset,
                             status = status, main_entity_type = main_entity_type)
  
  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  
  # error, since REST still under development
  # remaining part (returning/handling output) will be added, when REST is accessible
  # xml_doc = read_xml(content) ...
}