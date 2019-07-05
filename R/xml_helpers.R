xml_check_length = function(x, opt, mult, path) {
  if (length(x) == 0L && !opt) stopf("Required XML node not found: %s", path)
  else if(length(x) > 1L && !mult) stopf("Multiple XML nodes found: %s", path)
  else if(length(x) > 0L) x
}

xml_make_type = function(x, type) {
  switch(type, 
         I = as.integer(x),
         R = as.numeric(x),
         S = as.character(x),
         D = as.Date(x), 
         POSIXct = as.POSIXct(x, tz = "CET"),
         stopf("Conversion to type %s is not possible; Choose 'I', 'R', 'S', 'D' or 'POSIXct'", type)
  )
}

xml_query = function(doc, path, opt, mult, type) {
  val = xml_text(xml_find_all(doc, path))
  val = xml_check_length(val, opt, mult, path)
  if(!is.null(val)) xml_make_type(val, type)
}

parseXMLResponse = function(file, msg = NA_character_,
  type = NA_character_) {
  message(msg)
  doc = try(read_xml(file))
  if (is.error(doc))
    stopf("Error in parsing XML for type %s in file: %s", type, file)

  return(doc)
}
