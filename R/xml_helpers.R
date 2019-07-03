xmlNs = function(doc, path, optional) {
  ns = xml2::xml_find_all(doc, path)
  if (length(ns) == 0L) {
    if (optional)
      NULL
    else
      stopf("Required XML node not found: %s", path)
  } else {
    ns 
  }
}

# function factory
xmlVal = function(optional, fun) {
  force(optional)
  force(fun)
  function(doc, path) {
    ns = xmlNs(doc, path, optional)
    # path not found, also cant be no optional, otherwise exception in call before
    if (is.null(ns))
      return(NULL)
    if (length(ns) == 1L) {
      fun(xml2::xml_text(ns[1L]))
    } else {
      stopf("Multiple XML nodes found: %s", path)
    }
  }
}

# build functions
xmlOValS = xmlVal(TRUE, as.character)
xmlOValI = xmlVal(TRUE, as.integer)
xmlOValR = xmlVal(TRUE, as.numeric)
xmlOValD = xmlVal(FALSE, as.Date)
xmlRValS = xmlVal(FALSE, as.character)
xmlRValI = xmlVal(FALSE, as.integer)
xmlRValR = xmlVal(FALSE, as.numeric)
xmlRValD = xmlVal(FALSE, function(x) as.POSIXct(x, tz = "CET"))

xmlREValI = function(doc, path) {
  val = xmlRValI(doc, path)
  if (is.na(val))
    return(integer(0L))
  else
    return(val)
}

xmlREValR = function(doc, path) {
  val = xmlRValR(doc, path)
  if (is.na(val))
    return(numeric(0L))
  else
    return(val)
}

xmlREValI = function(doc, path) {
  val = xmlRValI(doc, path)
  if (is.na(val))
    return(integer(0L))
  else
    return(val)
}

xmlValsMultNs = function(doc, path, fun, val) {
  ns = xml2::xml_find_all(doc, path)
  vapply(ns, function(x) fun(xml_text(x)), val)
}

xmlValsMultNsS = function(doc, path) {
  xmlValsMultNs(doc, path, as.character, character(1))
}

xmlOValsMultNsS = function(doc, path, empty.return = NULL) {
  val = xmlValsMultNs(doc, path, as.character, character(1))
  if (length(val) == 0L)
    return(empty.return)
  else
    return(val)
}

xmlOValsMultNsSPara = function(doc, path, subs = NA_character_, exp.length) {
  val = xmlValsMultNs(doc, path, as.character, character(1L))
  if (length(val) == 0L)
    return(rep(subs, times = exp.length))
  val[is.na(val) | !nzchar(val)] = subs
  if (length(val) != exp.length)
    val = c(val, rep(subs, times = exp.length - length(val)))
  return(val)
}

parseXMLResponse = function(file, msg = NA_character_,
  type = NA_character_) {
  message(msg)
  doc = try(read_xml(file))
  if (is.error(doc))
    stopf("Error in parsing XML for type %s in file: %s", type, file)

  return(doc)
}
