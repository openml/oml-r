#' @title Get an OpenML data set.
#'
#' @description Given a data set ID, the corresponding \code{\link{OMLDataSet}}
#' will be downloaded (if not in cache) and returned.
#'
#' Note that data splits and other task-related information are not included in
#' an \code{\link{OMLDataSet}}. Tasks can be downloaded with \code{\link{getOMLTask}}.
#'
#' @note
#' One of \code{data.id} or \code{data.name} must be passed.
#'
#' @template arg_data.id
#' @param data.name [\code{character(1)}]\cr
#'   Data set name.
#'   This is an alternative to \code{data.id}.
#'   Default is \code{NULL}.
#' @param data.version [\code{integer(1)}]\cr
#'   Version number of the data set with name \code{data.name}.
#'   Default is \code{NULL}.
#'   Ignored if \code{data.id} is passed.
#' @template arg_cache_only
#' @template arg_verbosity
#' @return [\code{\link{OMLDataSet}}].
#' @family downloading functions
#' @family data set-related functions
#' @example inst/examples/getOMLDataSet.R
#' @export
getOMLDataSet = function(data.id = NULL, data.name = NULL, data.version = NULL, cache.only = FALSE, verbosity = NULL) {
  if (!xor(is.null(data.id), is.null(data.name)))
    stopf("You must provide either a data.id or a data.name, but not both.")

  assertFlag(cache.only)

  if (is.null(data.name)) {
    data.id = asInt(data.id, lower = 0)
    return(getOMLDataSetById(data.id = data.id, cache.only = cache.only, verbosity = verbosity))
  }
  getOMLDataSetByName(data.name = data.name, data.version = data.version, cache.only = cache.only, verbosity = verbosity)
}

# Helper function to get data set by data name (and version number).
# (Makes use of getOMLDataSetById)
getOMLDataSetByName = function(data.name = NULL, data.version = NULL, cache.only = FALSE, verbosity = NULL) {
  # else get list of datasets RESTRICTED to the given name
  data.sets = .listOMLDataSets(data.name = data.name, verbosity = verbosity)

  # match by name
  matching.ids = which(data.sets$name == data.name)
  matching.sets = data.sets[matching.ids, , drop = FALSE] # nolint

  # otherwise we have multiple matches and need to consider the version
  data.id = if (is.null(data.version)) {
    # in this case we default to the newest version
    showInfo(verbosity, "Multiple version available, but no data.version passed! Returning the newest version.")
    matching.sets[getMaxIndex(matching.sets$version), "data.id"]
  } else {
    data.version = asInt(data.version, lower = 0)
    matching.sets[matching.sets$version == data.version, "data.id"]
  }

  if (is.null(data.id) || length(data.id) == 0) {
    stopf("Version %i does not exist for dataset '%s'. Available versions: %s",
      data.version, data.name, collapse(matching.sets$version, sep = ", "))
  }

  # get number of matches ...
  n.matches = length(matching.ids)
  # ... and react accordingly
  if (n.matches == 0)
    stopf("No dataset with name '%s' found.", data.name)
  if (n.matches == 1)
    return(getOMLDataSetById(data.id = matching.sets$data.id, cache.only = cache.only, verbosity = verbosity))

  return(getOMLDataSetById(data.id = data.id, cache.only = cache.only, verbosity = verbosity))
}

# Helper function to get data set by data ID.
getOMLDataSetById = function(data.id = NULL, cache.only = FALSE, verbosity = NULL) {
  down = downloadOMLObject(data.id, object = "data", cache.only = cache.only, verbosity = verbosity)
  f = down$files

  # parse data set description
  data.desc = parseOMLDataSetDescription(down$doc)

  # warn if dataset not cached and deactivated
  if (data.desc$status == "deactivated") {
    warningf("Data set has been deactivated.")
  } else if (data.desc$status == "in_preparation") {
    warningf("Data set is in preparation and will be activated soon.")
  }

  # now read data file
  data = arff.reader(f$dataset.arff$path)

  if (!is.na(data.desc$row.id.attribute)) {
    # add row.id.attribute also to ignore list if not already there
    data.desc$ignore.attribute = union(data.desc$ignore.attribute, data.desc$row.id.attribute)
  }
  data = setRowNames(data, as.character(seq_row(data) - 1L))

  def.target = data.desc$default.target.attribute
  target.ind = which(colnames(data) %in% def.target)

  colnames.old = colnames(data)
  colnames(data) = make.names(colnames(data), unique = TRUE)
  colnames.new = colnames(data)

  # overwrite default target attribute to make sure that it's the actual name of the column
  data.desc$default.target.attribute = colnames.new[target.ind]

  makeOMLDataSet(
    desc = data.desc,
    data = data,
    colnames.old = colnames.old,
    colnames.new = colnames.new,
    target.features = data.desc$default.target.attribute
  )
}

parseOMLDataSetDescription = function(doc) {
  # specify settings for the xml query
  args_names = c("id", "name", "version", "description", "format", "creator", "contributor", 
                 "collection_date", "upload_date", "language", "licence", "url", 
                 "default_target_attribute", "row_id_attribute", "ignore_attribute", "version_label",
                 "citation", "visibility", "original_data_url", "paper.url", "update.comment", 
                 "md5_checksum", "status", "tag")
  args_opt = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  args_mult = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  args_type = c("I", "S", "S", "S", "S", "S", "S", "S", "POSIXct", "S", "S", "S", "S", "S", "S", "S", 
                "S", "S", "S", "S", "S", "S", "S", "S")
  args_xpQ = sprintf("/oml:data_set_description/oml:%s", args_names)
  
  # build list containing queried values
  args = lapply(seq_along(args_names), function(i) xml_query(doc, args_xpQ[i], args_opt[i], args_mult[i], args_type[i]))
  names(args) = convertNamesOMLToR(args_names)
  
  args$default.target.attribute = if (!is.null(args$default.target.attribute)) 
    unlist(strsplit(args$default.target.attribute, ",")) else ""
  args$ignore.attribute = if (!is.null(args$ignore.attribute)) 
    unlist(strsplit(args$ignore.attribute, ","))
  
  do.call(makeOMLDataSetDescription, filterNull(args))
}
