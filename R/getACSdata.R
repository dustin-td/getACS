#' Get ACS Data using API
#' 
#' Creates API call for ACS data.
#' 
#' @param vars List of ACS variable names
#' @param endpoint ACS API endpoint
#' @param apikey API Key
#' @param geounit API Geography unit
#' @return A data.table with data returned by API
#' @export
getACSdata = function(vars, endpoint, apikey, geounit, ...) {
  
  params = paste(apikey, geounit, ...,
                 paste0('get=',paste(vars, collapse=',')),
                 sep='&',
                 collapse='')
  
  call = utils::URLencode(paste0(endpoint,'?', params))
  print(call)
  get_call = httr::RETRY('GET', call)
  
  data = data.table::as.data.table(jsonlite::fromJSON(httr::content(get_call, 'text'), flatten=T))
  
  data.table::setnames(data,names(data),as.character(data[1]))
  data = data[-1]
  
  for (j in grep('^B.*_', names(data), value=T)) {
    data.table::set(data, j = j, value = as.numeric(data[[j]]))
  }
  
  return(data)
  
}

#' Load list of ACS 5-year variables
#' 
#' @param year ACS 5-year year
#' @return A data.table with list of vars
#' @export
getACS5vars = function(year) {
  vars = httr::content(httr::GET(paste0('https://api.census.gov/data/', year, '/acs/acs5/variables.html')), 'text')
  vars = data.table::as.data.table(XML::readHTMLTable(vars, header=T)[[1]])
  vars[, Label := sub('Estimate!!','',Label)]
  vars[, Label := gsub('!!', ' ', Label)]
  
  return(vars)
}

#' Load list of ACS 1-year variables
#' 
#' @param year ACS 1-year year
#' @return A data.table with list of vars
#' @export
getACS1vars = function(year) {
  vars = httr::content(httr::GET(paste0('https://api.census.gov/data/', year, '/acs/acs1/variables.html')), 'text')
  vars = data.table::as.data.table(XML::readHTMLTable(vars, header=T)[[1]])
  vars[, Label := sub('Estimate!!','',Label)]
  vars[, Label := gsub('!!', ' ', Label)]
                       
  return(vars)
}

#' Return character vector of variable names given ACS group
#' 
#' @param group ACS group name
#' @param vars List of ACS vars from getACS#vars()
#' @return a character vector of variable names
#' @export
getACSgroup = function(group, vars) {
  group.vars = vars[Group == group, .(Name)]
  group.vars[, Name := as.character(Name)]
  group.vars = c('NAME', group.vars[[1]])
  return(group.vars)
}

#' Download census-tract level ACS data for multiple states
#' 
#' @param vars List of ACS vars to retrieve
#' @param endpoint ACS API endpoint
#' @param states List of states to download data for
#' @return A data.table of selected ACS variables
#' @export
tractACS.us = function(vars, endpoint, states) {
  data = lapply(states, function(j) {
    state = paste0('in=state:', j)
    getACS::getACSdata(vars = vars,
               endpoint = endpoint,
               apikey = apikey,
               geounit = geounit,
               state = state)
  })
  data = data.table::as.data.table(do.call('rbind', data))
  return(data)
}


