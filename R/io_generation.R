#============================================================================#
#            Author: Laurent R. Bergé, University of Bordeaux                #
#            Copyright (C) 2025-present, Laurent R. Bergé                    #
#            MIT License (see project_root/LICENSE)                          #
#============================================================================#



generate_mock_io_funs = function(pkg){
  
  pkg = "base"
  ns = loadNamespace(pkg)
  funs_exported = getNamespaceExports(ns)
  
  formal_args = function(fun) names(formals(get(fun, mode = "function", ns)))
  
  fun_args_all = lapply(funs_exported, function(x) if(exists(x, mode = "function", where = ns)) formal_args(x))  
  
  fun_args_flat = list_flatten(fun_args_all)
  
  io_targets = c("description", "con", "file", "path")
  qui = fun_args_flat$x %in% io_targets

  funs_io = funs_exported[fun_args_flat$id[qui]]
  
  
  mock_funs
  
  
}



list_flatten = function(x){
  # x must be a list of atomic elements, all of the same type
  
  x_len = lengths(x)
  x_flat = unlist(x)
  
  id = rep(seq_along(x), x_len)
  
  
  data.frame(id = id, x = x_flat)
}


reset_loaded_packages = function(){
  options("rmake_loaded_pkg" = NULL)
}

get_loaded_packages = function(){
  unique(getOption("rmake_loaded_pkg"))
}

reset_loaded_sources = function(){
  options("rmake_loaded_sources" = NULL)
}

get_loaded_sources = function(){
  unique(getOption("rmake_loaded_sources"))
}

mock_library = function(package, ..., character.only = FALSE){
  
  if(!character.only){
    package = deparse(substitute(package))
  }
  
  options(rmake_loaded_pkg = append(getOption("rmake_loaded_pkg"), package))
  
  library(package, character.only = TRUE, ...)
}


mock_require = function(package, ..., character.only = FALSE){
  
  if(!character.only){
    package = deparse(substitute(package))
  }
  
  options(rmake_loaded_pkg = append(getOption("rmake_loaded_pkg"), package))
  
  require(package, character.only = TRUE, ...)
}


mock_source = function(file, ...){
  
  options(rmake_loaded_sources = append(getOption("rmake_loaded_sources"), file))
  
  source(file, ...)
}


mock_sourceCpp = function(file, ...){
  
  options(rmake_loaded_sources = append(getOption("rmake_loaded_sources"), file))
  
  Rcpp::sourceCpp(file, ...)
}


####
#### R/W funs ####
####


rw_funs = list(
  # source
  source = function(file, ...) structure(file, type = "source"),

  # read
  load = function(file, ...) structure(file, type = "read"),
  read.csv = function(file, ...) structure(file, type = "read"),
  readfst = function(path, ...) structure(path, type = "read"),
  read_fst = function(path, ...) structure(path, type = "read"),
  fread = function(input, file, text, ...){
    if(!missing(file)) return(structure(file, type = "read"))
    if(!missing(input) && !grepl("\n", input)) return(structure(input, type = "read"))
    ""
  },

  # write
  save = function(..., file) structure(file, type = "write"),
  write_fst = function(x, path, ...) structure(path, type = "write"),
  fwrite = function(x, file, ...) structure(file, type = "write"),
  write.csv = function(x, file) structure(file, type = "write")
)


read_funs = list(
  load = function(file, ...) file,
  read.csv = function(file, ...) file,
  readfst = function(path, ...) path,
  read_fst = function(path, ...) path,
  fread = function(input, file, text, ...){
    if(!missing(file)) return(file)
    if(!missing(input) && !grepl("\n", input)) return(input)
    ""
  },
  path_in = function(path) path
)


write_funs = list(
  save = function(..., file) file,
  write_fst = function(x, path, ...) path,
  fwrite = function(x, file, ...) file,
  write.csv = function(x, file) file,
  path_out = function(path) path
)

#
# same with calls
#


rw_fun_calls = list(
  # read
  load = function(file, ...) match.call()$file,
  read.csv = function(file, ...) match.call()$file,
  readfst = function(path, ...) match.call()$path,
  read_fst = function(path, ...) match.call()$path,
  fread = function(input, file, text, ...){
    if(!missing(file)) return(match.call()$file)
    if(!missing(input) && !grepl("\n", input)) return(match.call()$input)
    ""
  },
  # write
  save = function(..., file) match.call()$file,
  write_fst = function(x, path, ...) match.call()$path,
  fwrite = function(x, file, ...) match.call()$file,
  write.csv = function(x, file) match.call()$file
)




