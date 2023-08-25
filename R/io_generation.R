#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Sun Jul 31 10:45:38 2022
# ~: gets I/O functions
#----------------------------------------------#



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

init_mock_funs = function(){
  # DOES NOT WORK!!!!
  # BEWARE OF POSITONAL ARGUMENTS!!!!
  # NEED TO CREATE THE MOCK ON THE FLY!!!!
  
  # TO FIND OUT IO TYPE:
  # - if io_target first argument = READ
  #   otherwise: write
  
  io_mock = list()
  
  io_targets = c("description", "con", "file", "path")
  
  for(type in c("io", "input", "output")){
    for(arg in io_targets){
      fun_txt = sma("
      function(.[arg], ...){
        if(inherits(.[arg], 'connection')){
            # => nothing
            return(NULL)
        }
        attr(.[arg], 'type') = .[type]
        .[arg]
      }", .delim = ".[ ]")
    }
  }
  
  
  
}
# 
# af = file("DATA.R")
# summary(af)$mode


list_flatten = function(x){
  # x must be a list of atomic elements, all of the same type
  
  x_len = lengths(x)
  x_flat = unlist(x)
  
  id = rep(seq_along(x), x_len)
  
  
  data.frame(id = id, x = x_flat)
}






