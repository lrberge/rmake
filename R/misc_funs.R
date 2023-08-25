#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2023-08-24
# ~: misc funs to support dev
#------------------------------------------------------------------------------#



####
#### aliases to imports ####
####

sma = stringmagic::string_magic_alias(.check = FALSE)


####
#### internal misc ####
####


deparse_long = function(x){
    dep_x = deparse(x, width.cutoff = 500)
    if(length(dep_x) == 1){
        return(dep_x)
    } else {
        return(paste(gsub("^ +", "", dep_x), collapse = ""))
    }
}

deparse_code = function(x){
  res = as.character(x)
  if(length(res) == 0) return("")
  if(res[1] == "{") res = res[-1]
  res
}

setup_root_path = function(){
  path = find_project_path(TRUE)
  
  options(rmake_root_path = path)
}

find_project_path = function(force = FALSE){
    # finds the root directory
    # we just look up the search path to find the root

    past_path = "init"
    path = normalizePath(".", "/")

    is_found = FALSE
    i = 1
    nmax = 10
    while(past_path != path && i <= nmax){
        i = i + 1
        if(length(list.files(path, pattern = "(Rproj|git|vscode)$")) > 0){
            is_found = TRUE
            break
        } else {
            past_path = path
            path = dirname(path)
        }
    }

    proj_path = NULL
    if(is_found){
        proj_path = path
    }

    if(force && is.null(proj_path)){
        proj_path = normalizePath(".", "/")
    }

    proj_path
}

root_path = function(path){
  # This is an internal function, the path must not start with ./
  root_path = getOption("rmake_root_path")
  if(substr(path, 1, 2) == "./"){
    path = substr(path, 3, 5000)
  }
  
  file.path(root_path, gsub("^\\./", "", path))
}









