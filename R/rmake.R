#============================================================================#
#            Author: Laurent R. Bergé, University of Bordeaux                #
#            Copyright (C) 2025-present, Laurent R. Bergé                    #
#            MIT License (see project_root/LICENSE)                          #
#============================================================================#





#' Automatically keeps code chunks up to date
#' 
#' This function first finds the dependencies between chunks of code. Then it runs
#' the code in a specific order to make all the chunks up to date. 
#' To create code chunks in your R files and benefit from `rmake`, see details.
#' 
#' 
#' @param hard Logical, default is `FALSE`. If `TRUE`, all code chunks will be run afresh, 
#' as if they were just created.
#' @param ask Logical, default is `TRUE`. Whether to interactively ask the user whether or
#' not to run the code that needs to be run.
#' @param comment Logical, default is `FALSE`. If `TRUE`, then anything that should be printed
#' in the screen when running the code chunks will be inserted, as comments, in the document. 
#' Identical comments are not appended.
#' @param project A path to a folder, default is NULL. If provided, `rmake` will consider
#' the path given by `project` as the active project. It will then apply to all R files from
#' this project (not to the current one).
#' 
#' @details 
#' By default `rmake` tracks all R files from a given project and tries to find 
#' code chunks within them (see the code chunk section).
#' 
#' `rmake` tracks two kind of dependencies. First, file dependencies: if a chunk 
#' uses as input the files that another produced as output. Typical example: one chunk for data
#' cleaning (output `data.tsv`), another for the analysis of this data (input `data.tsv`).
#' Second, function dependencies. All user written functions are tracked. If any of 
#' them is modified, then all chunks that use them are re-run. Please have a look at the section
#' on input/ouput detection.
#' 
#' `rmake` handles indirect dependencies. For example, consider three chunks A, B, and C, 
#' and let's note a dependency with an arrow. Assume A => B => C (C depends on B which 
#' depends on A). Then if A's code is modified `rmake` will run the chunks A, then B, then C,
#' in that specific order.
#' 
#' rmake keeps track of the results in a file located at .rmake/
#' 
#' @section Defining a code chunks:
#' 
#' `rmake` only tracks code chunks which are pieces of code within the R files of a project.
#' To create a code chunk, use a header comment (i.e. a comment ending with 4 hashes)
#' that starts with the equal sign (`=`).
#' For example the line `# = data cleaning ####` defines the code chunk named 
#' "data cleaning".
#' 
#' A chunk goes from its starting line to either:
#' - the start of another chunk,
#' - the end of the R file,
#' - an ignored chunk.
#' 
#' An ignored chunk is a chunk starting with `!=`. For example: `# != dev ####` creates
#' the ignore chunk "dev". `rmake` will ignore everything after that line until if finds
#' a valid chunk (if there is one).
#' 
#' @section Writing a code chunk:
#' 
#' When writing a code chunk, keep in mind the following restriction:  
#' A code chunk must be a piece of code that is independent from any other code,
#' except from functions, and the values contained in the `.Rprofile`. 
#' That is: the user should be able to run it without error from a fresh R session.
#' 
#' If your code depends on functions you wrote, simply load them from within the 
#' `.Rprofile`. If you don't know what the `.Rprofile` is, it is a file whose content
#' is automatically run during the session's startup.
#' 
#' If your code depends on the data created in other places, save this data on disk 
#' in one chunk, and then load it from disk in another chunk.
#' 
#' You need to decompose the key steps of your processing. 
#' If two pieces of code cannot be separated, it means that they belong to the same chunk.
#' 
#' @section Input output auto detection:
#' 
#' To automatically detect inputs and outputs, `rmakes` uses a trick:
#' - all functions with the arguments `path` or `file` whose result is not assigned is
#' considered as `output`
#' - all functions with the arguments `path` or `file` whose result *is* assigned is
#' considered as `input`
#' 
#' For example let's consider the function `my_reading_fun(path)`. Then the line
#' `my_reading_fun("test.txt")` is considered as `output` by remake because there is 
#' no assignment. Instead, `rmake` considers the line `data = my_reading_fun("test.txt")` 
#' as `input` because there is an assignment. 
#' 
#' As you can imagine, even if this guessing works most of the time,
#' there will always be false positives and false negatives (inputs considered as output, 
#' vice versa, or input/outputs not tracked at all).
#' 
#' 
#' @return 
#' This function does not return anything. It displays on the console the files
#' that need to be run, and the reason for running them.
#' 
#' @examples 
#' 
#' # rmake only works project wise, so for this example:
#' #  1) we create a project in a temp folder with a few files
#' #  2) we apply rmake to this mock project
#' 
#' # Note that in general, rmake should only be called as rmake()
#' # The example below is artificial, only for illustration
#' 
#' #
#' # We create a project in a temporary directory
#' # The project contains one R file with two chunks
#' 
#' code = "
#' # = data creation ####
#' set.seed(0)
#' n = 100
#' base = data.frame(x = rnorm(n))
#' beta = 1
#' base$y = 1 + beta * base$x + rnorm(n)
#' 
#' # we save this data set
#' saveRDS(base, 'data_example.rds')
#' 
#' # = estimation ####
#' 
#' # we load the data from disk
#' base = readRDS('data_example.rds')
#' 
#' est = lm(y ~ x, base)
#' 
#' # we save the result
#' saveRDS(summary(est), 'estimation.rds')
#' "
#' 
#' proj_dir = tempdir()
#' writeLines(code, file.path(proj_dir, "example.R"))
#' 
#' # we run rmake on this project
#' if(interactive()){
#'   rmake(project = proj_dir)
#' } else {
#'   rmake(project = proj_dir, ask = FALSE)
#' }
#' 
#' # let's read the estimation
#' readRDS(file.path(proj_dir, "estimation.rds"))
#' 
#' # now let's change the code of the first chunk: we change beta
#' writeLines(sub("beta = 1", "beta = 2", code), file.path(proj_dir, "example.R"))
#' 
#' # rmake re-runs the code that needs to be run
#' if(interactive()){
#'   rmake(project = proj_dir)
#' } else {
#'   rmake(project = proj_dir, ask = FALSE)
#' }
#' 
#' # let's see if it worked:
#' readRDS(file.path(proj_dir, "estimation.rds"))
#' 
#' 
rmake = function(hard = FALSE, ask = TRUE, comment = FALSE, project = NULL, dry = FALSE){
  
  if(is.null(project)){
    project = getOption("rmake_root_path_origin")
  } else {
    check_arg(project, "read path dir")
    on.exit(setwd(getOption("rmake_root_path_origin")))
    project = normalizePath(project, "/")
    setwd(project)
  }
  
  options(rmake_root_path = project)

  if(hard){
    rm_file(root_path(".rmake/rmake.rds"))
  }

  # BEWARE: use here to find the root path
  # Go into the folders too.
  files = list.files(project, pattern = "\\.(r|R)$", full.names = TRUE, recursive = TRUE)
  files = gsub("^\\./", "", files)

  # .Rprofile
  env_rprofile = new.env(parent = .GlobalEnv)
  pkgs = NULL
  fun_list = list()
  
  if(file.exists(root_path(".Rprofile"))){
    # the profile is ALWAYS evaluated
    prof_text = readLines(root_path(".Rprofile"))
    
    # we remove the `base::` or `Rcpp::` before the packages/source
    prof_text = string_clean(prof_text, 
                             "base::(library|require|source) => \\1",
                             "Rcpp::(?=sourceCpp)")
    
    # we run the code to get the packages and source'd functions
    # => this way we can catch nested calls and not only plain 
    # library(haha) or source(path)
    # calls
    reset_loaded_packages()
    reset_loaded_sources()
    
    prof_code = try(parse(text = prof_text, keep.source = FALSE))
    
    assign("library", mock_library, envir = env_rprofile)
    assign("require", mock_require, envir = env_rprofile)
    assign("source", mock_source, envir = env_rprofile)
    assign("sourceCpp", mock_sourceCpp, envir = env_rprofile)
    
    eval(prof_code, env_rprofile)
    
    pkgs = get_loaded_packages()
    source_paths = get_loaded_sources()
    
    fun_list = extract_source_functions_from_path(source_paths)
  }
  
  all_chunks = list()
  for(f in files){
    file_chunks = create_chunks(f, fun_list, env_rprofile, pkgs)
    if(!is.null(file_chunks)){
      nm = paste0(f, "$", names(file_chunks))
      all_chunks[nm] = file_chunks
    }
  }

  # I also need to get the dependency of the source files
  dep_graph = dependency_graph(all_chunks)
  # directed graph of dependencies
  # vector of whether the files are up-to-date or not
  # Throw error if loops in the graph
  
  if(dry){
    return(dep_graph)
  }

  all_chunks = dep_graph$all_chunks
  dep_mat = dep_graph$dep_mat

  lazy_run(all_chunks = dep_graph$all_chunks, dep_mat = dep_graph$dep_mat, 
           env_rprofile = env_rprofile, comment = comment, ask = ask)

}


path_in = function(path){
  check_arg("path")
  path
}

path_out = function(path){
  check_arg("path")
  path
}


path_list_to_vector = function(x){
  res = c()
  for(path_info in x){
    if(!is.null(path_info$path)){
      res = c(res, path_info$path)
    }
  }
  
  res
}


#' List the path depedencies found by rmake
#' 
#' Use this function to display the path dependencies found by rmake across
#' all the chunks from the project.
#' 
#' @inheritParams rmake
#' 
#' @return 
#' It returns a list. The names of the list are the names of the code chunks in the 
#' following format: `filename@chunkname`. Each element of the list contains 
#' two elements: `input` and `output`, which are both character vectors of paths.
#' 
#' 
#' 
rmake_path_dependencies = function(project = NULL){
  info = rmake(project = project, dry = TRUE)
  
  chunks = info$all_chunks
  
  all_nm = sapply(chunks, function(x) sma("{x$filename}@{x$chunk_name}"), USE.NAMES = FALSE)
  res = lapply(chunks, function(x) list(input = path_list_to_vector(x[["data_input"]]), 
                                        output = path_list_to_vector(x[["data_output"]])))
  names(res) = all_nm
  
  res
}


####
#### Core internal ####
####


lazy_run = function(all_chunks, dep_mat, env_rprofile, comment = FALSE, ask = TRUE){
  # main evaluation function
  # runs only what is necessary
  # writes in the textfile directly if asked for (and needed).

  # !! beware the "project path" if the user provided the path argument (to run rmake in a separate folder)

  if(!dir.exists(root_path(".rmake"))){
    dir.create(root_path(".rmake"))
  }
  
  if(file.exists(root_path(".rmake/rmake.rds"))){
    info = readRDS(root_path(".rmake/rmake.rds"))
  } else {
    info = list()
  }


  #
  # Finding out which chunk to update
  #

  # If a chunk is up to date, we copy some variables from the old chunk,
  # the one that was run
  vars_copy = c(paste0("time_", c("start", "end", "run", "run_fmt")),
               "session")

  n = length(all_chunks)
  chunk_up_to_date = rep(FALSE, n)

  info.code_hash = sapply(info, `[[`, "code_hash")

  # it will be a matrix with the cause of the re-run
  n_chunks = length(all_chunks)
  info_cause = matrix(FALSE, n_chunks, 6)
  colnames(info_cause) = c("new", "code", "fun", "input", "output", "indirect")

  is_empty = rep(FALSE, n_chunks)

  for(i in 1:n_chunks){
    var = names(all_chunks)[i]
    chunk = all_chunks[[i]]

    if(chunk$code_n_expr == 0){
      chunk_up_to_date[i] = TRUE
      is_empty[i] = TRUE
      next
    }

    # Does the chunk exist already? Could it be renamed?
    if(var %in% names(info)){
      chunk_old = info[[var]]

    } else {
      # Isn't the chunk just an existing chunk renamed?
      qui = which(info.code_hash == chunk$code_hash)
      if(length(qui) == 1){
        chunk_old = info[[qui]]

      } else {
        if(length(qui) > 1){
          pblm = names(info)[qui]
          warni("There exists several identical chunks... Please debug (if needed). ",
                "They are:\n{'\n'c ? pblm}")
          # we skip the chunk when it matches several
        }

        # No existing chunk, we go next
        info_cause[i, "new"] = TRUE

        next
      }
    }

    # Cause of the non-updatedness
    is_diff_code = chunk$code_hash != chunk_old$code_hash
    is_diff_fundep = !identical(chunk$fundep, chunk_old$fundep)
    is_diff_input = !is_same_input_files(chunk, chunk_old)
    is_diff_output = !is_same_output_files(chunk, chunk_old)

    if(!(is_diff_code || is_diff_fundep || is_diff_input || is_diff_output)){
      chunk_up_to_date[i] = TRUE
    }

    # we save the previous run-time information
    # DEV: check everything is OK (that I don't carry on the run times forever)
    for(v in vars_copy){
      if(!is.null(chunk_old[[v]])){
        chunk[[v]] = chunk_old[[v]]
      }
    }

    all_chunks[[var]] = chunk

    # We save the information
    info_cause[i, 2:5] = c(is_diff_code, is_diff_fundep, is_diff_input, is_diff_output)

  }
  
  # we look at the dependencies: so that one chunk that is rerun can lead to many reruns
  chunk_rerun = which(!chunk_up_to_date)
  all_chunks_run = chunk_rerun
  chunk_rerun_indirect = c()
  next_chunks = chunk_rerun
  while(length(next_chunks) > 0){
    next_chunks = which(colSums(dep_mat[next_chunks, , drop = FALSE]) > 0)
    
    if(length(next_chunks) > 0){
      all_chunks_run = c(all_chunks_run, next_chunks)
      chunk_rerun_indirect = c(chunk_rerun_indirect, next_chunks)
    }
  }

  chunk_up_to_date[chunk_rerun_indirect] = FALSE
  info_cause[chunk_rerun_indirect, 6] = TRUE

  # we transform it into a DF
  core = data.frame(file = sapply(all_chunks, `[[`, "filename"),
                    chunk = sapply(all_chunks, `[[`, "chunk_name"))
  cause_raw = cbind(core, info_cause)

  # verbose
  verbose = TRUE
  if(all(chunk_up_to_date)){

    if(verbose){
      message("All code chunks are up to date.")
    }

    return(invisible(NULL))
  }

  if(TRUE){

    cause_fmt = as.matrix(cause_raw[!chunk_up_to_date, , drop = FALSE])
    cause_fmt = gsub("^TRUE$", "X", cause_fmt)
    cause_fmt = gsub("^FALSE$", "", cause_fmt)

    mema("The following chunk{#s, need ? nrow(cause_fmt)} to be run:")

    cause_fmt = rbind(colnames(cause_fmt), cause_fmt)

    for(i in 1:2){
      cause_fmt[, i] = format(cause_fmt[, i], justify = "l")
    }

    for(i in 3:8){
      cause_fmt[, i] = format(cause_fmt[, i], justify = "c")
    }

    msg = apply(cause_fmt, 1, paste0, collapse = " ")
    msg = paste(msg, collapse = "\n")
    message(msg)

    if(ask){
      qui = !chunk_up_to_date & names(all_chunks) %in% names(info) & !is_empty
      if(any(qui)){
        all_times = sapply(all_chunks[qui], `[[`, "time_run")
        total_time = sum(sapply(all_times, function(x) as.double(x, "secs")))
        message("Previous run-time for ", length(all_times),
                " existing chunk", plural_len(all_times), ": ", difftime_fmt(total_time))
      }

      m = readline("Proceed (y, yes; anything else: no)?  ")
      if(!tolower(trimws(m)) %in% c("y", "ye", "yes")){
        return(invisible(NULL))
      }
    }

  }

  if(all(chunk_up_to_date)){
    return(invisible(NULL))
  }

  #
  # Finding out the order of the evaluation
  #

  update_mat = dep_mat

  # 1) we clean the up to date
  for(i in which(chunk_up_to_date)){
    update_mat[i, ] = 0
  }

  # This is to write the output directly in the document
  # we use a kind of global variable.
  # we write "on exit" to avoid problems
  options(rmake_TEXT_MODIF = list())

  # Since we write in the documents, we set a width that fits a document
  width_origin = getOption("width")
  on.exit({
    write_output_in_doc(comment)
    if(getOption("width") == 80) options(width = width_origin)
  })

  options(width = 80)

  # information needed in run_chunk()
  env_list = list(RPROFILE = env_rprofile)
  info_all = list()
  # the data is strongly formatted, no error possible
  info_all$filenames = sapply(all_chunks, `[[`, "filename")
  info_all$id_sec    = sapply(all_chunks, `[[`, "id_sec")

  # 2) we update
  IS_ERROR = FALSE
  while(!all(chunk_up_to_date) && !IS_ERROR){
    # LATER: we may add priority to a file instead of running all the possible candidates
    # => this can be an option (although does not seem super important)

    # we pick the first without depends
    n_dep = colSums(update_mat)
    candidates = which(n_dep == 0 & !chunk_up_to_date)
    if(length(candidates) == 0){
      # LATER: detail which
      stop("There exists circular dependencies in your code.\nWIP: tell which")
    }

    for(i in candidates){
      info_eval = tryCatch(run_chunk(all_chunks, i, env_list, info_all),
                           error = function(e) structure(conditionMessage(e), class = "try-error"))

      if(inherits(info_eval, "try-error")){
        # We save the chunks that have been run before the error occurred
        all_chunks = all_chunks[which(chunk_up_to_date)]
        IS_ERROR = TRUE
        error_msg = info_eval

        break
      }

      env_list = info_eval$env_list
      all_chunks = info_eval$all_chunks

      chunk_up_to_date[i] = TRUE
      update_mat[i, ] = 0

    }

  }

  # We update the timestamps, so that when rmake() is next run, everything
  # will have the proper mtimes
  # why is it needed?
  # C1: I(path_a):21h00
  # C2: O(path_a)
  # We run rmake(hard = TRUE) at 22h00
  # C2 is run, leading to a modification of the mtime of path_a, affecting the mtimes of C1
  #
  # note: I need seq_along since all_chunks can be NULL if error above
  for(i in seq_along(all_chunks)){
    chunk = all_chunks[[i]]
    chunk$input_timestamps  = path_timestamp(chunk$data_input)
    chunk$input_sizestamps  = path_sizestamp(chunk$data_input)
    chunk$output_timestamps = path_timestamp(chunk$data_output)
    chunk$output_sizestamps = path_sizestamp(chunk$data_output)
    all_chunks[[i]] = chunk
  }

  # --> ATTENTION: que se passe t-il quand il y a une erreur????
  # est-ce que je dois sauver tous les all_chunks?
  # ou seulement ceux qui sont U2D ou qui ont ete lances avec succes?
  # OK: I've done that already!!!

  # Maybe, if there is an error, the best is to save the old values for the chunks that
  # have not been updated.

  # saving
  info = all_chunks
  class(info) = "rmake"
  saveRDS(info, root_path(".rmake/rmake.rds"))

  if(IS_ERROR){

    # if(requireNamespace("rstudioapi", quietly = TRUE)){
    #   f = gsub(".+in file : ([^\n]+)\n.+", "\\1", error_msg)
    #   line = as.numeric(gsub(".+at line : (\\d+)\n.+", "\\1", error_msg))
    #   rstudioapi::navigateToFile(f, line = line)
    # }

    stop_up(error_msg)
  }

}

run_chunk = function(all_chunks, id_chunk, env_list, info_all){
  # we need to run the preamble and the main subsection
  # all in a given environment

  filenames = info_all$filenames
  id_sec = info_all$id_sec

  chunk = all_chunks[[id_chunk]]
  sec = chunk$id_sec
  f = chunk$filename

  #
  # preparing the environment
  #
  
  # This is the init of the file
  env = new.env(parent = env_list$RPROFILE)
  env_list[[chunk$NAME]] = env

  if(chunk$code_n_expr == 0){
    res = list(env_list = env_list, all_chunks = all_chunks)
    return(res)
  }

  message(chunk$chunk_name_long, " ... ", appendLF = FALSE)

  #
  # Adding the line information
  #

  code_line_start = chunk$code_lines[1] - 1
  valid_code_lines = cpp_valid_code_index(chunk$code_text)
  line_valid = valid_code_lines$line_nb
  line_span = valid_code_lines$size

  n_lines = length(chunk$code_text)
  n_valid = length(line_valid)
  CODE_LINES = rep(chunk$code_text, (1:n_lines %in% line_valid) + 1)
  CODE_LINES[line_valid + cumsum(rep(1, n_valid)) - 1] = paste0("LINE_COUNTER = ", code_line_start + line_valid, " ; LINE_SPAN = ", line_span)

  #
  # Adding the "for" information
  #

  # We only tackle the most unnested "for" loops, the ones at "^for"
  # LIMITATIONS:
  # - text literals that include "^for" will lead to errors
  # - the closing "for" bracket can be wrongly attributed if:
  #   * the user formats its code poorly, in particular write "^   }"
  #     when it should be "^}" and vice versa
  #   * there are some "^}" nested in string literals, although it's unlikely
  #
  # => the limitation is not so dangerous since it only affects reporting.
  #    I may write a parser at some point (but I'm tired of doing that...).

  for_start = which(grepl("^for\\(", CODE_LINES))
  if(length(for_start) == 0){
    CODE_LINES_FOR = CODE_LINES
  } else {
    bracket_close = which(grepl("^\\}", CODE_LINES))
    for_end = c()
    for(i in for_start){
      for_end = c(for_end, bracket_close[which.max(bracket_close > i)])
    }

    n = length(CODE_LINES)
    my_rep = rep(1, n)
    my_rep[c(for_start, for_end)] = 2

    CODE_LINES_FOR = rep(CODE_LINES, my_rep)
    n_loop = length(for_start)
    CODE_LINES_FOR[for_start + 2 * (0:(n_loop - 1))] = "INSIDE_LOOP = TRUE ; INDEX_LOOP = list()"
    CODE_LINES_FOR[for_end + 2 * 1:n_loop] = "INSIDE_LOOP = FALSE"

    qui_for = which(grepl("\\bfor\\(", CODE_LINES_FOR))
    index = gsub("^.*for\\(([^ ]+).+", "\\1", CODE_LINES_FOR[qui_for])

    n = length(CODE_LINES_FOR)
    my_rep = rep(1, n)
    my_rep[qui_for] = 2

    CODE_LINES_FOR = rep(CODE_LINES_FOR, my_rep)
    n_loop = length(qui_for)
    CODE_LINES_FOR[qui_for + 1 + (0:(n_loop - 1))] = paste0("INDEX_LOOP[[\"", index, "\"]] = ", index)
  }
  
  #
  # Evaluation
  #

  parsed_code = parse(text = CODE_LINES_FOR, keep.source = FALSE)

  # We sink on the go
  # LATER: add an option if the user doesn't want to sink

  TEXT_MODIF = getOption("rmake_TEXT_MODIF")

  time_start = Sys.time()

  n_expr = length(parsed_code)
  for(i in 1:n_expr){
    # We need a line per line eval for the sinking.
    expr = parsed_code[[i]]
    # We don't silence the error => it will be useful if written in the document
    # output = try(capture.output(eval(expr, env)), silent = TRUE)
    output = try(capture_print_msg(eval(expr, env)), silent = TRUE)

    # BEGIN: error handling
    IS_ERROR = inherits(output, "try-error")
    if(IS_ERROR){
      line_fail = get("LINE_COUNTER", env)
      inside_loop = env$INSIDE_LOOP

      chunk_loc = paste0(" (", n_th(sec), " chunk)")

      error_msg = c("Run failed\n",
                    " - in file : ", f, "\n",
                    " - in chunk: ", chunk$chunk_name, chunk_loc, "\n",
                    " - at line : ", line_fail)

      if(isTRUE(inside_loop)){
        index_loop = get("INDEX_LOOP", env)
        index_names = names(index_loop)
        get_val = function(x){
          if(is.atomic(x) && length(x) == 1){
            x
          } else {
            paste0(deparse(x, width.cutoff = 20)[1], "...")
          }
        }
        index_values = sapply(index_loop, get_val)
        msg = paste0(index_names, ": ", index_values, collapse = ", ")
        error_msg = c(error_msg, paste0("\nLoop values: ", msg, "."))
      }

      deparse_short = function(x){
        res = deparse(x)
        if(length(res) > 2){
          res = head(res, 2)
          res[2] = paste0(res[2], " ... etc")
        }

        res
      }

      error_msg = c(error_msg,
                    "\nAll variables are assigned to the global environment for debugging.",
                    "\n| The expression evaluated is:\n", paste0("| ", deparse_short(expr)),
                    "\n| Leading to the following error:\n", paste0("| ", output))
      
      
      #
      # We assign the variables to the global env to facilitate debugging
      for(var in names(env)){
          # I can use parent.frame(n) with n = 3 + (sec > 0) + (subsec > 0)
          # if .GlobalEnv does not comply with the CRAN policies
          assign(var, get(var, env), .GlobalEnv)
        }

      # We stop only after writing in the document
    }
    #   END: error handling

    # Results from the output
    if(length(output) > 0 && !get_op(expr) %in% c("for", "while", "if", "else")){
      # we fetch the console output
      info = output
      # we comment the line if the output is not already commented
      info = gsub("^(?=[^#])", "#> ", info, perl = TRUE)
      line_start = get("LINE_COUNTER", env)
      line_span = get("LINE_SPAN", env)

      line_nb = line_start + (line_span - 1)

      # output with many special characters create sections
      # => we avoid that by adding another special char at the end
      qui_sec_pblm = grepl("(----|####) *$", info)
      info[qui_sec_pblm] = gsub("(-|#) *$", "\\1 \\1", info[qui_sec_pblm])

      # fix the data table dt[, new := old] print bug
      if(get_op(expr) == "[" && ":=" %in% all.names(expr)){
          next
      }

      line_chunk = line_nb + - chunk$code_lines[1] + 1

      # is it the same output as some output already in the code?
      n_info = length(info)
      done = FALSE
      if(chunk$code_lines[2] >= line_nb + n_info){
        # NOTE: I don't substract 1 b/c the console-output is ALWAYS after
        #       the code line and line_nb is the code line

        # Same reason why I add a 1 in the index here
        origin_text = chunk$code_text[line_chunk + 0:n_info + 1]
        if(all(origin_text[-1] == info)){
          # We just need to update the first line
          value = rmake_output_stamp()
          done = TRUE
        }
      }

      if(IS_ERROR){
        stop(error_msg)
      }

      if(!done){
        # we "push" the text
        value = paste0(rmake_output_stamp(), "\n",
                       paste0(info, collapse = "\n"), "\n",
                       chunk$code_text[line_chunk + 1])
      }

      TEXT_MODIF[[length(TEXT_MODIF) + 1]] = list(filename = f, at = line_nb + 1, text = value)

    }
  }

  time_end = Sys.time()
  time_run = time_end - time_start
  time_run_fmt = difftime_fmt(time_run)

  message(time_run_fmt)

  chunk$time_start = time_start
  chunk$time_end = time_end
  chunk$time_run = time_run
  chunk$time_run_fmt = time_run_fmt
  chunk$session = get_session()

  all_chunks[[id_chunk]] = chunk

  options(rmake_TEXT_MODIF = TEXT_MODIF)

  res = list(env_list = env_list, all_chunks = all_chunks)
  return(res)
}

dependency_graph = function(all_chunks){
  # We create a graph
  # Ideally it should also contain functions
  # We end up with a simple matrix representing a directed graph

  path_norm = function(x){
    if(length(x) == 0) return(list())
    absolute_to_relative(x)
  }

  dep_norm = function(x){
    if(length(x) == 0) return(list())
    sapply(x, function(y)
      if(grepl("(?i)cpp$", y$source))
        paste0("cpp@", y$source)
      else
        paste0("source@", y$source, "/", y$fun))
  }

  n = length(all_chunks)
  input = output = fundep = vector("list", n)
  all_names = names(all_chunks)
  for(i in 1:n){
    chunk = all_chunks[[i]]

    data_in = chunk$data_input
    paths = unlist(lapply(data_in, `[[`, "path"))
    input[[i]] = path_norm(paths)

    data_out = chunk$data_output
    paths = unlist(lapply(data_out, `[[`, "path"))
    output[[i]] = path_norm(paths)

    dep = chunk$function_depends
    fundep[[i]] = dep_norm(dep)
  }

  # Normalizing the names (removing project path from the names)
  path_proj = getOption("rmake_root_path")
  all_names = gsub(path_proj, "~", all_names, fixed = TRUE)

  # Extracting the dependencies from the sources
  all_fundep = unlist(fundep)
  source_files = gsub("^[^@]+@|/[^/]+$", "", all_fundep)
  source_unik = unique(unname(source_files))
  src_dep = c()
  for(src in source_unik){
    src_dep = c(src_dep, source_dependencies(src))
  }
  
  if(!is.null(src_dep)){
    names(src_dep) = gsub("=.+", "", src_dep)
  }

  n = length(input)

  # Dependency: Function links
  # => we add the information on the function dependencies directly in the chunk
  qui = which(lengths(fundep) > 0)
  for(i in qui){

    if(!all(fundep[[i]] %in% names(src_dep))){
      stop("Internal error: one function dependency could not be found.")
    }

    all_chunks[[i]]$fundep = src_dep[fundep[[i]]]
  }

  # We add the path_timestamps
  session = get_session()
  for(i in 1:n){
    chunk = all_chunks[[i]]
    chunk$session = session
    chunk$input_timestamps = path_timestamp(chunk$data_input)
    chunk$input_sizestamps = path_sizestamp(chunk$data_input)
    chunk$output_timestamps = path_timestamp(chunk$data_output)
    chunk$output_sizestamps = path_sizestamp(chunk$data_output)
    all_chunks[[i]] = chunk
  }

  #
  # Dependency: root data
  root_input = setdiff(unlist(input), unlist(output))
  if(length(root_input) > 0){
    qui = input %inlist% root_input
    for(i in qui){
      all_roots = list()
      all_root_paths = intersect(input[[i]], root_input)
      for(p in all_root_paths){
        tmp = list(path = p, path_rel = absolute_to_relative(p))
        tmp$exists = file.exists(p)
        if(tmp$exists){
          fi = file.info(p)
          tmp$size = fi$size
          tmp$mtime = fi$mtime
        }
        all_roots[[length(all_roots) + 1]] = tmp
      }

      all_chunks[[i]]$root_input = all_roots
    }
  }

  # Dependency: Data links
  
  all_files = c(unlist(input), unlist(output))
  dict_files = sort(unique(all_files))
  dict_files = setNames(seq_along(dict_files), dict_files)
  n_files = length(dict_files)

  mat_input = matrix(0, n, n_files)
  chunk_in = rep(1:n, lengths(input))
  index_in = cbind(chunk_in, dict_files[unlist(input)])
  mat_input[index_in] = 1

  mat_output = matrix(0, n, n_files)
  chunk_out = rep(1:n, lengths(output))
  index_out = cbind(chunk_out, dict_files[unlist(output)])
  mat_output[index_out] = 1

  dep_mat = tcrossprod(mat_output, mat_input)

  dimnames(dep_mat) = list(all_names, all_names)
  
  res = list(dep_mat = dep_mat, all_chunks = all_chunks)

  res
}




create_chunks_old = function(text, fun_list = list(), env, pkg_all = NULL, filename = NULL){

  subsection_id = which(grepl("^####? [^#]+####\\s*", text))

  if(length(subsection_id) == 0) return(NULL)

  num = function(x) gsub(" ", "0", format(x, width = 2))

  preamble = text[1:subsection_id[1]]
  is_section = grepl("^####", text[subsection_id])
  section_id = which(is_section)

  section_ignore = grepl("^####? *\\$", text[subsection_id])
  subsection_names = gsub("^####? [^[:alnum:]]*|\\s*####\\s*", "", text[subsection_id])
  section_names = subsection_names[is_section]

  n_subsec = length(subsection_names)

  section_end = c(subsection_id[-1] - 1, length(text))

  # The preamble
  preamble_text = text[1:subsection_id[1]]
  preamble_code = try(parse(text = preamble_text, keep.source = FALSE))
  preamble_char = deparse_code(preamble_code)

  fun_list = extract_source_functions(preamble_text, fun_list, env, pkg_all)

  pkg_preamble = find_packages(preamble_text)
  pkg_all = unique(c(pkg_all, pkg_preamble))

  preamble_parsed = extract_code_info(preamble_text,
                                      line_nb = 1, fun_list = fun_list, env = env)
  filename_rel = absolute_to_relative(filename)
  preamble_parsed$filename = filename_rel
  preamble_parsed$code_hash = cpp_hash_string(paste(trimws(deparse(preamble_code, width.cutoff = 500)), collapse = ""))
  preamble_parsed$code_lines = c(1, subsection_id[1])
  preamble_parsed$code_text = preamble_text
  preamble_parsed$code_n_expr = length(preamble_code)
  preamble_parsed$id_sec = 0
  preamble_parsed$id_subsec = 0
  preamble_parsed$NAME = paste0(filename_rel, "@00_00")
  preamble_parsed$NAME = paste0(filename_rel, "@00_00")
  preamble_parsed$chunk_name = "Init (very firt chunk, always unnamed)"
  preamble_parsed$chunk_name_long = paste0(filename_rel, "@00_00: INIT")

  res = list("00_INIT" = preamble_parsed)
  i_sec = 1
  i_subsec = 0
  fun_list_init = fun_list_sec = fun_list
  pkg_all_init = pkg_all_sec = pkg_all
  preamble_init = preamble_char

  for(i in 1:n_subsec){

    if(section_ignore[i]) next

    sec_text = text[subsection_id[i]:section_end[i]]

    # We gather the sources in a section specific way.
    fun_list_sec = extract_source_functions(sec_text, fun_list_init, env, pkg_all)
    sec_text = sec_text[!grepl("\\bsource(Cpp)?\\(", sec_text)]
    if(i_subsec == 0){
      fun_list_init = fun_list_sec
      pkg_all_init = pkg_all_sec
    }

    # DEV: add precise information when the code cannot be parsed
    # - file of the problm + chunk name
    # - line number at which there is a problem

    code = parse(text = sec_text, keep.source = FALSE)

    sec_parsed = extract_code_info(code, line_nb = 1, fun_list = fun_list_sec, env = env,
                                   extra_code_char = preamble_init, pkg_all = pkg_all_sec)

    if(i_subsec == 0 && !(i + 1) %in% section_id && !i == n_subsec){
      # We modify the preamble for the future subsections
      code_char = deparse_code(code)
      preamble_init = c(code_char, preamble_char)
    }

    sec_parsed$filename = filename_rel
    sec_parsed$code_hash = cpp_hash_string(paste(trimws(deparse(code, width.cutoff = 500)), collapse = ""))
    sec_parsed$code_lines = c(subsection_id[i], section_end[i])
    sec_parsed$code_text = sec_text
    sec_parsed$code_n_expr = length(code)
    sec_parsed$id_subsec = i_subsec
    sec_parsed$id_sec = i_sec
    sec_parsed$NAME = paste0(filename_rel, "@", sprintf("%02i_%02i", i_sec, i_subsec))
    sec_parsed$chunk_name = subsection_names[i]
    sec_parsed$chunk_name_long = paste0(sec_parsed$NAME, ": ", subsection_names[i])

    nm = paste0(num(i_sec), "_", num(i_subsec), "_", subsection_names[i])

    res[[nm]] = sec_parsed
    i_subsec = i_subsec + 1

    if((i + 1) %in% section_id || i == n_subsec){
      i_sec = i_sec + 1
      i_subsec = 0
      # we reset the list of functions
      fun_list_init = fun_list
      pkg_all_init = pkg_all
    }

  }

  return(res)

}


create_chunks = function(path, fun_list = list(), env, pkg_all = NULL){
  
  all_chunks = cpp_create_chunks(path)
  
  n_sec = length(all_chunks)
  
  if(n_sec == 0){
    return(NULL)
  }
  
  filename_rel = absolute_to_relative(path)

  res = list()

  for(i in 1:n_sec){
    
    section = all_chunks[[i]]

    sec_text = section$text

    # We gather the sources in a section specific way.
    fun_list_sec = extract_source_functions(sec_text, fun_list, env, pkg_all)
    sec_text = sec_text[!grepl("\\bsource(Cpp)?\\(", sec_text)]

    # DEV: add precise information when the code cannot be parsed
    # - file of the problm + chunk name
    # - line number at which there is a problem

    code = parse(text = sec_text, keep.source = FALSE)

    sec_parsed = extract_code_info(code, line_nb = 1, fun_list = fun_list_sec, env = env,
                                   extra_code_char = NULL, pkg_all = pkg_all)

    sec_parsed$filename = filename_rel
    sec_parsed$code_hash = cpp_hash_string(paste(trimws(deparse(code, width.cutoff = 500)), collapse = ""))
    sec_parsed$code_lines = c(section$line_start, section$line_end)
    sec_parsed$code_text = sec_text
    sec_parsed$code_n_expr = length(code)
    sec_parsed$id_sec = i
    sec_parsed$NAME = paste0(filename_rel, "@", sprintf("%02i", i))
    sec_parsed$chunk_name = section$title
    sec_parsed$chunk_name_long = paste0(sec_parsed$NAME, ": ", section$title)

    nm = sma("{%02i ? i}_{section$title}")

    res[[nm]] = sec_parsed

  }

  return(res)

}


####
#### utilities ####
####


capture_print_msg = function(expr){
    # function that captures both the stuff printed regularly on the console
    # and the messages (not the same channel)
    capture.output(
        withCallingHandlers(
            expr,
            message = function(m){
                cat(conditionMessage(m), sep = "\n")
                tryInvokeRestart("muffleMessage")
            }
        )
    )
}

write_output_in_doc = function(comment){
  # Writes the output of the console into the document containing the code

  if(!comment) return()

  TEXT_MODIF = getOption("rmake_TEXT_MODIF")

  all_files = list()
  for(value in TEXT_MODIF){
    f = value$filename
    if(f %in% names(all_files)){
      text = all_files[[f]]
    } else {
      text = readLines(f)
      # This is super special case: to avoid a bug if the output is to be written
      # at the very last non-code line of the text and it does not exist
      if(nchar(tail(text, 1)) != 0) text = c(text, "", "")
      all_files[[f]] = text
    }

    text[value$at] = value$text
    all_files[[f]] = text
  }

  for(i in seq_along(all_files)){
    f = names(all_files)[i]
    backup_file(f)

    writeLines(all_files[[i]], f)
  }
}

extract_code_info = function(code, line_nb = 1, fun_list = NULL,
                             env = new.env(parent = baseenv()),
                             extra_code_char = NULL, pkg_all = NULL){
  # code = parsed code
  # fun_list: list of functions that are known to be loaded (i.e. sourced)

  update_values = function(info){
    # we update the value of res from the multi line information
    for(var in c("data_input", "data_output", "function_depends", "assigned", "used")){
      new = info[[var]]
      if(is.null(new)) next
      old = get(var, parent.frame())

      if(var %in% c("function_depends", "assigned", "used")){
        qui = setdiff(names(new), names(old))
        if(length(qui) > 0){
          old[qui] = new[qui]
          assign(var, old, parent.frame())
        }
      } else {
        res = append(old, new)
        assign(var, res, parent.frame())
      }
    }

    line_nb = get("line_nb", parent.frame())
    line_nb = info$parsed_codelines
    assign("line_nb", line_nb, parent.frame())
  }

  IS_ORIGIN = line_nb == 1

  fun_vec = unlist(fun_list)
  assigned = used = c()
  data_input = data_output = list()
  function_depends = list()
  res = list()
  for(i in seq_along(code)){

    line = code[[i]]

    if(length(line) == 1) next

    op = deparse_long(line[[1]])

    if(op %in% c("source", "sourceCpp", "Rcpp::sourceCpp")){
      # I should have extracted the sources before
      # se we should not end here
      path = line[[2]]
      if(!path %in% names(fun_list)){
        fun_list[[path]] = funs_from_source(path)
        fun_vec = unlist(fun_list)
      }

    } else if(op %in% c("while", "for", "if")){

      if(op == "for"){
        index = deparse(line[[2]])
        assigned[index] = line_nb
        for(v in setdiff(all.vars(line[[3]]), names(used))){
          used[v] = line_nb
        }

        info = extract_code_info(line[[4]], line_nb = line_nb + 1, fun_list, env = env)
      } else if(op == "if"){

        for(v in setdiff(all.vars(line[[2]]), names(used))){
          used[v] = line_nb
        }

        info = extract_code_info(line[[3]], line_nb = line_nb + 1, fun_list, env = env)

        if(length(line) == 4){
          # The else
          update_values(info)
          info = extract_code_info(line[[4]], line_nb = line_nb + 1, fun_list, env = env)
        }
      } else {
        # while
        for(v in setdiff(all.vars(line[[2]]), names(used))){
          used[v] = line_nb
        }

        info = extract_code_info(line[[3]], line_nb = line_nb + 1, fun_list, env = env)
      }

      # update res
      update_values(info)

    } else if(op %in% c("=", "<-")){
      var = deparse(line[[2]])
      if(!grepl("(", var, fixed = TRUE) && !var %in% names(assigned)){
        assigned[var] = line_nb
      }

      if(is_read_fun(line)){
        data_input[[length(data_input) + 1]] = extract_rw_path(line, read = TRUE, env = env)
      }

      for(v in setdiff(all.vars(line[[3]]), names(used))){
        used[v] = line_nb
      }

      new_depends = intersect(all.names(line[[3]]), fun_vec)
      new_depends = setdiff(new_depends, names(function_depends))
      for(var in new_depends){
        for(j in seq_along(fun_list)){
          if(var %in% fun_list[[j]]){
            function_depends[[var]] = list(fun = var, source = names(fun_list)[j], 
                                           type = "assigned")
          }
        }
      }

    } else if(is_read_fun(line)) {
      data_input[[length(data_input) + 1]] = extract_rw_path(line, read = TRUE, env = env)

    } else if(is_write_fun(line)){
      data_output[[length(data_output) + 1]] = extract_rw_path(line, write = TRUE, env = env)

    } else {

      for(v in setdiff(all.vars(line), names(used))){
        used[v] = line_nb
      }

      new_depends = intersect(all.names(line), fun_vec)
      new_depends = setdiff(new_depends, names(function_depends))
      for(var in new_depends){
        for(j in seq_along(fun_list)){
          if(var %in% fun_list[[j]]){
            function_depends[[var]] = list(fun = var, source = names(fun_list)[j], type = "unassigned")
          }
        }
      }
    }

    line_nb = line_nb + 1
  }

  # We try to evaluate the data input/output based on non varying indices
  code_char = NULL
  if(IS_ORIGIN){

    for(rw in c("read", "write")){

      rw_data = if(rw == "read") data_input else data_output
      rw_ing = if(rw == "read") "reading" else "writing"

      for(i in seq_along(rw_data)){

        data_i = rw_data[[i]]

        if(!data_i$must_eval) next

        if(is.null(code_char)){
          code_char = c(extra_code_char, deparse_code(code))
        }

        pat = deparse_long(data_i$fun_call)

        line_id = which(grepl(pat, code_char, fixed = TRUE))

        if(length(line_id) != 1){
          msg = if(length(line_id) == 0) "the code corresponding to the __export__ path could not be found." else "multiple lines of code found for the __export__ path line."
          msg = gsub("__export__", rw_ing, msg)
          stop("Internal error: ", msg, " => Please revise code.")
        }

        code_char_new = code_char
        vars = get_vars_dsb(data_i$path_call)
        pattern = paste0("[^\n]*\\Q", pat, "\\E")
        code_char_new[[line_id]] = gsub(pattern, paste0("TARGET__ = ", 
                                                        paste(vars, collapse = " + "), 
                                                        " + END_TARGET__()"),
                                        code_char_new[[line_id]], perl = TRUE)

        code_new = parse(text = code_char_new, keep.source = FALSE)

        code_extract = crawl_for_vars(code_new, "TARGET__")

        vars_ok = code_extract$vars_ok

        reamaining_vars = names(vars_ok)[!vars_ok]

        if(length(reamaining_vars) > 0){
          # Are the remaining vars in the environment?
          env_vars = ls(env)

          pblm = setdiff(reamaining_vars, env_vars)

          if(length(pblm) > 0){
            stop("The ", rw_ing, " paths can be based on variables, but these must be hard values. ",
                 "This is not the case for ", pat,
                 ". \n => Sorry the algorithm cannot work, the variable", enumerate_items(pblm, "s.quote"),
                 " could not be isolated.")
          }
        }

        # now the evaluation
        code_new_char = deparse_code(code_extract$code[[1]])
        code_new_char = gsub("TARGET__.+END_TARGET__\\(\\)",
                             paste0("PATH_ALL__[[length(PATH_ALL__) + 1]] = ",
                                    deparse_long(data_i$path_call)),
                             code_new_char)

        code_new_char = add_namespace(code_new_char, env, pkg_all)

        eval_env = new.env(parent = env)
        assign("PATH_ALL__", character(0), eval_env)

        code_minified = parse(text = code_new_char, keep.source = FALSE)
        eval(code_minified, eval_env)

        all_paths = get("PATH_ALL__", eval_env)
        data_i$path = all_paths

        if(rw == "read"){
          data_input[[i]] = data_i
        } else {
          data_output[[i]] = data_i
        }
      }
    }
  }


  res = list(data_input = data_input, data_output = data_output, 
             function_depends = function_depends,
             assigned = assigned, used = used, parsed_codelines = line_nb - 1)

  res
}


crawl_for_vars = function(code, vars){

  if(length(vars) == 0){
    return(list(code = quote({}), vars_ok = character(0)))
  }

  vars_current = vars
  vars_ok = rep(FALSE, length(vars))
  names(vars_ok) = vars
  index = c()

  cont = TRUE
  while(cont){
    cont = FALSE
    # we restart every time we need

    for(i in seq_along(code)){
      line = code[[i]]

      if(any(vars_current %in% all.vars(line))){

        if(length(line) == 1) next

        op = deparse_long(line[[1]])

        if(op %in% c("=", "<-")){
          var_assigned = deparse_long(line[[2]])

          if(!var_assigned %in% vars_current){
            next

          } else {
            vars_ok[[var_assigned]] = TRUE
            index = c(index, i)

            vars_used = all.vars(line[[3]])
            todo = setdiff(vars_used, names(vars_ok))
            for(v in todo){
              vars_ok[[v]] = FALSE
            }

            vars_current = names(vars_ok)[!vars_ok]

            if(!all(vars_ok)){
              cont = TRUE
            }

            if(length(todo) > 0) break

          }

        } else if(op == "for"){
          var_assigned = deparse_long(line[[2]])

          line_ok = any_new_vars = FALSE

          if(var_assigned %in% vars_current){
            vars_ok[[var_assigned]] = TRUE
            line_ok = TRUE
          }

          # we update the line
          info = crawl_for_vars(line[[4]], names(vars_ok)[!vars_ok])

          if(any(info$vars_ok)){
            line_ok = TRUE
            line[[4]] = info$code[[1]]
            new_vars = setdiff(names(info$vars_ok), names(vars_ok))
            any_new_vars = length(new_vars) > 0 && any(!info$vars_ok[new_vars])
            for(v in names(info$vars_ok)){
              # we update the ones done
              vars_ok[[v]] = info$vars_ok[[v]]
            }
            code[[i]] = line
          }

          if(line_ok){
            index = c(index, i)

            # Since we use the loop, we add the variables that were used in it
            vars_used = all.vars(line[[3]])
            todo = setdiff(vars_used, names(vars_ok))
            for(v in todo){
              vars_ok[[v]] = FALSE
            }

            vars_current = names(vars_ok)[!vars_ok]

            if(!all(vars_ok)){
              cont = TRUE
            }

            if(any_new_vars) break
          }


        } else if(op == "if"){

          line_ok = any_new_vars = FALSE

          # we update the line
          info = crawl_for_vars(line[[3]], names(vars_ok)[!vars_ok])
          if(any(info$vars_ok)){
            line_ok = TRUE

            line[[3]] = info$code[[1]]
            new_vars = setdiff(names(info$vars_ok), names(vars_ok))
            any_new_vars = length(new_vars) > 0 && any(!info$vars_ok[new_vars])
            for(v in names(info$vars_ok)){
              # we update the ones done
              vars_ok[[v]] = vars_ok[[v]] || info$vars_ok[[v]]
            }

            code[[i]] = line
          }

          if(length(line) == 4){
            info = crawl_for_vars(line[[4]], names(vars_ok)[!vars_ok])
            if(any(info$vars_ok)){
              line_ok = TRUE

              line[[4]] = info$code[[1]]
              new_vars = setdiff(names(info$vars_ok), names(vars_ok))
              any_new_vars = any_new_vars || (length(new_vars) > 0 && any(!info$vars_ok[new_vars]))
              for(v in names(info$vars_ok)){
                # we update the ones done
                vars_ok[[v]] = vars_ok[[v]] || info$vars_ok[[v]]
              }
              code[[i]] = line
            }
          }

          if(line_ok){
            index = c(index, i)

            vars_used = all.vars(line[[2]])
            todo = setdiff(vars_used, names(vars_ok))
            for(v in todo){
              vars_ok[[v]] = FALSE
            }

            vars_current = names(vars_ok)[!vars_ok]

            if(!all(vars_ok)){
              cont = TRUE
            }

            if(any_new_vars) break
          }

        } else if(op == "while"){
          stop("Internal error: dynamic writing path variables found in a 'while': please use a 'for' instead.")

        } else {
          # nothing
          next
        }

      }
    }
  }

  index = sort(unique(index))

  new_code = quote({})
  j = 2
  for(i in sort(index)){
    # new_code = append(new_code, code[[i]])
    new_code[[j]] = code[[i]]
    j = j + 1
  }

  res = list(code = as.expression(new_code), vars_ok = vars_ok)

  res
}

extract_source_functions_from_path = function(all_paths, fun_list = list(), 
                                              names_hash = FALSE){
  
  for(path in all_paths){
    fun_list[[path]] = funs_from_source(path, names_hash = names_hash)
  }
  
  fun_list
}

extract_source_functions = function(code_text, fun_list = list(), env,
                                    pkgs_in = NULL, names_hash = FALSE){

  check_arg(code_text, "character vector no na")
  
  # recursivity counter + check
  n_rec = attr(code_text, "n_rec")
  if(is.null(n_rec)){
    n_rec = 0
  } else {
    n_rec = n_rec + 1
  }
  
  if(n_rec == 5){
    stop("In the current set of source files we have source() calling other source()",
         " recursively for at least 5 times. This is not allowed in rmake, please revise the code.")
  }

  # we remove comments
  code_text = code_text[!grepl("^\\s*#", code_text)]

  pkg_all = NULL
  code_text_scalar = paste(code_text, collapse = "\n")
  all_sources = cpp_extract_fun(code_text_scalar, "source")
  sources_cpp = cpp_extract_fun(code_text_scalar, "sourceCpp")
  all_sources = c(all_sources, sources_cpp)

  if(length(all_sources) > 0){
    source_fun = list(
      source = function(file, ...) file,
      sourceCpp = function(file, ...) file
    )

    if(is.null(pkg_all)){
      pkg_all = find_packages(code_text)
      pkg_all = sort(unique(c(pkgs_in, pkg_all)))
    }

    for(i in seq_along(all_sources)){
      src_txt = add_namespace(all_sources[[i]], env, pkgs_in)
      src_code = parse(text = src_txt, keep.source = FALSE)

      source_path = eval(src_code, source_fun, env)
      check_value(source_path, "character scalar",
                  .message = "The path of a source must be a character scalar!")

      if(grepl("\\.(r|R)$", source_path)){
        # We allow recursivity
        source_text = readLines(source_path)
        attr(source_text, "n_rec") = n_rec
        fun_list = extract_source_functions(source_text, fun_list, env, pkg_all)
      }

      fun_list[[source_path]] = funs_from_source(source_path, names_hash = names_hash)
    }
  }

  fun_list
}

add_namespace = function(code_text, env, pkg_all){

  DO_SPLIT = length(code_text) > 1
  if(DO_SPLIT){
    code_text = paste(code_text, collapse = "\n")
  }

  code = parse(text = code_text, keep.source = FALSE)
  all_names = all.names(code)
  name_exists = sapply(all_names, exists, where = env)
  if(!all(name_exists)){


    pkg_attached = gsub("package:", "", grep("package:", search(), fixed = TRUE, value = TRUE), fixed = TRUE)
    pkg_all = setdiff(pkg_all, pkg_attached)

    # we check if the functions belong to not yet loaded packages
    name_pblm = all_names[!name_exists]
    for(fun_name in name_pblm){

      # What if the package is not installed yet?
      # example: first run with here not installed and all paths
      # are written with here....
      # How to do it => I cannot find the paths... then I cannot find the dependencies...
      #

      if(grepl(paste0("(?<!::)", fun_name, "\\("), code_text, perl = TRUE)){
        for(pkg in pkg_all){
          if(requireNamespace(pkg, quietly = TRUE)){
            ns = loadNamespace(pkg)
            funs_exported = getNamespaceExports(ns)
            if(fun_name %in% funs_exported){
              code_text = gsub(paste0("(?<!::)", fun_name),
                               paste0(pkg, "::", fun_name), code_text, perl = TRUE)
              break
            }
          } else {
            stop("The package ", pkg, " is used but not installed. Install it?",
                 "\n install.packages(\"", pkg, "\")")
            install.packages(pkg)
          }
        }
      }
    }
  }

  if(DO_SPLIT){
    code_text = strsplit(code_text, split = "\n")[[1]]
  }

  code_text
}


funs_from_source = function(path, names_hash = FALSE){
  # we get both functions and global variables

  if(grepl("\\.(r|R)$", path)){
    code = parse(path, keep.source = FALSE)
    res = unlist(lapply(code, get_fun_name))

    if(names_hash){
      names(res) = unlist(lapply(code, get_fun_hash))
    }

  } else if(grepl("\\.cpp$", path)){

    cpp = readLines(path)
    i = which(grepl("// [[Rcpp::export]]", cpp, fixed = TRUE))
    if(length(i) == 0){
      warni("No exported function found in {Q?path}. Please add at least one function.")
    }

    fun_names_raw = cpp[i + 1]
    fun_names = gsub("(\\(.+)|(^[^ ]+ +)", "", fun_names_raw)
    res = fun_names

    if(names_hash){
      names(res) = rep(hash_cpp_funs(path), length(res))
    }

  } else {
    stopi("Internal error: could not extract functions from that source:\n ", 
          "{Q?path}\nPlease revise code to enable it.")
  }

  res
}

source_dependencies = function(path){

  code_text = readLines(path)

  if(grepl("\\.cpp$", path)){
    hash = hash_cpp_funs(path)

    res = paste0("cpp@", path, "=", hash)
    names(res) = res

    return(res)
  }


  fun_list = extract_source_functions(code_text, fun_list = list(),
                                      env = .GlobalEnv, pkgs_in = NULL,
                                      names_hash = TRUE)

  all_funs = funs_from_source(path, names_hash = TRUE)
  fun_list[[path]] = all_funs
  fun_vec = unlist(fun_list)

  funs_parsed = parse(path, keep.source = FALSE)

  n = length(funs_parsed)
  res = c()
  id = 1

  for(i in seq_along(funs_parsed)){
    fun = funs_parsed[[i]]

    if(length(fun) != 3){
      next
    }

    op = as.character(fun[[1]])[[1]]
    if(!op %in% c("=", "<-")){
      next
    }

    fun_name = deparse_long(fun[[2]])
    fun_hash = cpp_hash_string(deparse_long(fun[[3]]))

    fun_fmt = paste0("source@", path, "/", fun_name, "=", fun_hash)

    dep = intersect(fun_vec, all.names(fun[[3]]))
    # we avoid recursive links
    dep = setdiff(dep, fun_name)

    tmp = character(0)
    if(length(dep) > 0){
      tmp = c()
      for(f in dep){
        for(p in names(fun_list)){
          # p = path
          FL = fun_list[[p]]
          if(f %in% fun_list[[p]]){
            tmp = c(tmp, names(FL)[which(FL == f)])
            break
          }
        }
      }

      if(length(tmp) > 0){
        tmp = sort(unique(tmp))
        tmp = gsub(".+=", "", tmp)
        fun_fmt = paste0(fun_fmt, ":", paste0(tmp, collapse = ";"))
      }
    }

    res[id] = fun_fmt
    id = id + 1
  }

  res
}


####
#### small utilities ####
####


backup_file = function(f){
  # f is the raw file name
  f_raw = f

  # we "normalize" the name
  f = gsub("^\\./|\\.(r|R)", "", f)
  f = gsub("/", "_-_", f)

  # we get the backup number
  if(!dir.exists(".rmake/backup")) dir.create(".rmake/backup")

  all_backup_files = list.files(".rmake/backup", full.names = TRUE)
  bak_files = grep(f, all_backup_files, fixed = TRUE, value = TRUE)
  n_bak = length(bak_files)

  if(n_bak > 0){
    if(file.size(f_raw) == file.size(tail(bak_files, 1))){
      # Nothing to be done
      return()
    }
  }

  # we save the file
  new_file = paste0(".rmake/backup/", f, "_bak-", sprintf("%02i", n_bak + 1), ".R")
  file.copy(f_raw, new_file)
}

rmake_output_stamp = function(){
  paste0("#> rmake: ", Sys.time())
}

get_vars_dsb = function(call){
  res = all.vars(call)
  call_dp = deparse_long(call)
  if(grepl('"', call_dp, fixed = TRUE)){
    string = gsub('^[^"]+|[^"]+$', "", call_dp)
    if(grepl(".[", string, fixed = TRUE)){
      # very dirty, only works for simple cases like "bon.[jour]", does not accept complex operations
      # easy to expand though but I'm time constrained
      string_split = strsplit(string, ".[", fixed = TRUE)[[1]][-1]
      string_split = gsub("\\].+", "", string_split)
      res = c(res, string_split)
    }
  }

  res
}


run_test = function(chunk, from){

  test_code = readLines("tests/fixest_tests.R")

  i = grepl("library(fixest)", test_code, fixed = TRUE)
  test_code[i] = "devtools::load_all(export_all = FALSE)"

  # A) adding the line numbers

  lines = paste0("LINE_COUNTER = ", seq_along(test_code))

  code_LINE = rep(lines, each = 2)
  code_LINE[seq_along(test_code)  * 2] = test_code

  # We remove the counters for the lines with open parenthesis, like in
  # sum(x,
  #     y)
  # since this leads to parsing errors

  i_open = which(grepl(",[[:blank:]]*$", code_LINE))

  if(length(i_open)) i_open = i_open + 1

  # We remove the counters just before closing brackets => otherwise equivalent to a return statement!
  i_closing_bracket = which(grepl("^[[:blank:]]*\\}", code_LINE))

  if(length(i_closing_bracket)) i_closing_bracket = i_closing_bracket - 1

  # removing
  i_rm = c(i_open, i_closing_bracket)
  if(length(i_rm) > 0){
    code_LINE = code_LINE[-(i_rm)]
  }

  # B) Adding the FOR loops

  bracket_close = which(grepl("^\\}", code_LINE))
  for_start = which(grepl("^for\\(", code_LINE))
  for_end = c()
  for(i in for_start){
    for_end = c(for_end, bracket_close[which.max(bracket_close > i)])
  }

  n = length(code_LINE)
  my_rep = rep(1, n)
  my_rep[c(for_start, for_end)] = 2

  code_LINE_FOR = rep(code_LINE, my_rep)
  n_loop = length(for_start)
  code_LINE_FOR[for_start + 2 * (0:(n_loop - 1))] = "INSIDE_LOOP = TRUE ; INDEX_LOOP = list()"
  code_LINE_FOR[for_end + 2 * 1:n_loop] = "INSIDE_LOOP = FALSE"

  qui_for = which(grepl("\\bfor\\(", code_LINE_FOR))
  index = gsub("^.*for\\(([^ ]+).+", "\\1", code_LINE_FOR[qui_for])

  n = length(code_LINE_FOR)
  my_rep = rep(1, n)
  my_rep[qui_for] = 2

  code_LINE_FOR = rep(code_LINE_FOR, my_rep)
  n_loop = length(qui_for)
  code_LINE_FOR[qui_for + 1 + (0:(n_loop - 1))] = paste0("INDEX_LOOP[[\"", index, "\"]] = ", index)

  test_code = code_LINE_FOR

  # C) Chunk selection

  if(!missing(chunk) || !missing(from)){

    qui = which(grepl("^chunk\\(", test_code))
    all_chunks = test_code[qui]
    chunk_names = tolower(gsub(".+\\(\"|\".*", "", all_chunks))
    n_chunks = length(qui)

    if(!missing(from)){
      check_value_plus(from, "match | integer scalar no na", .choices = chunk_names)

      if(is.numeric(from)){
        if(any(from > n_chunks)){
          stop("There are maximum ", n_chunks, " chunks.")
        }
        chunk_select = from:n_chunks
      } else {
        chunk_select = which(chunk_names %in% from):n_chunks
      }

    } else {
      check_value_plus(chunk, "multi match | integer vector no na", .choices = chunk_names)

      if(is.numeric(chunk)){
        if(any(chunk > n_chunks)){
          stop("There are maximum ", n_chunks, " chunks.")
        }
        chunk_select = sort(unique(chunk))
      } else {
        chunk_select = which(chunk_names %in% chunk)
      }
    }

    qui = c(qui, length(test_code))

    new_test_code = c()
    for(i in chunk_select){
      new_test_code = c(new_test_code, test_code[qui[i] : (qui[i + 1] - 1)])
    }

    # We add the preamble
    preamble = test_code[1:(qui[1] - 1)]

    test_code = c(preamble, new_test_code)
  }

  # D) Evaluation

  parsed_code = parse(text = test_code)

  env = new.env()
  assign("INSIDE_LOOP", FALSE, env)

  my_eval = try(eval(parsed_code, env))

  # E) Message

  if("try-error" %in% class(my_eval)){
    line_fail = get("LINE_COUNTER", env)
    inside_loop = get("INSIDE_LOOP", env)

    message("Test failed at line: ", line_fail)
    if(inside_loop){
      index_loop = get("INDEX_LOOP", env)
      index_names = names(index_loop)
      index_values = unlist(index_loop)
      msg = paste0(index_names, ":", index_values, collapse = ", ")
      message("Loop values: ", msg, ".")
    }

    # We assign the variables to the global env to facilitate debugging
    for(var in names(env)){
      assign(var, get(var, env), parent.frame())
    }

  } else {
    print("tests performed successfully")
  }
}


hash_cpp_funs = function(path){
  text = paste0(readLines(path), collapse = "\n")
  text_clean = cpp_clean_comments(text)
  cpp_hash_string(text_clean)
}

"%inlist%" = function(l, values){
  # l: a list of vectors
  # value: a vector
  # returns an integer vector

  n = length(l)
  len_all = lengths(l)
  id = rep(1:n, len_all)

  l_flat = unlist(l)

  unique(id[l_flat %in% values])
}

absolute_to_relative = function(path_all, origin = getOption("rmake_root_path")){
  # path = normalizePath("../../C_testing/ctesting/")
  # NOTA: fs:::is_absolute_path does not consider ./image/etc or ../images/etc as relative...
  # ONLY /images/etc is OK or images/etc

  wd = normalizePath(origin, "/")
  path_all = normalizePath(path_all, "/", mustWork = FALSE)

  n_path = length(path_all)
  res_all = character(n_path)

  for(i in 1:n_path){

    path = path_all[i]

    if(wd == path){
      res_all[i] = "."
      next
    }

    wd.s = strsplit(wd, "/", fixed = TRUE)[[1]]
    path.s = strsplit(path, "/", fixed = TRUE)[[1]]

    n = min(length(wd.s), length(path.s))

    if(wd.s[1] != path.s[1]){
      # different drive, must be absolute
      res_all[i] = path
    }

    index = which.max(wd.s[1:n] != path.s[1:n])

    if(index == 1){
      # all the same => inclusion
      index = n + 1
    }

    rest_wd = wd.s[-(1:(index-1))]

    res = ""
    nr = length(rest_wd)
    if(nr > 0){
      res = sma("{`nr`times, '/'c ! ..}")
    }

    rest_path = path.s[-(1:(index-1))]

    nr = length(rest_path)
    if(nr > 0){
      res = sma("{if(.nchar > 0; '/'append.right) ? res}{'/'c ? rest_path}")
    }

    res_all[i] = res
  }

  res_all
}

is_write_fun = function(line){
  
  all_funs = setdiff(all.vars(line, functions = TRUE), all.vars(line))
  
  if(any(names(write_funs) %in% all_funs)){
    return(TRUE)
  }
  
  # we consider a function to be an input if:
  # - **no assignment**
  # - it has an argument path or file
  
  if(length(line) == 3 && is_operator(line, c("=", "<-"))){
    return(FALSE)
  }
  
  if(has_path_argument(line)){
    return(TRUE)
  }
  
  return(FALSE)
}

is_read_fun = function(line){
  
  # browser()
  
  all_funs = setdiff(all.vars(line, functions = TRUE), all.vars(line))
  
  if(any(names(read_funs) %in% all_funs)){
    return(TRUE)
  }
  
  # we consider a function to be an input if:
  # - it assigns to a variable
  # - it has an argument path or file
  
  if(length(line) == 3 && is_operator(line, c("=", "<-"))){
    rhs = line[[3]]
    if(has_path_argument(rhs)){
      return(TRUE)
    }
  }
  
  return(FALSE)
}


remove_magrittr_pipe = function(expr){
  # removes %>% pipes
  
  if(length(expr) == 3 && is_operator(expr, "%>%")){
    arg = expr[[2]]
    fun = expr[[3]]
    
    if(length(arg) == 3 && is_operator(arg, "%>%")){
      arg = remove_magrittr_pipe(arg)
    }
    
    if(is.call(fun)){
      if(length(fun) == 1){
        fun[[2]] = arg
        expr = fun
        
      } else {
        new_fun = fun
        new_fun[[2]] = arg
        for(i in 2:length(fun)){
          new_fun[[i + 1]] = fun[[i]]
        }
        
        expr = new_fun
      }
    }
    
  }
  
  expr
}

has_path_argument = function(expr){
  
  # limitations:
  # - does not handle:
  #   x = base::read.csv(path)
  #   because of `base::`
  # - does not handle piping:
  #   x = path |> read.csv()
  
  # add recursivity????
  
  if(length(expr) >= 2){
    
    # piping
    expr = remove_magrittr_pipe(expr)
    
    # case 1: path given explicitly
    
    if(any(c("path", "file") %in% names(expr))){
      return(TRUE)
    }
    
    # case 2: positional
    
    fun_text = as.character(expr[[1]])
    if(length(fun_text) == 1){
      if(exists(fun_text, mode = "function")){
        
        args_raw = args(fun_text)
        
        if(is.null(args_raw)){
          return(FALSE)
        }
        
        all_args = names(formals(args_raw))
        
        if("..." %in% all_args){
          # we remove everything after ... since they need to be called explicitly
          i = which(all_args == "...")
          if(i > 1){
            all_args = all_args[1:(i - 1)]
          } else {
            all_args = NULL
          }
        }
        
        return(any(c("path", "file") %in% all_args))
      }
    }
  }
  
  return(FALSE)
}

get_args = function(...){
  mc = match.call()
  sapply(mc[-1], deparse_long)
}

extract_rw_path = function(line, read = FALSE, write = FALSE,
                           env = new.env(parent = .GlobalEnv)){
  # line is a call

  if(!read && !write){
    stop("You mut provide either the argument 'read' or the argument 'write'.")
  }

  all_funs = setdiff(all.vars(line, functions = TRUE), all.vars(line))

  rw_funs = if(read) read_funs else write_funs

  fun_name = intersect(names(rw_funs), all_funs)
  
  if(length(fun_name) > 1){
    stopi("Internal error: Several {&read ; reading ; writing} functions in a same line were found => revise code. WIP improve this message")
  }

  if(length(fun_name) == 0){
    # stop("Internal error: No ", if(read) "reading" else "writing", " function was found => revise code.")
    
    # this is not a builtin function, we extract it
    if(read){
      # we remove the assignment
      fun_call = line[[3]]
    } else {
      fun_call = line
    }
    
    fun_call = remove_magrittr_pipe(fun_call)
    
    # we create the mock function
    fun_name = as.character(fun_call[[1]])
    args_used = names(fun_call)
    found = FALSE
    if(!is.null(args_used)){
      i = which(args_used %in% c("path", "file"))
      if(length(i) > 0){
        found = TRUE
        fun_call = fun_call[c(1, i[1])]
      }
    }
    
    if(!found){
      all_args = names(formals(args(fun_name)))
      i_drop = which(args_used %in% all_args)
      if(length(i_drop) > 0){
        fun_call = fun_call[-i_drop]
      }
      
      all_args = setdiff(all_args, args_used)
      i = which(all_args %in% c("path", "file"))
      
      check_value(i, "integer scalar", .message = "internal error: multiple path/file arguments. check code")
      
      if(i + 1 > length(fun_call)){
        stopi("internal error: argument path/file beyond all possible indexes")
      }
      
      fun_call = fun_call[c(1, i + 1)]
    }
    
    # we end up with a call to a function with a single unnamed argument
    names(fun_call) = NULL
    rw_funs[[fun_name]] = function(x) x
    rw_fun_calls[[fun_name]] = function(x) match.call()$x
    
  } else {
    fun = cpp_extract_fun(deparse_long(line), fun_name)

    if(grepl("^__", fun)){
      stop("Internal error: the {&read ; reading ; writing}",
          " function could not be extracted (", fun, ") => revise code.")
    }

    fun_call = str2lang(fun)
  }

  path = try(eval(fun_call, rw_funs, env), silent = TRUE)
  path_call = eval(fun_call, rw_fun_calls)
  if("try-error" %in% class(path)){
    path = list(fun_call = fun_call, path_call = path_call,
                path = NA_character_, must_eval = TRUE)
  } else {
    path = list(fun_call = fun_call, path_call = path_call,
                path = path, must_eval = FALSE)
  }

  path
}

get_fun_name = function(x){

  if(length(x) == 3 && as.character(x[[1]]) %in% c("=", "<-")){
    res = as.character(x[[2]])
  } else {
    res = NULL
  }

  res
}

get_fun_hash = function(x){

  if(length(x) == 3 && as.character(x[[1]]) %in% c("=", "<-")){
    res = as.character(x[[2]])
    res = paste0(res, "=", cpp_hash_string(deparse_long(x[[3]])))
  } else {
    res = NULL
  }

  res
}


parse_io = function(expr, is_assignment = FALSE, is_root = TRUE){
  # takes an expression and returns an object with the IO paths
  
  res = list(is_io = FALSE, path_input = list(), path_output = list())
  
  if(length(expr) <= 1){
    return(res)
  }
  
  if(length(expr) == 3 && is_operator(expr, c("=", "<-"))){
    is_assignment = TRUE
    expr = expr[[3]]
  }
  
  expr = remove_magrittr_pipe(expr)
  
  
  
  
  return(res)
}


find_packages = function(text){
  #
  # How do we do:
  # we mask the library/require/p_load functions
  # and evaluate with the new functions

  # 1) we fetch the valid lines of code
  code_line_info = cpp_valid_code_index(text)

  index_start_lines = code_line_info$line_nb
  start_lines = text[index_start_lines]

  # 2) we get the functions used to load packages:
  # ONLY: require, library and p_load. Easy to add more if needed

  who = grepl("(?<![[:alnum:]_.])(require|library|p_load)\\(", start_lines, perl = TRUE)

  res = c()
  if(any(who)){
    who = which(who)
    index_end_lines = code_line_info$line_nb + code_line_info$size - 1

    # NOTA:
    # - later add include/exclude that I can add as attributes later used in the code
    # - so far feeding in character vectors does not work => I can implement that later but
    #   it requires code crawling

    lib_extract = function(package, character.only = FALSE, ...){
      if(character.only){
        return(package)
      }
      mc = match.call()
      res = mc$package
      if(!is.character(res)) res = deparse_long(res)
      return(res)
    }

    p_extract = function(..., char, install, update, character.only = FALSE){
      if(!missing(char)) return(char)

      if(isTRUE(character.only)){
        return(..1)
      }

      mc_dots = match.call(expand.dots = FALSE)[["..."]]

      res = sapply(mc_dots, function(x) if(is.character(x)) x else deparse_long(x))
      res
    }

    pkg_extract_funs = list(library = lib_extract,
                            require = lib_extract,
                            p_load = p_extract)

    for(i in who){

      code_txt = text[index_start_lines[i]:index_end_lines[i]]
      code_txt = gsub("pacman::", "", code_txt, fixed = TRUE)

      code = parse(text = code_txt, keep.source = FALSE)

      pkg = eval(code, pkg_extract_funs, .GlobalEnv)
      res = c(res, pkg)
    }
  }

  res
}


path_timestamp = function(x){
  # x: list of (input or output) paths from a chunk

  if(length(x) == 0){
    return(NA)
  }

  # Note that a "path" element from x is guaranteed to exist,
  # we check it beforehand

  paths = sapply(x, function(y) y$path)
  mtimes = sapply(paths, file.mtime)

  res = paste0(paths, ": ", mtimes)
  res
}

path_sizestamp = function(x){
  # x: list of (input or output) paths from a chunk

  if(length(x) == 0){
    return(NA)
  }

  # Note that a "path" element from x is guaranteed to exist,
  # we check it beforehand

  paths = sapply(x, function(y) y$path)
  sizes = sapply(paths, file.size)

  res = paste0(paths, ": ", sizes)
  res
}

get_session = function(){
  # Simple function to identify the machine running the code
  si = Sys.info()
  si_machine = si[c("sysname", "nodename", "machine")]
  si_user = unique(si[c("login", "user", "effective_user")])
  paste(c(si_machine, si_user), collapse = "--")
}

is_same_output_files = function(x, y){
  is_same_input_files(x, y, output = TRUE)
}

is_same_input_files = function(x, y, output = FALSE){

  if(x$session == y$session){
    # We use the time stamps
    if(output){
      res = identical(x$output_timestamps, y$output_timestamps)
    } else {
      res = identical(x$input_timestamps, y$input_timestamps)
    }
  } else {
    # we use sizes (this is more dangerous though)
    # although the risk is super super super tiny

    if(output){
      res = identical(x$output_sizestamps, y$output_sizestamps)
    } else {
      res = identical(x$input_sizestamps, y$input_sizestamps)
    }
  }

  res
}

gen_tag = function(){
  # simple tag
  paste(sample(c(0:9, letters), 5, replace = TRUE), collapse = "")
}

difftime_fmt = function(x){
  # x: number of seconds, difftime, time

  if(inherits(x, "POSIXt")){
    x = Sys.time() - x
  }

  if(inherits(x, "difftime")){
    x = as.double(x, units = "secs")
  }

  if(x > 3600){
    n_h = x %/% 3600
    rest_s = floor(x %% 3600)
    n_min = rest_s %/% 60
    res = paste0(n_h, "h ", sprintf("%02i", n_min), "m")
  } else if(x > 60){
    n_min = x %/% 60
    rest_s = floor(x %% 60)
    res = paste0(n_min, "m ", sprintf("%02i", rest_s), "s")
  } else if(x < 0.1){
    res = "< 0.1s"
  }  else {
    res = paste0(fsignif(x, 2, 1), "s")
  }

  res
}

rm_file = function(x){
  if(file.exists(x)){
    file.remove(x)
  }
}

get_op = function(expr){
  as.character(expr[[1]])[1]
}





























