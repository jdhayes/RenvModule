module <-
function(action_type,module_name=""){
  # Check to see if modulecmd is in current PATH
  try(
    suppressWarnings(modulecmd_path <- system("which modulecmd",intern=TRUE,ignore.stderr=TRUE)),
    silent=TRUE
  )
  
  # Only initialize module system if it has not yet been initialized and the modulecmd exisits
  if ( Sys.getenv('MODULEPATH') == "" && length(modulecmd_path) > 0) {
    myEnvModules$init()
  } else if (Sys.getenv('MODULEPATH') == "" && length(modulecmd_path) == 0) {
    stop("Cound not find the installation of Environment Modules: \"modulecmd\"")
  }
  
  switch(action_type,
    "load"   = myEnvModules$load_unload(action_type,module_name),
    "unload" = myEnvModules$load_unload(action_type,module_name),
    "list"   = myEnvModules$avail_list(action_type),
    "avail"  = myEnvModules$avail_list(action_type),
    "clear"  = myEnvModules$clear(action_type),
    "init"   = myEnvModules$init(),
    stop("That action is not supported.")
  )
}
