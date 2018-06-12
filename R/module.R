#################################
## Access module system from R ##
#################################
# S3 Class for handling function calls
myEnvModules <- structure(list(), class="EnvModules")

## Main function to allow avail, list and list
myEnvModules$init <- function(){
  # Module function assumes MODULEPATH and MODULEDIR are set in login profile
  # Get base environment from login profile
  base_env <- strsplit(system('bash -l -c "env"',intern = TRUE),'\n')
  base_env <- strsplit(as.character(base_env),'=')
  
  # Iterate through base environment
  for (x in seq(1,length(base_env))) {
    
    # Set environment based on login profile
    if (base_env[[x]][1]=="LOADEDMODULES" || base_env[[x]][1]=="MODULESHOME" || base_env[[x]][1]=="MODULEPATH" || base_env[[x]][1]=="MODULES_DIR" || base_env[[x]][1]=="IIGB_MODULES"){
      if (base_env[[x]][1]=="LOADEDMODULES"){
        default_modules <- strsplit(base_env[[x]][2],":")
      }
      else{
        l <- list(base_env[[x]][2])
        names(l) <- base_env[[x]][1]
        do.call(Sys.setenv, l)
      }
    }
  }
  
  # Make sure to process default modules after the environment is set with the above loop
  for (x in seq(1,length(default_modules[[1]]))){
    module_name <- default_modules[[1]][x]
    print(paste("Loading module",module_name))
    try(myEnvModules$load_unload("load",module_name))
  }
}

# Print available modules or currently loaded modules on stderr
myEnvModules$avail_list <- function(action_type){
  try(module_vars <- system(paste('modulecmd bash',action_type),intern = TRUE))
}

# Unload all currently loaded modules
myEnvModules$clear <- function(action_type){
  loaded_modules <-  strsplit(Sys.getenv("LOADEDMODULES"),":")
  if (length(loaded_modules[[1]]) > 0) {
    for (x in seq(1,length(loaded_modules[[1]]))){
      module_name <- loaded_modules[[1]][x]
      print(paste("Unloading module",module_name))
      try(myEnvModules$load_unload("unload",module_name))
    }
  }
}

# Load and unload actions are basically the same, set environment variables given by modulecmd
myEnvModules$load_unload <- function(action_type, module_name=""){
  module_name <- paste(module_name, collapse=' ')
  
  # Use the low level C binary for generating module environment variables
  try(module_vars <- system(paste('modulecmd bash',action_type, module_name),intern = TRUE))
  
  if (length(module_vars) > 0){
    for (y in seq(1,length(module_vars))) {
      # Separate environment variables
      module_var <- strsplit(module_vars,";")
    
      # Iterate through all environment variables
      for (x in seq(1,length(module_var[[y]]))) {
        # Isolate key, value pair
        evar <- module_var[[y]][x]
        
        # Filter export commands
        if (length(grep('^ *export',evar)) == 0 && length(evar) > 0) {
          # Seprate key and value
          evar <- strsplit(as.character(evar),'=')
          # Stip spaces at the end of the value
          evar_val <- gsub('[[:space:]]','',evar[[1]][2])
          # Remove extra backslashes
          l <- list(gsub('\\$','',evar_val))
          
          # Load dependant modules
          if (length(grep('^ *module',evar[[1]][1])) > 0){
            inner_module <- strsplit(evar[[1]][1]," ")
            #myEnvModules$load_unload(inner_module[1][[1]][2],inner_module[1][[1]][3])
          }
          # Source environment
          else if (length(grep('^ *source',evar[[1]][1])) > 0){
            warning(paste0("Module uses a bash script to initialize, some software may not function as expected:\n\t",evar[[1]][1]))
          }
          # Unset variables that need to be unset
          else if(length(grep("^ *unset ",evar[[1]][1])) > 0){
            evar <- gsub("^unset (.*)$","\\1",evar[[1]][1])
            Sys.unsetenv(evar)
          } 
          else {
            # Assign names to each value in list
            names(l) <- evar[[1]][1]
            # Set environment variable in current environment
            do.call(Sys.setenv, l)
          }
        }
      }
    }
  }
}

#Define what happens bases on action
module <- function(action_type,module_name=""){
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
## Usage: 
# module("load","tophat")
# module("load","tophat/2.1.1")
# module("list")
# module("avail")
# module("init")
# module("unload", "tophat")
# module("unload", "tophat/2.1.1")

#####################
## Legacy Wrappers ##
#####################
## List software available in module system
modulelist <- function() {
  module("avail")
  warning("The function modulelist will be deprecated in future releases, please refer to the documentation for proper useage.")
}

## Load software from module system
moduleload <- function(module,envir="PATH") {
  module("load",module)
  warning("The function moduleload will be deprecated in future releases, please refer to the documentation for proper useage.")
}

