#################################
## Access module system from R ##
#################################

#' A Reference Class to contain method.
#' @import methods
#' @export RenvModule
#' @exportClass RenvModule
RenvModule <- setRefClass("RenvModule",
  fields=list(modulecmd_path="character"),
  methods=list(
    ## Main function to allow avail, list and list
    init=function(){
      # Module function assumes MODULEPATH and MODULEDIR are set in login profile
      # Get base environment from login profile
      base_env <- strsplit(system('bash -l -c "env"',intern = TRUE),'\n')
      base_env <- strsplit(as.character(base_env),'=')
      
      # Iterate through base environment
      for (x in seq(1,length(base_env))) {
        
        # Set environment based on login profile
        if (base_env[[x]][1]=="LOADEDMODULES" || base_env[[x]][1]=="MODULESHOME" || base_env[[x]][1]=="MODULEPATH" || base_env[[x]][1]=="MODULES_DIR" || base_env[[x]][1]=="HPCC_MODULES"){
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
        try(load_unload("load",module_name))
      }
    },

    # Return available modules or currently loaded modules
    avail_list=function(action_type){
      try(module_vars <- system2(modulecmd_path,paste('bash',action_type,'-t'),stdout=TRUE,stderr=TRUE))
      # Return only the module names
      return(module_vars[-grep(":$",module_vars)])
    },

    # Unload all currently loaded modules
    clear=function(action_type){
      loaded_modules <-  strsplit(Sys.getenv("LOADEDMODULES"),":")
      if (length(loaded_modules[[1]]) > 0) {
        for (x in seq(1,length(loaded_modules[[1]]))){
          module_name <- loaded_modules[[1]][x]
          print(paste("Unloading module",module_name))
          try(load_unload("unload",module_name))
        }
      }
    },

    # Load and unload actions are basically the same, set environment variables given by module command
    load_unload=function(action_type, module_name=""){
      module_name <- paste(module_name, collapse=' ')
      
      # Use the low level C binary for generating module environment variables
      try(module_vars <- system(paste(modulecmd_path,'bash',action_type, module_name),intern = TRUE))
      
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
              
              # Stip spaces at the end of the value, and remove backslashes
              evar_val <- gsub("\\\\", "", gsub('[[:space:]]','',evar[[1]][2]))
              
              # Remove extra backslashes
              l <- list(gsub('\\$','',evar_val))
              
              # Load dependant modules
              if (length(grep('^ *module',evar[[1]][1])) > 0){
                inner_module <- strsplit(evar[[1]][1]," ")
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
  )
)

#' Global instance of RenvModule
# @name myEnvModules
myEnvModules <- RenvModule()

#' Define what happens based on action
#'
#' @param action_type Name of the action to be executed as character vector. The following switches are accepted: \dQuote{avail}, \dQuote{list}, \dQuote{init}, \dQuote{load}, \dQuote{unload}, and \dQuote{clear}.
#' @param module_name Name of software to load as character vector.
#' @export module
#' @examples
#'\dontrun{
#' module("load","tophat")
#' module("load","tophat/2.1.1")
#' module("list")
#' module("avail")
#' module("init")
#' module("unload", "tophat")
#' module("unload", "tophat/2.1.1")
#' module("clear")
#'}
module <- function(action_type,module_name=""){

  # Find path for module command
  myEnvModules$modulecmd_path <- Sys.getenv('LMOD_CMD')
  if (length(myEnvModules$modulecmd_path) > 0) {
    try(
      suppressWarnings(myEnvModules$modulecmd_path <- system("which modulecmd",intern=TRUE,ignore.stderr=TRUE)),
      silent=TRUE
    )
  }
 
  # Only initialize module system if it has not yet been initialized and the module command exists
  if ( Sys.getenv('MODULEPATH') == "" && length(myEnvModules$modulecmd_path) > 0) {
    myEnvModules$init()
  } else if (Sys.getenv('MODULEPATH') == "" && length(myEnvModules$modulecmd_path) == 0) {
    stop("Cound not find the path of Environment Modules command \"modulecmd\" nor LMOD_CMD")
  }
  
  switch(action_type,
    "load"   = myEnvModules$load_unload(action_type,module_name),
    "unload" = myEnvModules$load_unload(action_type,module_name),
    "list"   = return(myEnvModules$avail_list(action_type)),
    "avail"  = return(myEnvModules$avail_list(action_type)),
    "clear"  = myEnvModules$clear(action_type),
    "init"   = myEnvModules$init(),
    stop("That action is not supported.")
  )
}

