moduleload <-
function(module,envir="PATH") {
  module("load",module)
  warning("The function moduleload will be deprecated in future releases, please refer to the documentation for proper useage.")
}
