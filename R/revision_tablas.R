#' Tablas de
#'
#' @param data Base de datos
#' @param regex Regex que busca en nombresR
#'
#' @return
#' @export
#'
#' @examples

revision_tablas = function(data, regex){
  for (i in nombresR(data, regex)) {
    print(i)
    cat("\n")
    print(table(data[i]))
    print("*********************************************************************************")
  }
}
