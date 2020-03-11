#' @title Construct a fuzzy number using a stepwise function
#' Faltará hacer comprobaciones para que lo que se ha definido cumpla con las propiedades para ser un número fuzzy.
#' @description This function is used to specify a fuzzy number using a stepwise function.
#'
#' @usage piecewise_fzz(extremos,
#'                  funciones,
#'                  valor)
#'
#' @param extremos  A (ordered) list containing the extremes at which the different step functions are defined.
#' @param funciones A list containig the names(strings) of the step functions, it is possible to use user functions defined in the usual form.
#' @param valor     A number that the fuzzy stepwise-number is evaluated.
#' @return          A number, the value of the memebership function (between 0 and 1).
#'
#' @examples
#' my_function<-function(x){x^2}
#' extremos<-c(12,15,18,19)
#' funciones<-c("exp","log","my_function")
#' valor<-17
#' piecewise_fzz(extremos,funciones,valor)
#'
#' @export
#'

piecewise_fzz<-function(extremos,funciones,valor)
            {
              if((min(extremos>=valor))|(max(extremos)<=valor)){0}
                 else{ indices<-which(valor<=extremos) #comprobamos aquellos 'puntos' de cambo que son mayores que el punto a estimar.
                      if((min(indices)==1)){0}          #El menor de los ?ndices es el que guarda la informaci?n sobre qu? funci?n ejecutar.
                        else{
                              indice<-min(indices)-1
                              eval(call(funciones[indice],valor))
                            }
                      }
             }


