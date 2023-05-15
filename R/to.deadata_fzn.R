#' @title Transforms to deadata_fzn
#'
#' @description Transforms a \code{deadata} or \code{deadata_fuzzy} object
#' into a \code{deadata_fzn} object.
#'
#' @usage to.deadata_fzn(x)
#'
#' @param x Any \code{deadata} or \code{deadata_fuzzy} object.
#'
#' @return Returns a \code{deadata_fzn} object.
#'
#' @export


to.deadata_fzn <- function(x) {

  if (inherits(x, "deadata")) {

    res <- x
    res$input <- apply(x$input, MARGIN = 1, FUN = fuzzyfy)
    res$output <- apply(x$output, MARGIN = 1, FUN = fuzzyfy)
    res <- structure(res, class = "deadata_fzn")

  } else if (inherits(x, "deadata_fuzzy")) {

    nd <- ncol(x$input$mL)

    ni <- nrow(x$input$mL)
    input <- vector(mode = "list", length = ni)
    for (i in 1:ni) {
      input[[i]] <- vector(mode = "list", length = nd)
      for (j in 1:nd) {
        input[[i]][[j]] <- fzn(alpha = c(0, 1),
                               l = c(x$input$mL[i, j] - x$input$dL[i, j], x$input$mL[i, j]),
                               u = c(x$input$mR[i, j] + x$input$dR[i, j], x$input$mR[i, j]))
      }
      input[[i]] <- fzarray(input[[i]])
      names(input[[i]]) <- colnames(x$input$mL)
    }
    names(input) <- rownames(x$input$mL)

    no <- nrow(x$output$mL)
    output <- vector(mode = "list", length = no)
    for (i in 1:no) {
      output[[i]] <- vector(mode = "list", length = nd)
      for (j in 1:nd) {
        output[[i]][[j]] <- fzn(alpha = c(0, 1),
                                l = c(x$output$mL[i, j] - x$output$dL[i, j], x$output$mL[i, j]),
                                u = c(x$output$mR[i, j] + x$output$dR[i, j], x$output$mR[i, j]))
      }
      output[[i]] <- fzarray(output[[i]])
      names(output[[i]]) <- colnames(x$output$mL)
    }
    names(output) <- rownames(x$output$mL)

    res <- structure(list(input = input,
                          output = output,
                          dmunames = x$dmunames,
                          nc_inpunts = x$nc_inputs,
                          nc_outputs = x$nc_outputs,
                          nd_inputs = x$nd_inputs,
                          nd_outputs = x$nd_outputs,
                          ud_inputs = x$ud_inputs,
                          ud_outputs = x$ud_outputs),
                     class = "deadata_fzn")

  } else {
    stop("x should be of deadata or deadata_fuzzy class.")
  }

  return(res)

}
