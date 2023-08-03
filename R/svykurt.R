#' @title svykurt
#'
#' @description Calculates Pearson kurtosis with complex survey data
#'
#' @param x A formula (e.g., ~var1) specifying the variable on which to estimate kurtosis.
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#' @param na.rm Logical. Should cases with missing values be dropped?
#' @param excess Logical. The default (TRUE) subtracts 3 from the output, giving excess kurtosis.
#'
#' @return An object of class \code{svykurt} giving the kurtosis on x
#'
#' @examples
#' if (requireNamespace("survey")) {
#'  library(survey)
#'  data(toydata)
#' # Create survey design object
#' toydesign <- svydesign(data = toydata_w, ids = ~1, weights = ~weight)
#' # Print the excess kurtosis of a variable
#' svykurt(x = ~att_5, design = toydesign)
#'
#' @export
#'
#' @importFrom survey svymean svyvar


svykurt <- function(
    x,
    design,
    na.rm = FALSE,
    excess = TRUE
) {

  if (!inherits(design, "survey.design"))
    stop("design is not a survey design")

  # Storing variable in formula as string
  # This is necessary to construct the  moments formula below
  var_name <- as.character(x)[2]

  x <- model.frame(x, design$variables, na.action = na.pass)
  x <- as.matrix(x)

  if (ncol(x) > 1)
    stop("Only calculate kurtosis one variable at a time")

  if(na.rm){
    x <- x[!is.na(x)]
  }

  momnts_fmla <- paste0(
    "~",
    var_name,
    " + I(",
    var_name,
    "^2) + I(",
    var_name,
    "^3) + I(",
    var_name,
    "^4)")

  momnts <- svymean(
    as.formula(momnts_fmla),
    design,
    na.rm = na.rm
  )

centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = bquote(-3 * .(x^4) + 6 * .(x^2) * .(`I(x^2)`) - 4 * .(x) * .(`I(x^3)`) + .(`I(x^4)`)),
      sigma2 = bquote(.(`I(x^2)`) - .(x^2))
    )
  )

  return(centrl_momnts)

}

# This is the closest I've gotten to a working solution
centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = bquote(-3 * .(x^4) + 6 * .(x^2) * .(`I(x^2)`) - 4 * .(x) * .(`I(x^3)`) + .(`I(x^4)`)),
      sigma2 = bquote(.(`I(x^2)`) - .(x^2))
    )
  )

centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = bquote(-3 * x^4 + 6 * x^2 * `I(x^2)` - 4 * x * `I(x^3)` + `I(x^4)`),
      sigma2 = bquote(`I(x^2)` - x^2))
    )

}


  centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = eval(bquote(-3 * x^4 + 6 * x^2 * `I(x^2)` - 4 * x * `I(x^3)` + `I(x^4)`)),
      sigma2 = eval(bquote(`I(x^2)` - x^2))
    )
  )

}

bquote(-3 * x^4 + 6 * x^2 * `I(x^2)` - 4 * x * `I(x^3)` + `I(x^4)`)

  centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = eval(bquote(-3 * x^4 + 6 * x^2 * I(x^2) - 4 * x * I(x^3) + I(x^4))),
      sigma2 = eval(bquote(I(x^2) - x^2))
    )
  )
    

  
}

eval(bquote(-3 * x^4 + 6 * x^2 * I(x^2) - 4 * x * I(x^3) + I(x^4)))

  centrl_momnts <- svycontrast(
    momnts,
    list(mu4 = substitute(
    -3 * x_4 + 6 * x_2 * ix_2 - 4 * x_1 * ix_3 + ix_4,
    list(
    x_4 = as.symbol(paste0(var_name, "^4")),
    x_2 = as.symbol(paste0(var_name, "^2")),
    ix_2 = as.symbol(paste0("I(", var_name, "^2)")),
    x_1 = as.symbol(var_name),
    ix_3 = as.symbol(paste0("I(", var_name, "^3)")),
    ix_4 = as.symbol(paste0("I(", var_name, "^4)"))
    )
  ),
    sigma2 = substitute(
    ix_2 - x_2,
    list(
    x_2 = as.symbol(paste0(var_name, "^2")),
    ix_2 = as.symbol(paste0("I(", var_name, "^2)")),
    )
  )
  ))
}

substitute(
    -3 * x_4 + 6 * x_2 * ix_2 - 4 * x_1 * ix_3 + ix_4,
    list(
      x_4 = as.symbol(paste0(var_name, "^4")),
    x_2 = as.symbol(paste0(var_name, "^2")),
    ix_2 = as.symbol(paste0("I(", var_name, "^2)")),
    x_1 = as.symbol(var_name),
    ix_3 = as.symbol(paste0("I(", var_name, "^3)")),
    ix_4 = as.symbol(paste0("I(", var_name, "^4)"))
    )
  )

substitute(
      -3 * x_4 + 6 * x_2 * ix_2 - 4 * x * ix_3 + ix_4,
      list(
        x_4 = as.symbol(x^4),
        x_2 = as.symbol(x^2),
        ix_2 = as.symbol(I(x^2)),
        ix_3 = as.symbol(I(x^3)),
        ix_4 = as.symbol(I(x^4))
      )
    )

  substitute(
    ix_2 - x_2,
    list(
    x_2 = as.symbol(paste0(var_name, "^2")),
    ix_2 = as.symbol(paste0("I(", var_name, "^2)")),
    )
  )

}

x_4 <- var_name^4
x_2 <- x^2
ix_2 <- I(x^2)
ix_3 <- I(x^3)
ix_4 <- I(x^4)


  mu4_fmla <- paste0(
    "mu4 = quote(-3 * ",
    var_name,
    "^4 + 6 * ",
    var_name,
    "^2 * `I(",
    var_name,
    "^2)` - 4 * ",
    var_name,
    " * `I(",
    var_name,
    "^3)` + `I(",
    var_name,
    "^4)`)"
  )

  sigma2_flma <- paste0(
    "sigma2 = quote(`I(",
    var_name,
    "^2)` - ",
    var_name,
    "^2)"
  )

  

  centrl_momnts <- svycontrast(
    momnts,
    list(
      mu4 = bquote(
        -3 * .(as.name(x_4))
}


       -3 * .(as.name(x_4)) + 6 * .(as.name(x_2)) * .(as.name(ix_2)) - 4 * .(as.name(x)) * .(as.name(ix_3)) + .(as.name(ix_4))),
      sigma2 =  bquote(.(as.name(ix_2)) - .(as.name(x_2)))
      )
    )


  return(centrl_momnts)

}


print(substitute(
    -3 * x_4 + 6 * x_2 * ix_2 - 4 * x_1 * ix_3 + ix_4,
    list(
      x_4 = as.symbol(paste0(var_name, "^4")),
    x_2 = as.symbol(paste0(var_name, "^2")),
    ix_2 = as.symbol(paste0("I(", var_name, "^2)")),
    x_1 = as.symbol(var_name),
    ix_3 = as.symbol(paste0("I(", var_name, "^3)")),
    ix_4 = as.symbol(paste0("I(", var_name, "^4)"))
    )
  ))



print.svykurt <- function(x) {
  m <- as.matrix(x, ncol = 1)
  rownames(m) <- names(x)
  colnames(m) <- "kurtosis"

  print(m)
}
