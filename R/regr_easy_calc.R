#' @title
#' Calculation of Regression Models: Linear, Quadratic and Cubic.
#'
#' @description
#' Performs regression calculations: linear, quadratic and cubic, allowing to perform only one or both, returning a detailed result of the calculation
#'
#' @param x Values that should be used as an independent variable for the regression calculation.
#' @param y Values that should be used as a dependent variable for the regression calculation.
#' @param model Character, defined which model will be calculated. model = "L", calculate the linear, model = "Q" calculate the quadratic, model = "C" calculate the cubic, model = "all" = calculate both).
#'
#' @return
#' returns a list with the regression result (linear, quadratic and/or cube)
#'
#' @export
#'
#' @examples
#' library(regr.easy)
#' x <- seq(0,300,50)
#' y <- c(138.6,153.6,164.525,164.925,158.725,159.975,154.425)
#' regr_easy_calc(x,y,model = "all")



regr_easy_calc <- function(x,y,model="all"){
  #x-------------------------------------------------------------------
  if(model != "Q"& model != "L" & model != "C" & model != "all"){
    stop("The accepted parameters for model are: all, L, Q or C")
  }

  tryCatch({

    regreres <- list()

    if (model == "L" | model == "all") {
      #modelo linear
      ln <- stats::lm(y ~ x)

      #detalhes modelo
      ln_det <- summary(ln)
      row.names(ln_det$coefficients) <- c("b0", "b1")

      #tabela com modelos estimados
      obs_est_ln <-
        cbind(x, y, y_est = stats::fitted(ln), Resid = ln_det$residuals)

      #anova
      ln_an <- stats::anova(ln)
      row.names(ln_an) <-
        c("Linear effect", "Residuals")

      #formula
      lb0 <- round(ln_det$coefficients[1], 4)
      lb1 <- sprintf("%+f", ln_det$coefficients[2])

      form_ln <- data.frame(equation = paste0(lb0, lb1, "x"))

      #RESULTADO----
      stargazer::stargazer(obs_est_ln,
                           type = "text",
                           digits = 3,
                           title = "Observed and estimated - Linear Model")

      stargazer::stargazer(
        ln_an,
        type = "text",
        digits = 6,
        summary = F,
        title = "Analysis of Variance Table - Linear Model"
      )

      stargazer::stargazer(
        ln_det$coefficients,
        type = "text",
        digits = 6,
        title = "Linear Model"
      )

      stargazer::stargazer(
        ln_det$r.squared,
        type = "text",
        digits = 6,
        title = "R-square - Linear Model"
      )

      stargazer::stargazer(
        form_ln$equation,
        type = "text",
        digits = 6,
        title = "Equation - Linear Model",
        flip = FALSE,
        summary = F
      )

      #FIM----
      regreres["Observed and estimated - Linear Model"] <-
        list(obs_est_ln)
      regreres["Analysis of Variance Table - Linear Model"] <-
        list(ln_an[2:5])
      regreres["Linear Model"] <- list(ln_det$coefficients)
      regreres["R-square - Linear Model"] <- list(ln_det$r.squared)
      regreres["Equation - Linear Model"] <- list(form_ln$equation)

    }
    #x^2-------------------------------------------------------------------
    if (model == "Q" | model == "all") {
      #modelo quad
      quad <- stats::lm(y ~ x + I(x ^ 2))

      #detalhes modelo
      quad_det <- summary(quad)
      row.names(quad_det$coefficients) <- c("b0", "b1", "b2")

      #tabela com modelos estimados
      obs_est_quad <-
        cbind(x, y, y_est = stats::fitted(quad), Resid = quad_det$residuals)

      #anova
      quad_an <- stats::anova(quad)
      row.names(quad_an) <-
        c("Linear effect", "Quadratic effect", "Residuals")

      #formula
      b0 <- round(quad_det$coefficients[1], 4)
      b1 <- sprintf("%+f", quad_det$coefficients[2])
      b2 <- sprintf("%+f", quad_det$coefficients[3])

      form_quad <- data.frame(equation = paste0(b0, b1, "x", b2, "x^2"))

      #RESULTADO----
      stargazer::stargazer(obs_est_quad,
                           type = "text",
                           digits = 3,
                           title = "Observed and estimated - Quadratic Model")

      stargazer::stargazer(
        quad_an,
        type = "text",
        digits = 6,
        summary = F,
        title = "Analysis of Variance Table - Quadratic Model"
      )

      stargazer::stargazer(
        quad_det$coefficients,
        type = "text",
        digits = 6,
        title = "Quadratic Model"
      )

      stargazer::stargazer(
        quad_det$r.squared,
        type = "text",
        digits = 6,
        title = "R-square - Quadratic Model"
      )

      stargazer::stargazer(
        form_quad$equation,
        type = "text",
        digits = 6,
        title = "Equation - Quadratic Model",
        flip = FALSE,
        summary = F
      )

      #FIM----
      regreres["Observed and estimated - Quadratic Model"] <-
        list(obs_est_quad)
      regreres["Analysis of Variance Table - Quadratic Model"] <-
        list(quad_an[2:5])
      regreres["Quadratic Model"] <- list(quad_det$coefficients)
      regreres["R-square - Quadratic Model"] <- list(quad_det$r.squared)
      regreres["Equation - Quadratic Model"] <- list(form_quad$equation)

    }

    #x^3----
    if(model=="C"|model=="all"){
      #modelo quad
      cub <- stats::lm(y ~ x + I(x ^ 2) + I(x ^ 3))

      #detalhes modelo
      cub_det <- summary(cub)
      row.names(cub_det$coefficients) <- c("b0", "b1", "b2","b3")

      #tabela com modelos estimados
      obs_est_cub <-
        cbind(x, y, y_est = stats::fitted(cub), Resid = cub_det$residuals)

      #anova
      cub_an <- stats::anova(cub)
      row.names(cub_an) <-
        c("Linear effect", "Quadratic effect", "Cubic effect", "Residuals")

      #formula
      cb0 <- round(cub_det$coefficients[1], 4)
      cb1 <- sprintf("%+f", cub_det$coefficients[2])
      cb2 <- sprintf("%+f", cub_det$coefficients[3])
      cb3 <- sprintf("%+f", cub_det$coefficients[4])

      form_cub <- data.frame(equation = paste0(cb0, cb1, "x", cb2, "x^2", cb3,
                                               "x^3"))

      #RESULTADO----
      stargazer::stargazer(obs_est_cub,
                           type = "text",
                           digits = 3,
                           title = "Observed and estimated - Cubic Model")

      stargazer::stargazer(
        cub_an,
        type = "text",
        digits = 6,
        summary = F,
        title = "Analysis of Variance Table - Cubic Model"
      )

      stargazer::stargazer(
        cub_det$coefficients,
        type = "text",
        digits = 6,
        title = "Cubic Model"
      )

      stargazer::stargazer(
        cub_det$r.squared,
        type = "text",
        digits = 6,
        title = "R-square - Cubic Model"
      )

      stargazer::stargazer(
        form_cub$equation,
        type = "text",
        digits = 6,
        title = "Equation - Cubic Model",
        flip = FALSE,
        summary = F
      )

      #FIM----
      regreres["Observed and estimated - Cubic Model"] <-
        list(obs_est_cub)
      regreres["Analysis of Variance Table - Cubic Model"] <-
        list(cub_an[2:5])
      regreres["Cubic Model"] <- list(cub_det$coefficients)
      regreres["R-square - Cubic Model"] <- list(cub_det$r.squared)
      regreres["Equation - Cubic Model"] <- list(form_cub$equation)
      invisible(regreres)
    }

  },
  warning=function(war)
  {
    print(paste("WARNING: ", war))
  },
  error=function(err)
  {
    print(paste("ERROR: ", err))
  })
}

