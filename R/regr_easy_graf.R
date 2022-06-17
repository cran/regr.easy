#' @title
#' Regression Model Graphs: Linear, Quadratic and Cubic.
#'
#' @description
#' It makes graphs for the regression models: linear, quadratic and cubic, allowing the plotting of the R-square, the equation, and other aspects related to regression.
#'
#' @import ggplot2
#'
#' @param x Values that should be used as an independent variable for the regression calculation.
#' @param y Values that should be used as a dependent variable for the regression calculation.
#' @param model Character, defined which model will be calculated. model = "L", calculate the linear, model = "Q" calculate the quadratic, model = "C" calculate the cubic, model = "all" = calculate both). Default "L".
#' @param plot_eq Logical, if TRUE (default) plots the regression equation on the graph.
#' @param plot_R2 Logical, if TRUE (default) plots the regression R-square on the graph.
#' @param plot_res Logical, if true (default), it plots segments referring to the residuals in the graph.
#' @param title Character, title of the graph.
#' @param subtitle Character, subtitle of the graph.
#' @param title_x Character, x axis label in plot.
#' @param title_y Character, y axis label in plot.
#' @param pch y and x plot symbol. Default = 21.
#' @param pch_size,pch_fill,pch_colour Size, padding and contour of points (pch) of y and x. Defaults = 2.5, "black", "black").
#' @param point_max Logical, if TRUE, the value corresponding to the maximum value will be added to the graph. Valid only for model="Q". Default = FALSE.
#' @param equ_pos A vector of 2 values to position the equation on the graph, if NULL will be plotted at a predefined position.
#' @param R2_pos A vector of 2 values to position the R-square on the graph, if NULL will be plotted at a predefined position.
#' @param l_type,l_color Line type e color to use in the regression equation curve. Defaults = 1,"red".
#' @param col_resid Color to be used in the residuals of the regression equation. Default = "red.
#' @param ax_size Size for axis marking labels. Default = 12.
#' @param ax_title_size Size for axis titles. Defaults = 12,12.
#' @param equ_tex_size Size for the regression equation e R-square. Default = 12.
#' @param pch_max Symbol of the maximum value of the quadratic regression model. Default = 4.
#' @param pmax_size,pmax_fill,pmax_col Size, padding and outline of the maximum value symbol of the quadratic regression model. Defaults = 2.5, "red, "red.
#' @param lmax_type,lmax_col,lmax_size,lmax_alpha Type, color, size and transparency of the maximum value line of the quadratic regression model. Defaults = 2, "red", 0.5, 1.
#'
#' @return Returns a ggplot2 for the defined regression model.
#'
#' @export
#'
#' @examples
#' library(regr.easy)
#' x <- seq(0,300,50)
#' y <- c(138.6,153.6,164.525,164.925,158.725,159.975,154.425)
#' regr_easy_graf(x,y, model = "Q")

regr_easy_graf <- function(x,
                           y,
                           model = "L",
                           plot_eq = TRUE,
                           plot_R2 = TRUE,
                           plot_res = TRUE,
                           title = "",
                           subtitle = "",
                           title_x = "x",
                           title_y = "y",
                           pch = 21,
                           pch_size = 2.5,
                           pch_fill = "black",
                           pch_colour = "black",
                           point_max = FALSE,
                           equ_pos = NULL,
                           R2_pos = NULL,
                           l_type = 1,
                           l_color = "red",
                           col_resid = "red",
                           ax_size = 12,
                           ax_title_size = 12,
                           equ_tex_size = 12,
                           pch_max = 4,
                           pmax_size = 2.5,
                           pmax_fill = "red",
                           pmax_col = "red",
                           lmax_type = 2,
                           lmax_col = "red",
                           lmax_size = 0.5,
                           lmax_alpha = 1) {

  if(model != "Q"& model != "L" & model != "C"){
    stop("The accepted parameters for model are: L, Q or C")
  }



  tryCatch({

    #x-------------------------------------------------------------------
    if (model == "L") {
      #modelo quad
      lngr <- stats::lm(y ~ x)

      #detalhes modelo
      lngr_det <- summary(lngr)
      row.names(lngr_det$coefficients) <- c("b0", "b1")

      #definindo equação pra colocar no grafico
      eq_quad <- sprintf(
        "y == %f %+f*x",
        lngr_det$coefficients[1],
        lngr_det$coefficients[2]
      )

      #convertendo a proporção pra geom_tex
      ftr = round(equ_tex_size / (14 / 5), 0)


      #definindo r2 pra colocar no grafico
      R2_val = sprintf("R^{2} == %2.2f", lngr_det$r.squared)

      #definindo posições se = NULL
      if (is.null(equ_pos)) {
        equ_pos = c(round(mean(x)*1.25,0),
                    round(min(y)*1.02,0))
      }

      if (is.null(R2_pos)) {
        R2_pos = c(equ_pos[1], equ_pos[2] - equ_pos[2] * 0.015)
      }


      #plotando Grafico

      ggquad <- ggplot(mapping = aes(y = y, x = x))


      if (isTRUE(plot_res)) {
        ggquad <- ggquad + geom_segment(aes(
          x = x,
          y = y,
          xend = x,
          yend = stats::fitted(lngr),
          colour = col_resid
        )) +
          scale_colour_identity()
      }

      ggquad <- ggquad + geom_point(
        size = pch_size,
        pch = pch,
        fill = pch_fill,
        colour = pch_colour
      ) +
        geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = F,
          color = l_color,
          linetype = l_type
        ) +
        labs(
          title = title,
          subtitle = subtitle,
          x = title_x,
          y = title_y
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background  = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text = element_text(colour = "black", size=ax_size),
          axis.title = element_text(size = ax_title_size)
        )

      #plots opcionais
      if (isTRUE(plot_eq)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = equ_pos[1],
          y = equ_pos[2],
          label = eq_quad
        ),
        size = ftr,
        parse = T)
      }

      if (isTRUE(plot_R2)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = R2_pos[1],
          y = R2_pos[2],
          label = R2_val
        ),
        size = ftr,
        parse = T)
      }

    }
    #x^2-------------------------------------------------------------------
    if (model == "Q") {
      #modelo quad
      quadgr <- stats::lm(y ~ x + I(x ^ 2))

      #detalhes modelo
      quadgr_det <- summary(quadgr)
      row.names(quadgr_det$coefficients) <- c("b0", "b1", "b2")

      #definindo equação pra colocar no grafico
      eq_quad <- sprintf(
        "y == %f %+f*x %+f*x^{2}",
        quadgr_det$coefficients[1],
        quadgr_det$coefficients[2],
        quadgr_det$coefficients[3]
      )

      #calcula a melhor dose pela derivada
      ex <- quadgr_det$coefficients[2]/(-2*quadgr_det$coefficients[3])
      #aplica a melhor dose na formula da regressão
      ey <- quadgr_det$coefficients[1]+
        quadgr_det$coefficients[2]*ex+
        quadgr_det$coefficients[3]*ex^2


      #convertendo a proporção pra geom_tex
      ftr = round(equ_tex_size / (14 / 5), 0)


      #definindo r2 pra colocar no grafico
      R2_val = sprintf("R^{2} == %2.2f", quadgr_det$r.squared)

      #definindo posições se = NULL
      if (is.null(equ_pos)) {
        equ_pos = c(round(mean(x)*1.25,0),
                    round(min(y)*1.02,0))
      }

      if (is.null(R2_pos)) {
        R2_pos = c(equ_pos[1], equ_pos[2] - equ_pos[2] * 0.015)
      }


      #plotando Grafico

      ggquad <- ggplot(mapping = aes(y = y, x = x))


      if (isTRUE(plot_res)) {
        ggquad <- ggquad + geom_segment(aes(
          x = x,
          y = y,
          xend = x,
          yend = stats::fitted(quadgr),
          colour = col_resid
        )) +
          scale_colour_identity()
      }

      ggquad <- ggquad + geom_point(
        size = pch_size,
        pch = pch,
        fill = pch_fill,
        colour = pch_colour
      ) +
        geom_smooth(
          method = "lm",
          formula = y ~ x + I(x ^ 2),
          se = F,
          color = l_color,
          linetype = l_type
        ) +
        labs(
          title = title,

          subtitle = subtitle,
          x = title_x,
          y = title_y
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background  = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text = element_text(colour = "black", size=ax_size),
          axis.title = element_text(size = ax_title_size)
        )
      if (isTRUE(point_max)) {
        ggquad <- ggquad+
          geom_hline(
            yintercept = ey,
            linetype = lmax_type,
            color = lmax_col,
            size = lmax_size,
            alpha = lmax_alpha
          ) +
          geom_vline(
            xintercept = ex,
            linetype = lmax_type,
            color = lmax_col,
            size = lmax_size,
            alpha = lmax_alpha
          ) +
          geom_point(
            mapping = aes(x = ex, y = ey),
            size = pmax_size,
            pch = pch_max,
            fill = pmax_fill,
            colour = pmax_col
          )
      }
      #plots opcionais
      if (isTRUE(plot_eq)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = equ_pos[1],
          y = equ_pos[2],
          label = eq_quad
        ),
        size = ftr,
        parse = T)
      }

      if (isTRUE(plot_R2)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = R2_pos[1],
          y = R2_pos[2],
          label = R2_val
        ),
        size = ftr,
        parse = T)
      }

    }
    #X^3-------------------------
    if (model == "C") {
      #modelo quad
      cubgr <- stats::lm(y ~ x + I(x ^ 2) + I(x ^ 3))

      #detalhes modelo
      cubgr_det <- summary(cubgr)
      row.names(cubgr_det$coefficients) <- c("b0", "b1","b2","b3")

      #definindo equação pra colocar no grafico
      eq_quad <- sprintf(
        "y == %f %+f*x %+f*x^{2} %+f*x^{3}",
        cubgr_det$coefficients[1],
        cubgr_det$coefficients[2],
        cubgr_det$coefficients[3],
        cubgr_det$coefficients[4]
      )

      #convertendo a proporção pra geom_tex
      ftr = round(equ_tex_size / (14 / 5), 0)


      #definindo r2 pra colocar no grafico
      R2_val = sprintf("R^{2} == %2.2f", cubgr_det$r.squared)

      #definindo posições se = NULL
      if (is.null(equ_pos)) {
        equ_pos = c(round(mean(x)*1.25,0),
                    round(min(y)*1.02,0))
      }

      if (is.null(R2_pos)) {
        R2_pos = c(equ_pos[1], equ_pos[2] - equ_pos[2] * 0.015)
      }


      #plotando Grafico

      ggquad <- ggplot(mapping = aes(y = y, x = x))


      if (isTRUE(plot_res)) {
        ggquad <- ggquad + geom_segment(aes(
          x = x,
          y = y,
          xend = x,
          yend = stats::fitted(cubgr),
          colour = col_resid
        )) +
          scale_colour_identity()
      }

      ggquad <- ggquad + geom_point(
        size = pch_size,
        pch = pch,
        fill = pch_fill,
        colour = pch_colour
      ) +
        geom_smooth(
          method = "lm",
          formula = y ~ x + I(x ^ 2) + I(x ^ 3),
          se = F,
          color = l_color,
          linetype = l_type
        ) +
        labs(
          title = title,
          subtitle = subtitle,
          x = title_x,
          y = title_y
        ) +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background  = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text = element_text(colour = "black", size=ax_size),
          axis.title = element_text(size = ax_title_size)
        )

      #plots opcionais
      if (isTRUE(plot_eq)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = equ_pos[1],
          y = equ_pos[2],
          label = eq_quad
        ),
        size = ftr,
        parse = T)
      }

      if (isTRUE(plot_R2)) {
        ggquad <- ggquad + geom_text(mapping = aes(
          x = R2_pos[1],
          y = R2_pos[2],
          label = R2_val
        ),
        size = ftr,
        parse = T)
      }

    }


    print(ggquad)

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
