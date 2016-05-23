library(shiny)

shinyServer(function(input, output) {

  # initial hyperparameters
  a_1 <- 2; b_1 <- 2
  a_2 <- 2; b_2 <- 2

  # experiment values
  N1 <- 40; z1 <- 4
  N2 <- 15; z2 <- 14

  # prior parameters
  a1 <- z1 + a_1; b1 <- N1 - z1 + b_1
  a2 <- z2 + a_2; b2 <- N2 - z2 + b_2

  output$stepPlot <- renderPlot({

    set.seed(111)

    # initialize parameters
    x <- rep(0, 200); y <- rep(0, 200)

    # initial values for parameters
    x[1] <- 0; y[1] <- 0

    # sample the parameters
    x[2:200] <- rbeta(199, a1, b1)
    y[2:200] <- rbeta(199, a2, b2)

    # initialize sequence for plots
    seq.x <- seq(0, 1, length.out = 200)

    # number of steps
    N <- as.numeric(input$n_steps)

    # define plot layout
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    nf <- layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)

    # plot
    par(mar = c(3, 3, 1, 1))
    if (N > 0){
      plot(x[1:N+1], y[1:N+1],
           xlim = c(0, 1), ylim = c(0, 1),
           frame = FALSE,
           col = "midnightblue",
           pch = 16)
      # initial value
      points(x[1], y[1], col = "firebrick", pch = 15)
    }else{
      plot(x[1], y[1],
           xlim = c(0, 1), ylim = c(0, 1),
           frame = FALSE,
           xlab = expression(theta[1]), ylab = expression(theta[2]),
           col = "firebrick", pch = 15)
    }

    # steps
    if (N > 0 & input$addition == "Steps"){
      for (i in 1:N) {
        arrows(x[i], y[i],  x[i+1], y[i], col = "forestgreen", length = .1)
        arrows(x[i+1], y[i],  x[i+1], y[i+1], col = "skyblue", length = .1)
      }
    }else{
      if(input$addition == "True Contours"){

        grid.theta <- expand.grid(theta1 = seq.x, theta2 = seq.x)
        values <- matrix(data = apply(grid.theta, 1, function(x){dbeta(x[1], a1, b1) * dbeta(x[2], a2, b2)}),
                         nrow = length(seq.x))

        contour(x = seq.x, y = seq.x, z = values, #TODO set contour levels
                add = TRUE,
                # levels = c(qnorm(.975)),
                col = "forestgreen")
      }
    }

    # density plots on axis
    if (N > 0 | input$addition == "True Contours"){
      par(mar = c(0, 3, 1, 1))
      plot(seq.x, dbeta(seq.x, a1, b1),
           type = "l",
           frame.plot = FALSE, axes = FALSE,
           col = "forestgreen",
           lwd = 5, xlab = "", ylab = "")
      text(.5, 2, expression(f[1](theta[1])))

      par(mar = c(3, 0, 1, 1))
      plot(dbeta(seq.x, a2, b2), seq.x,
           type = "l",
           frame.plot = FALSE, axes = FALSE,
           col = "skyblue",
           lwd = 5,
           xlab = "", ylab = "")
      text(2, .5, expression(f[2](theta[2])))
      # reset default plot values
      par(def.par)
      }
  })

})
