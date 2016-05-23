library(shiny)

shinyServer(function(input, output) {

  # initial hyperparameters
  m <- 2; v <- 4 # beta prior
  a <- 5; b <- 1 # gamma prior
  
  # experiment values
  z1 <- 1

  output$stepPlot <- renderPlot({

    set.seed(11)

    true.ng <- function(x, y){ y^(-2*(a-1)-1) * exp(-1/2*x^2 *(1/y^2 + 1/v) + x*(z1/y^2 + m/v) -z1^2/(2*y^2) - b/y^2 )  }

    # number of steps - input
    N <- 100
    N.input <- as.numeric(input$n_steps)

    # initialize parameters
    x <- rep(0, N); y <- rep(0, N)
    x[1] <- 1; y[1] <- 1
    full.cond.beta.mean <- rep(0, N); full.cond.beta.var <- rep(0, N);
    # full.cond.beta.var[2] <- v
    full.cond.invvar.a <- rep(0, N); full.cond.invvar.b <- rep(0, N);

    # Gibbs Sampling Loop
    for (i in 1:N) {
      full.cond.beta.mean[i+1] <- (z1*v + m*y[i])/(v + y[i])
      full.cond.beta.var[i+1] <- v*y[i]/(v + y[i])
      x[i + 1] <- rnorm(1, mean = full.cond.beta.mean[i+1], sqrt(full.cond.beta.var[i+1]))

      full.cond.invvar.a[i+1] <- a - 1/2
      full.cond.invvar.b[i+1] <- (z1 - x[i])^2/2 + b
      y[i + 1] <- rgamma(1, full.cond.invvar.a[i+1], full.cond.invvar.b[i+1])
    }

    # initialize sequence for plots
    x.start <- -2; x.end <- 10
    y.start <- 0; y.end <- 10
    seq.x <- seq(x.start, x.end, by = .001); seq.y <- seq(y.start, y.end, by = .001)


    # define the plot layout
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    nf <- layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1), c(1, 3), TRUE)

    # Sample Plot
    par(mar = c(3, 3, 1, 1))
    if (N.input > 0){
      plot(x[1 : N.input + 1], y[1 : N.input + 1],
           xlim = c(x.start, x.end), ylim = c(y.start, y.end),
           frame = FALSE)
      # initial value
      points(x[1], y[1], col = "firebrick", pch = 15)
    }else{
      plot(x[1], y[1],
           xlim = c(x.start, x.end), ylim = c(y.start, y.end),
           frame = FALSE,
           xlab = expression(theta[1]), ylab = expression(theta[2]),
           col = "firebrick", pch = 15)
    }

    # Gibbs Sampling Steps or Contour
    if (N.input > 0 & input$addition == "Steps"){
      for (i in 1 : N.input) {
        arrows(x[i], y[i],  x[i+1], y[i], col = "forestgreen", length = .1)
        arrows(x[i+1], y[i],  x[i+1], y[i+1], col = "skyblue", length = .1)
      }
    }else{
      if (input$addition == "True Contours"){
        grid.xy <- expand.grid(x = seq.x, y = seq.y)
        # values <- matrix(data = apply(grid.xy, 1, function(x){true.ng(x[1], x[2])}), #--- todo: exact posteriori function
        #                  nrow = length(seq.x))
        # 
        # contour(x = seq.x, y = seq.y, z = values,
        #         col = "forestgreen")
      }
    }

    # Density plots on the axis
    par(mar = c(0, 3, 1, 1))
    if (N.input > 0){
      plot(seq.x, dnorm(seq.x, mean = full.cond.beta.mean[2], sd = sqrt(full.cond.beta.var[2])),
           xlim = c(x.start, x.end), ylim = c(0, 2.5),
           type = "l",
           frame.plot = FALSE, axes = FALSE,
           col = "forestgreen",
           lwd = 2,
           xlab = "", ylab = "")
      # text(.25, 2, expression(theta[1]))
      if(N.input > 1){
        for(i in 2 : N.input){
          lines(seq.x, dnorm(seq.x, mean = full.cond.beta.mean[i+1], sd = sqrt(full.cond.beta.var[i+1])),
                type = "l",
                col = i + 5,
                lwd = 2)
        }
      }
    }

    par(mar = c(3, 0, 1, 1))
    if (N.input > 0){
      plot(dgamma(seq.y, full.cond.invvar.a[2], full.cond.invvar.b[2]), seq.y,
           ylim = c(y.start, y.end), xlim = c(0, 2.5),
           type = "l",
           frame.plot = FALSE, axes = FALSE,
           col = "skyblue",
           lwd = 2,
           xlab = "", ylab = "")
      # text(2, .5, expression(theta[2]))
      if (N.input > 1){
        for (i in 2 : N.input){
          lines(dgamma(seq.y, full.cond.invvar.a[i+1], full.cond.invvar.b[i+1]), seq.y,
                ylim = c(0, 20),
                type = "l",
                col = i + 5,
                lwd = 2)
        }
      }
    }

    # reset the default plot values
    par(def.par)
  })
  
})