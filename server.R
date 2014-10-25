
# This is the server logic for a Shiny web application.

library(shiny)

housing <- read.csv("data/Housing.csv")

shinyServer(function(input, output) {
   
  output$dotPlot <- renderPlot({
    
    input$rerun
    
    df <- run_bootstrap(input$sample_size)
    
    # draw the dot plot
    if (input$radio == 1) {
      x <- df$mean_price
    } else {
      x <- df$median_price
    }
    boot_mean <<- mean(x)
    boot_se <<- sd(x) / sqrt(sample_size)
    boot_ci_95 <<- boot_mean + c(-1, 1) * 1.96 * boot_se
    hg_dot(x)
  })
  
  output$title <- renderUI({
    if (input$radio == 1) {
      h2("Dotplot of Sample Mean")
    } else {
      h2("Dotplot of Sample Median")
    }
  })
  
  output$estimates <- renderUI({

    input$sample_size
    input$radio
    input$rerun

    p(paste("The Boot. mean is ", round(boot_mean), ". The Boot. SE is ", round(boot_se, 2),
      ". The 95% confidence interval for the parameter is (",
      paste(lapply(boot_ci_95, round), collapse=", "), ")", sep=""))
    
  })
  
})

run_bootstrap <- function(sample_size) {
  n <- 50
  sample_means <- rep(NA, n)
  sample_medians <- rep(NA, n)
  
  for (i in 1:n) {
    s <- sample(housing$price, sample_size, replace=T)
    sample_means[i] <- mean(s)
    sample_medians[i] <- median(s)
  }
  data.frame(mean_price=sample_means, median_price=sample_medians)
}

## Dotplot histogram
## Mark Gardener 2013
## www.dataanalytics.org.uk

hg_dot <- function(x, breaks = "sturges", offset = 0.4, cex = 3, pch = 19, ...) {
  # x = data vector
  # ... = other instructions for plot
  
  hg <- hist(x, breaks = breaks, plot = FALSE) # Make histogram data but do not plot
  bins <- length(hg$counts) # How many bin categories are needed?
  
  yvals <- numeric(0) # A blank variable to fill in
  for(i in 1:bins) { # Start a loop
    yvals <- c(yvals, hg$counts[i]:0) # Work out the y-values
  } # End the loop
  
  xvals <- numeric(0) # A blank variable
  for(i in 1:bins) { # Start a loop
    xvals <- c(xvals, rep(hg$mids[i], hg$counts[i]+1)) # Work out x-values
  } # End the loop
  
  dat <- data.frame(xvals, yvals) # Make data frame of x, y variables
  dat <- dat[yvals > 0, ] # Knock out any zero y-values
  
  minx <- min(hg$breaks) # Min value for x-axis
  maxx <- max(hg$breaks) # Max value x-axis
  
  miny <- min(dat$yvals) # Min value for y-axis
  maxy <- max(dat$yvals) # Max value for y-axis
  # Make the plot, without axes, allow ponts to overspill plot region
  
  plot(yvals + offset ~ xvals, data = dat, 
       xlim = c(minx, maxx), ylim = c(miny, maxy),
       axes = FALSE, ylab = "", xpd = NA,
       cex = cex, pch = pch, ...)
  
  axis(1) # Add in the x-axis
  invisible(dat) # Save the x, y data invisibly
  
}
