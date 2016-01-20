rm(list=ls()) ## clears R's workspace
graphics.off() ## closes any open graphs

## load needed packages
require(ggplot2)  ## run "install.packages("ggplot2")" if you get an error here
require(reshape)  ## run "install.packages("reshape")" if you get an error here

N <- c(1,2,5,10,20,50,100) ## Sample sizes
simulations <- 5000 ## number of simulations for each sample size
means <- matrix(0,nrow=simulations,ncol=length(N))
unscaled.means <- means
## means = matrix of mean for each simulation and sample size
for(i in 1:length(N)) { ## loop over sample sizes
  n <- N[i]
  dat <- matrix(rbeta(n*simulations, shape1=0.5, shape2=0.5),
                ## draws n*simulations r.v.'s from beta(0.5,0.5)
                ## distribution. rbeta can be changed to runif, rnorm,
                ## rcauchy, etc. If rbeta is changed, the shape1=0.5,
                ## shape2=0.5 arguments must be deleted or changed
                nrow=simulations, ncol=n)
  ## matrix(whatever, nrow=simulations,ncol=n) takes the numbers from
  ## whatever and arranges them into a simulations by n matrix
  means[,i] <- (apply(dat, 1, mean) - ## apply(dat, 1, fun)  applies
                  ## the function fun to each row of the matrix. For the
                  ## assignment you will want fun to be either mean or var.
                  0.5)*sqrt(n)
  unscaled.means[,i] <- apply(dat,1,mean)
  ## We must subtract the population mean or variance. For the
  ## beta(0.5,0.5) distribution, the population mean is 0.5 and the variance is
  ## 0.5^2/2. This should be changed for other
  ## distributions. Wikipedia is an easy place to look up the mean and
  ## variance of many distributions.
}

## Creating histograms
df <- data.frame(means) ## Convert the means matrix to a dataframe
## because ggplot works better with dataframes
## than matrices.o
## We want to make a histogram of each column of means.
df <- melt(df) ## reshapes df to long format, which makes it easier to
## use with ggplot. For example, if initially
## df =  X1   X2
##      [0.1  0.4
##       0.3 -3.3
##      -0.5  0.6 ]
## then,
## melt(d) = variable value
##          [X1       0.1
##           X1       0.3
##           X1      -0.5
##           X2       0.4
##           X2      -3.3
##           X2       0.6]
cltPlot <- ggplot(data=df, aes(x=value, fill=variable)) +
  ## ggplot(..) creates a plot using data from dataframe df where the
  ## x variable is called value. The plots will be separated into
  ## groups by "variable"
  geom_histogram(alpha=0.2, position="identity") +
  ## geom_histogram(..) makes the plot a histogram. alpha=0.2 controls
  ## transparency, position="identity" makes the histograms for
  ## different groups lie on top of each other
  scale_x_continuous(name=expression(sqrt(n)(hat(mu) - mu))) + ## label x-axis
  scale_fill_brewer(type="div",palette="RdYlGn",
                    name="N",label=N) + ## controls colors of histograms
  labs(title="Histogram of sample mean for Beta(0.5,0.5) distribution")
cltPlot ## display the plot

## saves the plot as pdf
fdims <-1.5*c(5.03937, 3.77953) ## figure dimensions, =128mm x 96mm = dimensions of beamer slide
pdf("cltPlot.pdf",width=fdims[1],height=fdims[2])
cltPlot
dev.off()