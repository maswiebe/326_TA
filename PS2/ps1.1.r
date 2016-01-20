rm(list=ls()) ## clears R's workspace
graphics.off() ## closes any open graphs

## load needed packages
require(ggplot2)  ## run "install.packages("ggplot2")" if you get an error here
require(reshape)  ## run "install.packages("reshape")" if you get an error here

N <- c(8,32,128) ## Sample sizes
simulations <- 5000 ## number of simulations for each sample size
avg1 <- c(1,1,1)

means1 <- matrix(0,nrow=simulations,ncol=length(N))
unscaled.means1 <- means1
means2 <- matrix(0,nrow=simulations,ncol=length(N))
unscaled.means2 <- means2
means3 <- matrix(0,nrow=simulations,ncol=length(N))
unscaled.means3 <- means3

for(i in 1:length(N)) { ## loop over sample sizes
  n <- N[i]
  dat <- matrix(rnorm(n*simulations), nrow=simulations, ncol=n)
  means1[,i] <- (apply(dat, 1, mean))*sqrt(n)
  unscaled.means1[,i] <- apply(dat,1,mean)
  #avg1(i) <- mean(means1[,i])
  # need to get mean and variance of each sample, show var = sigma^2/n
}

for(i in 1:length(N)) { ## loop over sample sizes
  n <- N[i]
  dat <- matrix(runif(n*simulations), nrow=simulations, ncol=n)
  means2[,i] <- (apply(dat, 1, mean) - 0.5)*sqrt(n)
  unscaled.means2[,i] <- apply(dat,1,mean)
}

for(i in 1:length(N)) { ## loop over sample sizes
  n <- N[i]
  dat <- matrix(rcauchy(n*simulations), nrow=simulations, ncol=n)
  means3[,i] <- (apply(dat, 1, mean))*sqrt(n) # Cauchy undefined mean?
  unscaled.means3[,i] <- apply(dat,1,mean)
}


## Creating histograms
df1 <- data.frame(means1)
df1<- melt(df1)

cltPlot1 <- ggplot(data=df1, aes(x=value, fill=variable)) +

  geom_histogram(alpha=0.2, position="identity") +

  scale_x_continuous(name=expression(sqrt(n)(hat(mu) - mu))) + ## label x-axis
  scale_fill_brewer(type="div",palette="RdYlGn",
                    name="N",label=N) + ## controls colors of histograms
  labs(title="Histogram of sample mean for N(0,1) distribution")
cltPlot1 ## display the plot

## saves the plot as pdf
fdims <-1.5*c(5.03937, 3.77953) ## figure dimensions, =128mm x 96mm = dimensions of beamer slide
pdf("cltPlot_norm.pdf",width=fdims[1],height=fdims[2])
cltPlot1
dev.off()
#--------------
df2 <- data.frame(means2)
df2 <- melt(df2)

cltPlot2 <- ggplot(data=df2, aes(x=value, fill=variable)) +

  geom_histogram(alpha=0.2, position="identity") +

  scale_x_continuous(name=expression(sqrt(n)(hat(mu) - mu))) + ## label x-axis
  scale_fill_brewer(type="div",palette="RdYlGn",
                    name="N",label=N) + ## controls colors of histograms
  labs(title="Histogram of sample mean for U(0,1) distribution")
cltPlot2 ## display the plot

## saves the plot as pdf
fdims <-1.5*c(5.03937, 3.77953) ## figure dimensions, =128mm x 96mm = dimensions of beamer slide
pdf("cltPlot_unif.pdf",width=fdims[1],height=fdims[2])
cltPlot2
dev.off()
#-----------------
df3 <- data.frame(means3)
df3 <- melt(df3)

cltPlot3 <- ggplot(data=df3, aes(x=value, fill=variable)) +

  geom_histogram(alpha=0.2, position="identity") +

  scale_x_continuous(name=expression(sqrt(n)(hat(mu) - mu))) + ## label x-axis
  scale_fill_brewer(type="div",palette="RdYlGn",
                    name="N",label=N) + ## controls colors of histograms
  labs(title="Histogram of sample mean for Cauchy(0,1) distribution")
cltPlot3 ## display the plot

## saves the plot as pdf
fdims <-1.5*c(5.03937, 3.77953) ## figure dimensions, =128mm x 96mm = dimensions of beamer slide
pdf("cltPlot.pdf",width=fdims[1],height=fdims[2])
cltPlot3
dev.off()
