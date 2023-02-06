### Non-Religious Identities

# By Anne L. Mauritsen & Theiss Bendixen

### Load packages
library(psych)
library(brms)
library(bayesplot)
library(ggplot2)
library(ggtext)

### Load data
dat <- read.csv("identities.csv", sep = ";") # read in the .csv

View(dat) # check the raw data

data <- dat # copy raw data to new object

### Prep data

## Response options for each category

# Helt enig = 1 (Fully agree)
# Delvist enig = 2 (Partly agree)
# Delvist uenig = 3 (Partly disagree)
# Delvist uenig = 4 (Fully disagree)
# Ved ikke = 5 (Don't know)

# Participants get a one if they respond 1 or 2,
# so in the identity columns, if 2 or above, replace with a one, otherwise 0
for (c in colnames(data[,4:15])){
  data[,c] <- ifelse(data[,c] <= 2, 1, 0)
}

# count up number of identities per row (i.e., participant)
data$idsum <- rowSums(data[,4:15])

# make table of no. of identities and frequency of each
sumdata <- as.data.frame(table(data$idsum))

# get percentages for each row sum
sumdata$perc <- with(sumdata, (Freq/sum(Freq))*100)

# exploratory plot
plot(sumdata$perc, # plot percentages of each number of identities
     ylim = c(0,35), # length of y-axis
     xlab = "Number of identities", # x-axis label
     ylab = "Percentage of sample", # y-axis label
     type = "b", # points connected by lines
     xaxt="n", # remove x-axis labels (we defined new labels in the axis() command below)
     bty="l", # box type
     pch=1, # point type
     lwd=1.5 # line width
     )

title(main = "The percentage of the sample\nselecting a certain number of identities", # title text
      adj = 0) # left-align title

axis(side = 1, at=1:10, seq(0,9,by=1)) # x-axis ticks and labels

## Binomial regression
binmod <- brm(idsum | trials(12) ~ 1,
              data = data, 
              prior = set_prior("normal(0, 1)", class = "Intercept"),
              family = "binomial",
              cores = 4)

yrep <- posterior_predict(binmod) # marginal posterior predictions

color_scheme_set("mix-pink-gray")

# Plot posterior predictions
cairo_pdf("fig.pdf",
          width = 6, height = 4) # start print to pdf

ppc_bars(y = binmod$data[["idsum"]], 
                                yrep = yrep, 
                                freq = F,
                                prob = 0.95,
                                size = 0.1) + 
  scale_x_continuous(limits = c(0,12), n.breaks = 13) +
  theme(legend.position = "none",
        plot.title = element_markdown(), 
        plot.subtitle = element_markdown()) +
          labs(title = "*Complex Religious Identities*",
               subtitle = "Proportion of the sample subscribing to a number of identities.<br>
         <b style='color:#b97c9b;'>Raw proportions</b> vs. <b style='color:#000000;'>model-predicted proportions</b>.")

dev.off() # end print to pdf

## factor analysis
fadat <- na.omit(data[,4:15]) # Only complete-cases

nfactors(fadat)

fa.parallel(fadat) # 4-5 factors

factanal(fadat, factors=5)

### END ###
