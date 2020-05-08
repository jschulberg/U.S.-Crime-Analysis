#######################################################################
#######################################################################

#####                     U.S. Crime Analysis                     #####

#######################################################################
#######################################################################
# In this script, I will analyze the U.S. Crime dataset included in the
# data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do so, including, but not limited to:

#   -- Outlier Detection
#   -- Principal Component Analysis (PCA)
#   -- Outlier Detection
#   -- Outlier Detection
#   -- Outlier Detection
#   -- Outlier Detection
#   -- Outlier Detection


#######################################################################
# Set Up --------------------------------------------------------------
#######################################################################
# Bring in packages
suppressMessages(library("dplyr")) # Used for data cleaning
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files
suppressMessages(library("factoextra")) # Used for PCA 

# Bring in the data, delimited by a tab ("\t")
data_crime <- read.delim("Data/uscrime.txt", header = T)

# Convert to a tibble, my preferred data structure
data_crime <- as_tibble(data_crime)

# Let's take a peek under the hood
head(data_crime)
summary(data_crime)

# Below is a list of the variables in data along with their associated descriptions. We want to predict the last
# column, Crime, based on the other predictor variables.
# Variable	 	Description
# M		percentage of males aged 14-24 in total state population
# So		indicator variable for a southern state
# Ed		mean years of schooling of the population aged 25 years or over
# Po1		per capita expenditure on police protection in 1960
# Po2		per capita expenditure on police protection in 1959
# LF		labour force participation rate of civilian urban males in the age-group 14-24
# M.F		number of males per 100 females
# Pop		state population in 1960 in hundred thousands
# NW		percentage of nonwhites in the population
# U1		unemployment rate of urban males 14-24
# U2		unemployment rate of urban males 35-39
# Wealth		wealth: median value of transferable assets or family income
# Ineq		income inequality: percentage of families earning below half the median income
# Prob		probability of imprisonment: ratio of number of commitments to number of offenses
# Time		average time in months served by offenders in state prisons before their first release
# Crime		crime rate: number of offenses per 100,000 population in 1960

#######################################################################
# Outliers ------------------------------------------------------------
#######################################################################
# In this section, we'll first test to see whether there are any
# outliers in the last column (number of crimes per 100,000 people). 
# To do so, we'll use the grubbs.test function in the outliers package in R.

# Save our visualization to the correct working directory
setwd("C:/Users/jschulberg/Documents/Data Analytics Blog/Blog 3 - US Crime/us-crime-analysis/Viz/")
jpeg(file = "U.S. Crime Boxplot.jpeg") # Name of the file we'll save

# First, let's look at a boxplot of the data
ggplot(data_crime, aes(y = Crime)) +
  geom_boxplot(outlier.colour="slateblue4",
               outlier.size=2,
               color = "slateblue3") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = "Crime Rate for 47 States",
       subtitle = "Year = 1960",
       caption = "*Number of offenses per 100,000 population") +
  ylab("Crime Rate*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete(limits=c("-.1", ".1"))

# Allow us to write other visualizations later on
dev.off()

# Now that we have a clear idea of what the outliers could look like, let's use the grubbs.test to
# better understand these outliers.
data_grubbs <- data_crime 
(grubbs1 <- grubbs.test(data_grubbs$Crime))
grubbs1$alternative 
# This indicates that the highest value 1993 would be an outlier if there were one in the set

(p_value <- grubbs1$p.value) 
# Since the p-value is above .05 (.079, to be exact), we cannot say with confidence that there
# is an outlier in the set. Let's move on.



#######################################################################
# Principal Component Analysis ----------------------------------------
#######################################################################
# In this section, I'll apply Principal Component Analysis, a method
# for trimming down the number of variables in our dataset and for
# variable selection. This *should* help with model-building later on.

# Start by getting rid of the Crime column for now since we won't perform
# PCA on the one column we are aiming to predict.
base_data <- data_crime %>%
  select(-Crime)

# Because we'll need this to unscale our data later on, let's track the means 
# and standard deviations of each variable
mu <- sapply(base_data, mean)
std_devs <- sapply(base_data, sd)

# Before we continue, let's set our seed to ensure randomness
set.seed(123)

# Let's also turn off scientific notation, because it'll make our results messy to read
options(scipen = 999)

# Run our first Principle Component Analysis using the prcomp() function which uses 
# Singular value decomposition (SVD), which examines the covariances / correlations 
# between individual observations. Note: We won't scale the data within the function 
# because we already scaled it earlier.
pca1 <- prcomp(base_data, scale. = T)

summary(pca1)
# We can see that, from our Cumulative Proportion dimension, the first six principle components explain about 90% of the 
# variation in the data.

# Create a graph of the individual observations. Individual states with a similar profile of factors will be grouped.
(ind_plot <- fviz_pca_ind(pca1,
                          col.ind = "slateblue2", # Shade the viz by the quality of representation
                          repel = T, # Avoid text overlapping
                          alpha.ind = .8 # Add some transparency to better show overlayed points
)
)

# Create a graph of the variables (columns in our dataset). 
# Positively correlated variables point to the same side of the plot. 
# Negatively correlated variables point to opposite sides of the plot.
fviz_pca_var(pca1,
             # Shade the viz by the quality of representation
             col.var = "slateblue2", 
             # Avoid text overlapping
             repel = T
)

# What are our eigenvalues?
(pca_eig_val <- get_eigenvalue(pca1))
# Note that the variance.percent values noted here align with our screeplot above. This also now tells us
# that 90% of the variance in our data is explained by just 6 dimensions.

# Let's graph the cumulative variance percentages so it's easier to visualize.
# Before we do so, let's bring the dimension numbers in as a column.
pca_cum_var <- pca_eig_val %>%
  mutate("dimension" = row.names(pca_eig_val))

# Clean up the dimensions so they're more interpretable and make sure they're read in as factors
pca_cum_var$dimension <- as.factor(substr(pca_cum_var$dimension, 5, length(pca_cum_var$dimension)))

jpeg(file = "U.S. Crime PCA Variation") # Name of the file for the viz we'll save

# Viz time
ggplot(pca_cum_var, aes(x = dimension)) +
  # Create our bars
  geom_col(aes(y = variance.percent), fill = "slateblue1") +
  # Let's try to add a line to represent our cumulative variance to close out our visualization
  geom_path(aes(y = cumulative.variance.percent, group = 1), color = "slateblue1", size = .5) +
  # Let's try to overlay the cumulative variance as points
  geom_point(aes(y = cumulative.variance.percent),  color = "slateblue4", size = 2) +
  # Ensure that everything's ordered by dimension
  scale_x_discrete(limits = pca_cum_var$dimension) +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = "Variation explained by the dimensions\nin our dataset",
       subtitle = "This uses the method of Principle Component Analysis",
       y = "Variance (%)",
       x = "Dimension") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4", size = 14),
        plot.subtitle = element_text(color = "dark gray", size = 10))

# In this visualization, we can see that the first 6 dimensions explain about 90% of the
# variance in our dataset, so we'll opt to keep these.



########################################################################
# Linear Regression ----------------------------------------------------
######################################################################## 
# Now we'll bring our crime data (our dependent variable) back in and see how our predictions are using the principle
# components. First, let's limit our data to just the first six principle components, based on
# our analysis above.
# Create a variable for the number of coefficients we'll be pulling
pca_len <- 6
pca_vals <- pca1$x[, 1:pca_len]

pca_vals <- pca_vals %>%
  cbind(Crime = data_crime$Crime) %>%
  as_tibble()

mod1 <- lm(Crime ~ ., data = pca_vals)

summary(mod1)
# Notice that our adjusted R-squared value is 60.7%. This is significantly lower than what we saw in the previous homework (somewhere 
# around 75-80%), but we've significantly reduced the number of variables we're using.

### Transform our data back
# Get the original intercept from our model
(b0 <- mod1$coefficients[1])

# Pull out model coefficients so we can make the Beta vector
(b_vals <- mod1$coefficients[2:(pca_len + 1)])

# To get our data re-transformed, we'll have to multiply the coefficients by our rotated matrix, A to create alpha vector
# Note: The "%*%" operator enables us to perform matrix multiplication.
# First let's verify that we are using the correctly-sized objects for this.
if ( dim(pca1$rotation[, 1:pca_len])[2] == length(b_vals) ) {
  "Great work. Let's keep on going."
} else {
  "Hold on. Something seems wrong."
}

(rota_matr <- pca1$rotation[, 1:pca_len] %*% b_vals)

# Using the standard deviation values (std_devs) and mean values (mu) from our original dataset, 
# let's recover (unscale) our original coefficient values by dividing the rotation matrix vector by our standard deviations
# and get our original intercept by subtracting the sum of (rota_matr * mu)/std_devs from the intercept
orig_coefs <- rota_matr / std_devs
orig_b0 <- b0 - sum(rota_matr * mu / std_devs)

## Get our original data's formula (coefficients + intercept) such that our model is of the form Y = aX + b
# Before we get the original formula, let's verify that we are using the correctly-sized objects for this. This is important
# for matrix multiplication
if ( dim(data[, 1:15])[2] == dim(orig_coefs)[1] ) {
  "Great work. Let's keep on going."
} else {
  "Hold on. Something seems wrong."
}

estimates <- as.matrix(data[, 1:15]) %*% orig_coefs + orig_b0

summary(estimates)
# The estimates represent what the PCA model expects our crime data to be based on the six principle components we identified.
# These are not exact, because our PCA model really only explains about 90% of the variation within our dataset.

## Predict
# Now, let's predict on the dataset we've been provided. First we'll store all the new data we've been provided into a data frame.
M = 14.0
So = 0
Ed = 10.0
Po1 = 12.0
Po2 = 15.5
LF = 0.640
M.F = 94.0
Pop = 150
NW = 1.1
U1 = 0.120
U2 = 3.6
Wealth = 3200
Ineq = 20.1
Prob = 0.04
Time = 39.0

# Bring all this new data into one data frame so it's all in one row and counts as one observation.
new_data <- data.frame(M = M, So = So, Ed = Ed, Po1 = Po1, Po2 = Po2, 
                       LF = LF, M.F = M.F, Pop = Pop, NW = NW, U1 = U1,
                       U2 = U2, Wealth = Wealth, Ineq = Ineq, Prob = Prob, Time = Time)


# Convert the new state data using the PCA we found earlier so we can apply our model.
new_pca <- data.frame(predict(pca1, new_data)) 

# So where does this new "state" fit? We'll use the graph we saw earlier and add this new point, manipulated utilizing
# our PCA, as a red dot on the graph.
fviz_add(ind_plot, new_pca, color ="red")

# Predict what the crime rate would now be by leveraging the new data that's had PCA applied
(pred <- predict(mod1, newdata = new_pca))

# Thus we predict that the crime rate in the new state would be 1248 crimes per 100,000 population. This seems
# within the range of reason for our data, especially given that the mean of our crime data is:
mean(data$Crime)
# and the range goes from 
range(data$Crime)[1] 
# to
range(data$Crime)[2]
