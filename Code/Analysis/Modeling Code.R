#######################################################################
#######################################################################

#####                     U.S. Crime Analysis                     #####

####################################################################### 
#######################################################################
# In this script, I will analyze the U.S. Crime dataset included in the
# data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do so, including, but not limited to:

#   -- Linear Regression 
#   -- Principal Component Analysis (PCA) + Lin. Reg.


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


########################################################################
# Linear Regression ----------------------------------------------------
######################################################################## 
# In the first section, we'll use a simple linear regression model to
# predict on our crime data.
# Start by scaling the data so it's standardardized. Avoid column 2 because 
# it's an indicator (factor) for southern states
data_scaled <- as_tibble(scale(data_crime[, c(1, 3:16)]))

# bring column 2 back in and reorder our columns
data_scaled <- data_scaled %>%
  cbind(data_crime[, 2]) %>%
  select(Crime, everything())

# Before we build our first Linear Regression model, let's set our seed to ensure randomness
set.seed(123)

# Let's also turn off scientific notation, because it'll make our results messy to read
options(scipen = 999)

### Build our first model using every variable as an output. Print the results
(model_lm1 <- lm(Crime ~ ., data = data_scaled))

summary(model_lm1)
# The first two items we should look at are the R-squared value (80.3%) and the Adjusted R-squared value (71.8%)
# The next item we should look at is the overal p-value, which is p-value: 0.0000003539. This means that our crime data
# can be explained by the amalgam of predictor variables we provided



# What do our coefficients look like?
model_lm1$coefficients
# As an example of what this means, if M (percentage of males aged 14-24 in total state population) increases by 1,
# we would expect crime to increase by 87.83 crimes per 100,000 population.

# But which of the variables mattered and which did not? Well, to start, let's look at the individual p-values for each
# variable we have. We'll do this by pulling out the p-values and coefficient names.
p_vals <- summary(model_lm1)$coefficients[,4]
coefs <- rownames(summary(model_lm1)$coefficients)

# bring our coefficient names and p-values into a data frame
p_vals_df <- as_tibble(cbind(coefs, p_vals))

# remove the first row which includes the intercept
p_vals_df <- p_vals_df[-1, ]

# Make sure the p-values are read in as numerics
p_vals_df$p_vals <- as.double(p_vals_df$p_vals)

# Save our visualization to the correct working directory
setwd("C:/Users/jschulberg/Documents/Data Analytics Blog/Blog 3 - US Crime/us-crime-analysis/Viz/")
jpeg(file = "U.S. Crime Linear Regression.jpeg") # Name of the file for the viz we'll save

# Visualization time
ggplot(p_vals_df,
       # order by size of the p-values
       aes(x = reorder(coefs, -p_vals), y = p_vals)) +
  # Let's make it a bar graph and change the color
  geom_bar(stat = "identity", fill = "slateblue2") +
  # Change the theme to minimal
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Predictor Variable") +
  ylab("P-Value*") +
  labs(title = "P-Values for Different Predictors in the U.S. Crime Dataset",
       subtitle = "*The dashed line shows which p-values are above or below the threshold of .05") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "black"), 
        plot.subtitle = element_text(color = "dark gray", size = 10)) +
  # let's create a gray dashed, drop-line at .05 so we can see which variables are under our threshold
  geom_hline(yintercept = .05, linetype = "dashed", color = "light gray", size = .75) +
  # flip the axes and fix the axis
  coord_flip(ylim = c(0, 1)) 

dev.off()

# Let's confirm what we see above, especially since U2 seems on the border. Filter our data frame
# to only show p-values less than .05 and then order our results
signif_vars <- p_vals_df %>%
  filter(p_vals < 0.05) %>%
  arrange(p_vals)

# Pull the data back in
data_signif <- data_scaled[which(colnames(data_scaled) %in% signif_vars$coefs)]

# Bring Crime column back in
data_signif <- cbind(data_signif, Crime = data_scaled$Crime)

# Now that we have the coefficients that matter, let's re-run the linear model only on the coefficients we see above
model_lm2 <- lm(Crime ~ ., data_signif)

summary(model_lm2)
# We see from this model that our R-squared value dropped significantly. Very strange, and not what I expected.
# Let's try bringing in the two variables that were just above the .05 threshold (U@ and Po1).

data_updated <- cbind(data_signif, U2 = data_scaled$U2, Po1 = data_scaled$Po1)

model_lm3 <- lm(Crime ~., data_updated)

summary(model_lm3)
# Great! Our R-squared value went up to 76.6% and our adjusted R-squared went up to 73.1%. In particular, we notice
# that our R-squared value has gone down (it was originally 80.3%); however, our adjusted R-squared value has gone up
# (it was originally 70.1%). This makes sense because adjusted R-squared factors in the number of variables, and
# should get closer to 1 the less we overfit our model.

# Let's check out our coefficients with the new model
model_lm3$coefficients

# Now, let's predict on this fake dataset
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

# Bring all this new data into one data frame, transposed so it's all in one row and counts as one observation
new_data <- as.data.frame(t(c(M = M, So = So, Ed = Ed, Po1 = Po1, Po2 = Po2, 
                              LF = LF, M.F = M.F, Pop = Pop, NW = NW, U1 = U1,
                              U2 = U2, Wealth = Wealth, Ineq = Ineq, Prob = Prob, Time = Time)))


# Predict what the crime rate would now be
predict(model_lm3, new_data) 
# The linear regression model estimates that the Crime rate based on this data will be 35.79. This translates to
# about 36 crimes per 100,000 population. This seems extremely off from our original dataset, so we'll instead
# resort to our original, unscaled data which included every variable.


# Now let's take a look at the confidence interval for this new data
predict(model_lm3, new_data, interval = "confidence")
# The same linear regression model estimates 798 crimes per 100,000 population. It also estimates that 
# the lower limit for our data is -886.6 and the upper limit is 2482.3. This result makes more sense based on the
# fact that the median of our original crime is
median(data_crime$Crime)



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
# Linear Regression w/ PCA ---------------------------------------------
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

model_pca <- lm(Crime ~ ., data = pca_vals)

summary(model_pca)
# Notice that our adjusted R-squared value is 60.7%. This is significantly lower than what we saw in the previous homework (somewhere 
# around 75-80%), but we've significantly reduced the number of variables we're using.

### Transform our data back
# Get the original intercept from our model
(b0 <- model_pca$coefficients[1])

# Pull out model coefficients so we can make the Beta vector
(b_vals <- model_pca$coefficients[2:(pca_len + 1)])

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
# Now, let's predict on this fake dataset below.
# First we'll store all the new data we've been provided into a data frame.
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
(pred <- predict(pca1, newdata = new_pca))

# Thus we predict that the crime rate in the new state would be 1248 crimes per 100,000 population. This seems
# within the range of reason for our data, especially given that the mean of our crime data is:
mean(data_crime$Crime)
# and the range goes from 
range(data_crime$Crime)[1] 
# to
range(data_crime$Crime)[2]


#######################################################################
# Decision Trees ------------------------------------------------------
#######################################################################
# In this section, I'll find the best model available using:
#   - Regression Trees
#   - Random Forest

# Change column 2 to be a factor
data_scaled_facs <- as.data.frame(as.factor(unlist(crime_data[, 2])))

# bring column 2 back in and reorder our columns
data_scaled_fix <- data_scaled %>%
  cbind("S0" = data_scaled_facs) %>%
  select(Crime, everything()) %>%
  as_tibble()

head(data_scaled)
