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

