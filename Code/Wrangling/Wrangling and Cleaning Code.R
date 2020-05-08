################################################################## ####
#######################################################################

#####                     U.S. Crime Analysis                     #####

#######################################################################
#######################################################################
# In this script, I will analyze the U.S. Crime dataset included in the
# data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do so, including, but not limited to:

#   -- Outlier Detection
#   -- Outlier Detection
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
suppressMessages(library("outliers"))
suppressMessages(library("gridExtra"))
suppressMessages(library("reshape2"))
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files

# Bring in the data, delimited by a tab ("\t")
data_crime <- read.delim("Data/uscrime.txt", header = T, sep = "\t")

# Convert to a tibble, my preferred data structure
data_crime <- as_tibble(data_crime)


#######################################################################
# Outliers ------------------------------------------------------------
#######################################################################
# In this section, we'll first test to see whether there are any
# outliers in the last column (number of crimes per 100,000 people). 
# To do so, we'll use the grubbs.test function in the outliers package in R.

# First, let's look at a boxplot of the data
ggplot(data_crime, aes(y = data_crime$Crime)) +
  geom_boxplot(outlier.colour="slateblue3",
               outlier.size=2,
               color = "slateblue") +
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


# Now that we have a clear idea of what the outliers could look like, let's use the grubbs.test to
# better understand these outliers.
data_grubbs <- data_crime 
(grubbs1 <- grubbs.test(data_grubbs$Crime))
grubbs1$alternative 
# This indicates that the highest value 1993 would be an outlier if there were one in the set

(p_value <- grubbs1$p.value) 
# Since the p-value is above .05 (.079, to be exact), we cannot say with confidence that there
# is an outlier in the set. Let's move on.

