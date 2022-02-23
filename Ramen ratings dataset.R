##################################
# Import dataset
##################################

# Import dataset
ramen <- read.csv("ramen-ratings-dataset.csv", header=TRUE, sep= ",")

# Structure of the dataset
str(ramen)

# Read first few rows of ramen ratings data
head(ramen)

#################################
# Clean the data
#################################

# Change the data type from charcter to numeric
ramen$Stars = as.numeric(paste(ramen$Stars))

# Change the data type from character to factor
ramen$Style <- factor(ramen$Style)

# See the levels in Style (before)
levels(ramen$Style)

# Dealing with "" in Style, to assign with "NA"
ramen$Style[ramen$Style == ""] <- "NA"

# See the levels in Style (after)
levels(ramen$Style)

# Remove NA in Style, and other features, but exclude Top.Ten
ramen <- na.omit(ramen, c("Review..", "Brand", "Variety", "Style", "Country","Stars")) 

# Drop unused level - NA
ramen$Style <- droplevels(ramen$Style)

# See the levels in Style after dropping NA
levels(ramen$Style)

# Structure of the dataset after removing records with missing data (but exclude Top.Ten)
str(ramen)

# Change the data type from character to factor
ramen$Top.Ten <- factor(ramen$Top.Ten)

# Dealing with "\n" in Top.Ten, to assign with ""
ramen$Top.Ten[ramen$Top.Ten == "\n"] <- "NA"

# Dealing with "" in Top.Ten, to assign with "NA"
ramen$Top.Ten[ramen$Top.Ten == ""] <- "NA"

# Read the first 6 observations
head(ramen)

# Check levels before dropping the levels "" and "\n"
levels(ramen$Top.Ten)

# Drop "" and and "\n" levels
ramen$Top.Ten <- droplevels(ramen$Top.Ten)

# Check levels again
levels(ramen$Top.Ten)

# Structure of the dataset after dropping the level
str(ramen)

# Replace NA with "None", and include "None" into factor levels
ramen[] <- lapply(ramen, function(x){
  # check if you have a factor first:
  if(!is.factor(x)) return(x)
  # otherwise include NAs into factor levels and change factor levels:
  x <- factor(x, exclude=NULL)
  levels(x)[is.na(levels(x))] <- "None"
  return(x)
})

# Check levels again
levels(ramen$Top.Ten)

# Structure of the dataset after including "None" into factor level
str(ramen)

# View first 6 observations after cleaning Top.Ten
head(ramen)

#################################
# Understanding/Visualizing the data
#################################

# Display summary statistics
summary(ramen)

# Histogram of Stars
hist(ramen$Stars)
# Count of Stars in descending order
table(ramen$Stars) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# Produces plots based on the provided data
plot(ramen$Brand)
# Count of Brands in descending order
table(ramen$Brand) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# Extract Nissin ramen details
nissin_data = subset(ramen, Brand == "Nissin")
nissin_data

# To view the most country's ramen being reviewed
plot(ramen$Country)
# Count of Country in descending order
table(ramen$Country) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# To find the brands under USA
usa_data = subset(ramen, Country == "USA")
usa_data
USA_df <- data.frame(table(usa_data$Brand))
USA_df[rev(order(USA_df$Freq)),]

# To find the most common ramen style being reviewed
plot(ramen$Style)

# Count of Style in descending order
table(ramen$Style) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# To view what ramen styles are with the best reviews (above average scale)
table(ramen$Stars, ramen$Style) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# Seeing trends between 2 categorical data
plot(ramen$Style, ramen$Stars)

# To view what ramen styles are with the best reviews (above average scale)
plot(ramen$Country, ramen$Stars) 

# Extract box ramen details
box_data = subset(ramen, Style == "Box")
box_data

# To view what ramen styles in Top Ten over 2012 - 2016
table(ramen$Style, ramen$Top.Ten) 

# Extract Top Ten ramen details
topten_data = subset(ramen, Top.Ten != "None")
print("No of Top Ten ramen from 2012 to 2016: ")
nrow(topten_data)
topten_data

#To view which Country has the most Top Ten ramens
df <- data.frame(table(topten_data$Country))
df[rev(order(df$Freq)),]

# save the cleaned dataset as csv
write.table(ramen, file = "ramen_cleaned_R.csv",
            sep = "\t", row.names = F)
