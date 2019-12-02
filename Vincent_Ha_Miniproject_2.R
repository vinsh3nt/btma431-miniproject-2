library(httr)
library(XML)
library(dplyr)

###############################
# Web Scraping from Wikipedia
###############################

# Scraping data from Wikipedia page
url <- 'https://en.wikipedia.org/wiki/Terrace_House:_Opening_New_Doors'
browseURL(url)  # View the page in a browser

# Get the html file for the above url
get_object <- GET(url)

# Parse the html file into an object that R can interpret
th_parsed <- htmlParse(get_object)

# I want to extract the main table with the information on the housemates here
# we can use the readHTMLTable() function to see all the tables on the page
page_tables <- readHTMLTable(th_parsed, stringsAsFactors = FALSE)

# The tables are stored as dataframes
View(page_tables)

# We want the table with 21 rows and 9 columns
View(page_tables[[2]])
th_table <- page_tables[[2]]

# Now that we have the table we want to clean it up a little bit
# First, we only care about a few of the columns so we'll subset the dataframe to include only these columns
th_table <- th_table[ , c(2,4,5,6,7,8,9)]

# The first two rows are the headers for the data, we'll just drop these rows and name the columns ourselves
th_table <- th_table[-c(1, 2), ]
colnames(th_table) <- c('name', 'nickname', 'occupation', 
                        'DOB', 'age', 'appearance_eps', 'appearance_count')
# Reset row names
rownames(th_table) <- seq(length=nrow(th_table))
View(th_table)

# Now that we have this table I want to add a field for gender
gender <- c('F','M','F','M','F','M','M','F','F','F','M','F','M','M','F','M','M','F','M')
th_table$gender <- gender

# I also want to add a column that shows the current Instagram follower count for the members
# to do this I will just mannually input the value from Instagram.
# The members social media info can be found at https://terracehousesocial.com/
followers.in.thousands <- c(305, 28.1, 105, 132, 282, 223, 100, 678, 81.8, 107, 146,
                            161, 65.6, 74.8, 144, 30.7, 49.3, 216, 69)
th_table$follows.thousands <- followers.in.thousands

# In case we want to do Chi Sq Test of Independence we would want to create bins for the follower count
# We can do this by looking at the quantiles and generating the bins (i.e. x values fall between x%)

# Now I want to clean up the occupation column a bit to reduce the number of categories we have to deal with
# there's only a few entries that need to be edited
th_table$occupation[c(1,9,18)] <- 'model'
th_table$occupation[3] <- 'entrepreneur'
th_table$occupation[c(4,5,14,17)] <- 'athlete'
th_table$occupation[c(7, 19)] <- 'musician' 
th_table$occupation[c(2, 10:13, 15)] <- 'in school'

# Everything is almost ready at this point, we just need to check the datatypes of the columns now
str(th_table)
# We can see that age, and appearance count are characters when we need them to be numbers
th_table[ , c(5,7)] <- sapply(th_table[ , c(5,7)], as.numeric)

# Now that everything is good to go we save this dataframe as a csv file in case we ever want to do visualizations
write.csv(th_table, 'th_ond_scraped.csv')

#############
# Analysis
#############

# Two main questions, do girls do better than guys in terms of followers?
# Models were super common house members during several seasons, but do models have more followers than the other careers?

# To answer the first question, we need to subset the data into a dataframe of just Female members and just Male members
th_fem <- th_table %>%
  filter(gender == 'F')

th_male <- th_table %>%
  filter(gender == 'M')

# Now we can run the appropriate hypothesis test, in this case we are doing a t-test
# Null: The avg followers are = between m and f
# Alt: The avg followers for males are < avg followers for females
test1 <- t.test(th_male$follows.thousands, th_fem$follows.thousands, alternative = 'less')
test1$p.value

# We see that we reject the null hypothesis in this case, Females do have more followers
# But what if we removed one of the entries from the female dataframe?
# One of the members, Seina, is a rare case. She is the only member in history to have been on the show more than once.
# This would have been her third time as a member of the cast, perhaps her prior popularity is skewing our result.
th_fem <- th_fem[-4, ]

# Now let's run the test again
test2 <- t.test(th_male$follows.thousands, th_fem$follows.thousands, alternative = 'less')
test2$p.value
# We see an even smaller p-value when ignoring our special case member
# This confirms that females that participated in this season have more followers on average than male members

# Now moving on to the second question, we need to again subset the data
# We want a dataframe with just the models, and a dataframe with everyone who is not a model
th_model <- th_table %>%
  filter(occupation == 'model')

th_rest <- th_table %>%
  filter(occupation != 'model')

# Now lets do the test
# Null: models and other careers have the same number of avg followers
# Alt: Models have more followers on average than everyone else
test3 <- t.test(th_model$follows.thousands, th_rest$follows.thousands, alternative = 'greater')
test3$p.value
# Based on the results of this test we fail to reject the null hypothesis
# Models, on average, do not have more followers than the other careers