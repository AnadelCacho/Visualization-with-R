
################ COURSE: VISUALIZATION WITH R #####################

## Libraries
library(ggplot2)



# Data for this course
mtcars<-mtcars


##### BAR CHARTS #####
### qplot function
qplot(
  mtcars$cyl,
  geom = "bar",
  fill = I("blue"), # The colour of the bars
  colour = I("red"), # The colour of the bars' outlines
  xlab = "Cylinders", # x axis label
  ylab = "Number of vehicles", # y axis label
  main = "Cylinders in mtcars" # title of the bar chart
) # help() function to see all the colours


##### HISTOGRAMS #####
qplot(
  mtcars$hp,
  geom = "histogram",
  binwidth = 20, # Improve the smoothness of the histogram
  colour = I("black"), #Colour of the borders
  xlim = c(50,350), # X axis limit 
  xlab = "Horsepower", # x axis label
  ylab = "Number of Cars", # y axis label
  alpha = I(0), # Delete the colours of the bars
  main = "Histogram" # Title
)



##### PIE CHARTS #####

# To make a pie chart, you first need to create a stacked bar plot.
bar_plot <- ggplot(
  mtcars, # Data Base
  aes(x=1, y=sort(mtcars$carb), # fix the axis
      fill = sort(mtcars$carb))) +  
  geom_bar(stat = "identity") # Stack the bars the way we want them


print(bar_plot)


## In order to form the pie chart, we need to add the 'coord_polar' method 
#with the argument 'theta = y'
bar_plot2 <- bar_plot + coord_polar(theta = "y")
print(bar_plot2)

## We can edit the theme of the pie chart by using the 'axis' class
bar_plot3 <- bar_plot2 + theme( # This "theme" will remove the text
  axis.line = element_blank(), 
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(), # To remove the gray border of the chart
) + labs(y = "Carburetos") # Y label

print(bar_plot3)



##### SCATTER PLOTS #####
ggplot(
  mtcars,
  aes(x = mpg, y = wt)
) + geom_point(shape = 1) # change the shape of the dots within the graph


# Othe way, for black solid circles
ggplot(
  mtcars,
  aes(x = mpg, y = wt)
) + geom_point(shape = 19) 


# The automobiles can be categorized by the number of cylinders, 
# so we're going to use the shape of each point to represent this.
mtcars$cylFactor <- factor(mtcars$cyl)
mtcars$cylFactor
ggplot(
  mtcars,
  aes(x = mpg, y = wt, 
      shape = cylFactor)
) + geom_point()

# Putting the third variable as a colour
ggplot(
  mtcars,
  aes(x = mpg, y = wt, color = cyl)
) + geom_point(shape = 19)

## or 
ggplot(
  mtcars,
  aes(x = mpg, y = wt, color = cylFactor)
) + geom_point(shape = 19) +
  labs(colour = "Cylinders") # To change the legend's label

## More descriptive:
ggplot(
  mtcars,
  aes(x = mpg, y = wt, color = cylFactor)) +
  geom_point(shape = 10) + 
  xlab("Miles per Gallon") +
  ylab("Weight") +
  labs(colour = "Cylinders") +
  ggtitle("Scatterplot")
  

##### LINE PLOTS AND REGRESSION ####
## New data
EuStockDF <- as.data.frame(EuStockMarkets)
head(EuStockDF) # show the first 6 rows

ggplot(EuStockDF, 
       aes(x = c(1:nrow(EuStockDF)), y = DAX)) +
  geom_line() + # The 'geom_line' method will connect each of the observations and draw the graph
  labs(x = "Stocks") # x label

# Changing the size of the geom_line
ggplot(EuStockDF, 
       aes(x = c(1:nrow(EuStockDF)), y = DAX)) +
  geom_line(size = 1.5) + # The 'geom_line' method will connect each of the observations and draw the graph
  labs(x = "Stocks") # x label

# Changing the colour
ggplot(EuStockDF, 
       aes(x = c(1:nrow(EuStockDF)), y = DAX)) +
  geom_line(size = 1.5, colour = "red") + # The 'geom_line' method will connect each of the observations and draw the graph
  labs(x = "Time", y = "Stocks") #Labels

### To display multiple figures in the same line plot, we simply need to add additional 'geom_line' methods
dax_smi_plot <- ggplot() +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = DAX),
            size = 1.5, colour = "light blue") +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = SMI),
            size = 1.5, colour = "pink") +
  labs(x = "Time", y = "Stocks")

print(dax_smi_plot)
  
# To display multiple variables
all_stocks <- ggplot() +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = DAX),
            size = 1.5, colour = "light blue") +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = SMI),
            size = 1.5, colour = "pink") +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = CAC),
            size = 1.5, colour = "purple") +
  geom_line(data = EuStockDF, aes(x = c(1:nrow(EuStockDF)), y = FTSE),
            size = 1.5, colour = "green") +
  labs(x = "Time", y = "Stocks")

print(all_stocks)


# Add some methods:
legend_stocks <- all_stocks +
  xlab("Days") +
  ylab("Price") +
  ggtitle("EU Stocks")

print(legend_stocks)
  
## MTCARS
#### Linear regression is a mathematical approach for fitting a linear function to a set of data
ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point(shape=19) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# se = TRUE -> confidence interval appear (default = true)
ggplot(mtcars, aes(x = mpg, y = wt, color = cylFactor)) +
  geom_point(shape=19) +
  geom_smooth(method = "lm", se = TRUE, color = "red")

# Add details:
ggplot(mtcars, aes(x = mpg, y = wt, color = cylFactor)) +
  geom_point(shape=19) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  xlab("Miles per Gallon") +
  ylab("Weight") +
  labs(colour = "Cylinders") +
  ggtitle("Linear Regression")


### Gaussian model
ggplot(mtcars, aes(x = mpg, y = wt, color = cylFactor)) +
  geom_point(shape=19) +
  geom_smooth(method = "auto", se = TRUE, color = "red") +
  xlab("Miles per Gallon") +
  ylab("Weight") +
  labs(colour = "Cylinders") +
  ggtitle("Gaussian Regression")




##### WORD CLOUDS #####
#install.packages("tm") # this package will transform the text into a format that's handled by R
#install.packages("wordcloud") # To create the visualization
library(tm)
library(wordcloud)


# Crear un corpus a partir de un archivo de texto específico
#file_path <- "/Users/anadelcacho/Desktop/Assembler Master/Courses_extra/Visualization with R/External Video-en.txt"
#speech <- Corpus(VectorSource(file_path))

speech2 <-Corpus(VectorSource(text))
inspect(speech2)



## Data cleaning

# First, to convert the text to lower case:
speech <- tm_map(speech2, content_transformer(tolower))

# Second, remove all numbers:
speech <- tm_map(speech, removeNumbers)

# Third, remove common stop words like "the" and "we":
speech <- tm_map(speech, removeWords, stopwords("english"))
speech <- tm_map(speech, removeWords, c("squirrelled", "floccinaucinihilipification")) # You can remove your own words too

# Fourth, to remove punctuation
speech <- tm_map(speech, removePunctuation)

# Fifth, to remove unnecessary whitespaces
speech <- tm_map(speech, stripWhitespace)



## Term Document Matrix (table that contains the frequency of the words)

# Crete a Term Document Matrix
dtm <- TermDocumentMatrix(speech)

# Matrix transformation
m <- as.matrix(dtm)

# Sort it to show the most frequent words
v <- sort(rowSums(m), decreasing = TRUE)

# Transform to a data frame
d <- data.frame(word = names(v), freq = v)
head(d, 10)




## Simple Word Cloud

# Create our word cloud
wordcloud(words = d$word, freq = d$freq)
wordcloud(words = d$word, freq = d$freq, min.freq = 1)





##### RADAR CHARTS #####
#install.packages("ggforce") # This is the equivalence of ggradar for this version of R (use the function "geom_polygon() to track a radar-graph")
library(ggforce)
library(dplyr) # to provide the pipe operation (A pipe operation essentially takes the output from one function and provides it to another)
library(scales)

# Crear el gráfico de radar con el dataframe mtcars
ggplot(mtcars) +
  geom_polygon(aes(x = factor(cyl), y = disp, group = 1), fill = "blue", alpha = 0.5) +
  geom_polygon(aes(x = factor(cyl), y = hp, group = 1), fill = "red", alpha = 0.5) +
  geom_polygon(aes(x = factor(cyl), y = mpg, group = 1), fill = "yellow", alpha = 0.5) +
  coord_polar() +
  theme_minimal()


##### WAFFLE CHARTS ######
#install.packages("waffle")
library(waffle)

# Create data
expenses <- c(`Health ($43,212) `= 43212,
              `Education ($113,412) `= 113412,
              `Transportation ($20,231) `= 20231,
              `Entertainment ($28,145) `= 28145)

expenses

# Create Waffle Chart:
waffle(expenses/1235, rows = 5, size = 0.3,
       colors = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"),
       title = "Imaginary Household Expenses each Year",
       xlab = "1 square = $934")





##### BOX PLOTS #####

# Example Data Frame
set.seed(1234)

set_a <- rnorm(200, mean = 1, sd = 2)
set_b <- rnorm(200, mean =  0, sd = 1)

# Create a data frame
df <- data.frame(label = factor(rep(c("A", "B"),
                                    each = 200)),
                 value = c(set_a, set_b))

head(df)
tail(df)

# Additional packages:
#install.packages("plotly")
library(plotly) # Plotly provides online graphing, analytics, and statistics tools for individuals and collaboration, as well as a scientific graphing library.

ggplot(df, aes(x = label, y = value)) + geom_boxplot()
ggplotly()


# lets practice with mtcars:
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")

cars <- ggplot(mtcars, aes(factor(cyl), mpg))
cars + geom_boxplot()





##### MAPS #####

# install.packages("leaflet")
library(leaflet) # Leaflet is a powerful library capable of producing complex visuals, like this street map of Times Square

map <- leaflet()
map <- leaflet() %>% addTiles() # This allows us to zoom in and see the countries and cities in detail

# Add a specific location
map_ts <- leaflet() %>% addTiles() %>%
  addMarkers(lng = -73.9851, lat = 40.7589,
             popup = "Times Square") # To add a caption to a specific location, you can use the 'popup' argument

map_te <- leaflet() %>% addTiles() %>%
  addMarkers(lng = 2.2945, lat = 48.8584,
             popup = "Eiffel Tower") # To add a caption to a specific location, you can use the 'popup' argument

map_ts
map_te


# Add colour
map_te <- leaflet() %>% addProviderTiles("Stamen.Watercolor") %>%
  addMarkers(lng = 2.2945, lat = 48.8584,
             popup = "Eiffel Tower") # To add a caption to a specific location, you can use the 'popup' argument


# Other kind of map: SECOND EXAMPLE
quakes <- quakes
head(quakes)

map_q <- leaflet(quakes) %>% addTiles() %>%
  addCircleMarkers(lng = quakes$long, lat = quakes$lat)

map_q

## To improve the clarity:
map7 <- leaflet(quakes) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

map7

#install.packages("IRdisplay")
library(IRdisplay)
#install.packages("htmlwidgets")
library(htmlwidgets)

#saveWidget(map7, file = "map7.html", selfcontained = F)
display_html(paste("<iframe src = ' ", "map7.html", "'width='100%', height='300'", "/>"))
