library(tidyverse)

#Bar charts
qplot(mtcars$cyl, 
      geom = "bar", 
      fill = I("blue"), 
      color = I("red"),
      xlab = "Cylinders",
      ylab = "Number of Vehicles",
      main = "Cylinders in mtcars")

#Histograms
qplot(mtcars$hp,
      geom = "histogram",
      binwidth = 25,
      color = I("black"), #specifies color of line in between bins
      xlim = c(50, 350), #specify range of x-axis
      xlab = "Horsepower",
      ylab = "Number of cars",
      alpha = I(0), #specifies the fill color, in this case 0=none
      main = "Histogram")

#Pie charts, grouped bar charts, stacked bar charts
#convert cyl to factor since this groups the data
mtcars <- mtcars %>% mutate(cyl_factor = as.factor(cyl)) 
#Grouped bar chart
ggplot(mtcars, aes(cyl_factor, 
                   fill = cyl_factor)) +
  geom_bar(position = "dodge") #"dodge" creates a grouped bar chart

#Stacked bar chart
ggplot(mtcars, aes(" ", #no variable on x-axis, so need to make blank
                   fill = cyl_factor)) +
  geom_bar(position = "stack") #"stack" creates a stacked bar chart

#Pie chart
ggplot(mtcars, aes(" ", 
                   fill = cyl_factor)) +
  geom_bar(position = "stack")+
  coord_polar(theta = "y")+
  scale_fill_brewer(palette = "Dark2") #change fill color themes


#Scatterplots
qplot(mpg, wt, data = mtcars)
ggplot(mtcars,
       aes(mpg, wt))+
  geom_point(shape = 1) #change the shape of the points

#Can use different shapes and/or color to show a third variable, like cylinders
mtcars$cyl_factor <- factor(mtcars$cyl) #make the "cyl" variable a factor
ggplot(mtcars, 
       aes(mpg, wt,
           color = cyl_factor))+
  geom_point(shape = 19)+
  labs(color = "Cylinders")+
  xlab("Miles per gallon")+
  ylab("Weight")+
  ggtitle("Scatterplot")

#Line plots
#Need to convert data set from a multivariate time series object to a data frame
EuStockDF <- as.data.frame(EuStockMarkets)
EuStockDF$Date <- as.numeric(time(EuStockMarkets))
ggplot(EuStockDF, aes(Date, SMI))+
  geom_line()+
  labs(y = "Closing price of Switzerland (SMI) stock index")

#Add multiple lines to a graph
ggplot(EuStockDF, aes(x=Date))+
  geom_line(aes(y=DAX), color = "red")+
  geom_line(aes(y=SMI), color = "blue")+
  geom_line(aes(y=CAC), color = "green")+
  geom_line(aes(y=FTSE), color = "gold")+
  labs(y = "Closing price of stock indices")

#Another option
library(broom)
#Convert the columns to rows
tidy_stocks <- tidy(EuStockMarkets) %>%
  rename(Date = index,
         Stock_index = series,
         Price = value) # renaming columns from the defaults of index, series, and value
ggplot(tidy_stocks, aes(Date, Price))+
  geom_line(aes(color = Stock_index))


#Boxplots
#Example using randomly generated data
set.seed(1234, sample.kind = "Rounding")
set_a <- rnorm(200, mean = 1, sd = 2)
set_b <- rnorm(200, mean = 0, sd = 1)
df <- data.frame(label = factor(rep(c("A", "B"),
                                    each = 200)),
                 value = c(set_a, set_b))

install.packages("plotly")
library(plotly)

ggplot(df, aes(label, value))+
  geom_boxplot()
ggplotly() #plotly gives a little more style to the boxplot

qplot(factor(cyl), mpg,
      data=mtcars,
      geom = "boxplot")

ggplot(mtcars, aes(factor(cyl), mpg))+
  geom_boxplot()


#Changing labels & adding subtitles
ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(color=factor(cyl)))+
  labs(x = "Weight (1000 lbs)",
       y = "Miles/(US)gallon",
       color = "Cylinders", #changes teh legend title
       title = "Mileage by weight and cylinder",
       subtitle = "Source: 1974 Motor Trend US magazine")

#Add text to data points
ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(color=factor(cyl)))+
  geom_text(aes(label=rownames(mtcars)),
            check_overlap = TRUE, #makes sure labels aren't overlapping the points
            hjust = "inward") #makes sure the text is in the plot

#Custom annotations
ggplot(mtcars, aes(x = mpg))+
  geom_histogram(bins=10)+
  labs(x = "Miles/(US) gallon",
       y = "Count",
       title = "Distribution of Miles/Gallon")+
  geom_vline(aes(xintercept = 19.2), color = "red")+ #adds vertical line at 19.2 (median)
  annotate(geom = "text",
           label = "Median = 19.2",
           x = 21,
           y= 7, #coordinates for where the text will go
           hjust = 0,
           color = "red")

#Faceting
ggplot(mtcars, aes(x=mpg))+
  geom_histogram()+
  facet_wrap(~cyl) #create different plots for different categories of cylinders

#Themes
ggplot(mtcars, aes(cyl))+
  geom_bar()+
  theme_gray() #default
#theme_bw - what background
#theme_minimal - no background annotation
#theme_classic - x and y axis, but no gridlines
#theme_void - empty theme
#theme_linedraw - black lines of various widths
#theme_light - grey lines of various widths
#theme_dark - dark background

#example
ggplot(mtcars, aes(cyl))+
  geom_bar()+
  ggtitle("Number of cylinders")+
  theme(plot.background = element_rect(color = "green", fill = "gray"),
        plot.title = element_text(face = "bold", color = "blue")) #make title bold and blue

#ggthemes has many more options for graph themes
install.packages("ggthemes")
library(ggthemes)
#store a base plot example to make it easy to try different themes
p <- ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(color = factor(cyl)))+
  labs(x = "Weight (1000 lbs)", y = "Miles/(US) gallon", color = "Cylinders")
p

#Economist magazine theme
p + theme_economist() + scale_color_economist()

#Color blind safe & clean theme
p + theme_clean() + scale_color_colorblind()

p + theme_solarized() + scale_color_colorblind()

p + theme_fivethirtyeight()

p + theme_clean()

p + theme_gdocs()

p+ theme_get()


#MAPS
install.packages("leaflet")
library(leaflet)
map <- leaflet() %>% #create an object that represents an empty world map
  addTiles() %>% #publish a tile layer on the map
  addMarkers(lng = -73.9851, lat = 40.7589, popup = "Times Square") #add a marker (e.g. Times Square) with addMarkers function, include longitude and latitude
map

leaflet() %>% addProviderTiles("Stamen.Watercolor") %>% #addProviderTiles can add different color themes and styles
  addMarkers(lng = 2.2945, lat = 48.8584, popup = "Eiffel Tower")

#Using "quakes" data set to create map
leaflet() %>% addTiles() %>%
  addCircleMarkers(lng = quakes$long, lat = quakes$lat)
#Group markers into clusters to add clarity
leaflet(quakes) %>% addTiles() %>%
  addCircleMarkers(clusterOptions = markerClusterOptions())

#To add a rectangle to map
leaflet(quakes) %>% addTiles() %>%
  addMarkers(lng = 86.92, lat = 27.99, popup = "Mount Everest") %>%
  addRectangles(86.9, 27.95, 87, 28.05) #coordinates of two points that serve as delimiters of rectangle


#Using the mtcars dataset, create a scatter plot of the gross horsepower on the x-axis and the 1/4 mile time on the y-axis. Add labels and a title to the plot. Also, add a line separating horsepowers greater than 200.
#As a bonus, add a text annotation next to the point that has the slowest 1/4 mile time value.

i_max <- which.max(mtcars$qsec) #finds the highest/slowest time in 1/4 mile

mtcars %>% ggplot(aes(hp, qsec))+
  geom_point()+
  ggtitle("Horsepower vs. 1/4 mile time")+
  xlab("Horsepower")+
  ylab("1/4 mile time in seconds")+
  geom_vline(xintercept = 200, color = "red")+
  annotate(geom = "text",
           label = "slowest",
           x = mtcars$hp[i_max] + 5,
           y = mtcars$qsec[i_max],
           hjust = 0,
           color = "red")

#Create a bar chart of number of cylinders and facet by transmission. Add labels and a title.   
ggplot(mtcars, aes(cyl))+
  geom_bar()+
  facet_wrap(~am)+
  ggtitle("Number of cyliders in manual and automatic transmissions")+
  ylab("Count of cars")

#Use mtcars to create a bar chart of the number of forward gears. Remember to set fill as gears. Chose a built in theme and a color palette different from the examples.
mtcars$gear_factor <- factor(mtcars$gear)
ggplot(mtcars, aes(gear_factor, fill = gear_factor))+
  geom_bar()+
  theme_economist()+
  scale_fill_brewer(palette = "Set3")

#Plot a boxplot with "mpg" on the y axis. Change the background to any color, choose one here. Also, remove the x text ("axis.text.x").
ggplot(mtcars, aes(factor(cyl), mpg))+
  geom_boxplot()+
  theme(plot.background = element_rect(fill = "azure1"),
        axis.text.x = element_blank())
  
#Using the "quakes" dataset, add circles and give them color to visualize magnitude (mag). Also, add a legend for the magnitudes. Try following the first example in the legend section in the documentation.
pal <- colorNumeric(palette = "Reds", domain = quakes$mag)
leaflet(quakes) %>% addTiles() %>%
  addCircles(lng = quakes$long, lat = quakes$lat, color = ~pal(mag)) %>%
  addLegend("bottomright", pal=pal, values = ~mag, title = "Magnitude")

#DASHBOARDS
install.packages("shiny")
library(shiny)
runExample("01_hello")

ui <- fluidPage() #define UI / empty layout, it is preferred to write this code in separate R files
server <- function(input, output) {} #define logic for the server, it is preferred to write this code in separate R files
shinyApp(ui=ui, server=server) #run the app, don't put the runApp() function inside the Shiny code


#How to display/deploy a Shiny dashboard
install.packages("rsconnect") #platform for hosting Shiny applications
library(rsconnect)
#create account by going to www.shinyapps.io
#authorize account
rsconnect::setAccountInfo(name='martinwera',
                          token='D9A11B7A817E68825CCC924EE4ABD092',
                          secret='vqnrhi1I06W1rwmM3lAtKldghSYWgw57T2gC8VvA')
#deploy
library(rsconnect)
rsconnect::deployApp('path/to/your/app')



