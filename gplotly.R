library(plotly)

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p <- qplot(carat, price, data=dsamp, colour=clarity)

p <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="ggplot2/intro-1")
chart_link


p <- ggplot(mtcars, aes(x = factor(gear), y = mpg, color = cyl)) +
  geom_boxplot() + 
  geom_jitter(size = 2) +
  ggtitle("geom_jitter: boxplot")

p <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p)
chart_link

#Sys.setenv("plotly_username"="ulala")
#Sys.setenv("plotly_api_key"="1jfjgHM76loBGmvwnI9w")
