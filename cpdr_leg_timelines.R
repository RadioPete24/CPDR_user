#####CPDR Timeline figure generation######


#Code for generating timeline
Events <- c("California State Assembly Bill \n(AB) 2248\n", "California State Senate Bill \n(SB) 97\n", "California State Health and Safety Code (HSC) \nRichard Paul Hemann \nParkinson's Disease Program\n", "Mandated Reporting Begins\n", "California State Assembly Bill \n(AB) 715\n", "Sunset Date of CPDR\n")
Event_Dates <- c("2004-09-30", "2017-07-10", "2018-01-01", "2018-07-01", "2019-10-12", "2021-01-01")



cpdr_timeline <- data.frame(Events, Event_Dates, stringsAsFactors = FALSE)
cpdr_timeline$Event_Dates <- as.Date(cpdr_timeline$Event_Dates)
# cpdr_timeline$Event_Dates <- strptime(cpdr_timeline$Event_Dates,
#                                      , format = "%Y-%m-%d"
#                                      )
# cpdr_timeline$Event_Dates <- paste(months(ymd(cpdr_timeline$Event_Dates)), format(cpdr_timeline$Event_Dates, "%d, %Y"))
rm(Events, Event_Dates)

install.packages("timelinesS")
devtools::install_github("daheelee/timelinesS", dependencies = TRUE)
require(timelineS)
#using .rda data format
# timelineS(cpdr_timeline
#           , main = "Timeline of California Parkinson's Disease Registry Legislation"
#           #, label.direction = "up"
#           , label.length = c(0.5, 0.3, 0.2, 0.1)
#           , label.position = 3
#           #, line.color = "blue"
#           , label.color = "blue"
#           , point.color = "blue"
#           , pch = "-")
timelineS(cpdr_timeline
          , main = "Timeline of California Parkinson's Disease Registry Legislation"
          , label.direction = "up"
          , label.length = c(0.3, 0.45, 0.7, 0.2)
          , label.position = 3
          , label.color = "#59BEE3"
          # , point.color = "#59BEE3"
          , scale.orient = 3
)
timelineS(cpdr_timeline[-1,]
          , main = "Timeline of California Parkinson's Disease Registry Legislation"
          , buffer.days = 150
          , label.direction = "up"
          , label.length = c(0.55, 0.9, 0.45, 0.85, 0.55)
          , label.position = 3
          , label.color = "#59BEE3"
          , label.cex = 1.05
          # , point.color = "#59BEE3"
          , scale.orient = 3
)

#####Another plot option #####
#####CPDR Timeline figure generation######


#Code for generating timeline
Events <- c("California State Assembly Bill \n(AB) 2248\n", "California State Senate Bill \n(SB) 97\n", "California State Health and Safety Code (HSC) \nRichard Paul Hemann \nParkinson's Disease Program\n", "California State Assembly Bill \n(AB) 715\n")
Event_Dates <- c("2004-09-30", "2017-07-10", "2018-01-01", "2019-10-12")

cpdr_timeline <- data.frame(Events, Event_Dates, stringsAsFactors = FALSE)
cpdr_timeline$Event_Dates <- as.Date(cpdr_timeline$Event_Dates)
# cpdr_timeline$Event_Dates <- strptime(cpdr_timeline$Event_Dates,
#                                      , format = "%Y-%m-%d"
#                                      )
# cpdr_timeline$Event_Dates <- paste(months(ymd(cpdr_timeline$Event_Dates)), format(cpdr_timeline$Event_Dates, "%d, %Y"))
rm(Events, Event_Dates)

install.packages("timelinesS")
devtools::install_github("daheelee/timelinesS")
require(timelineS)
#using .rda data format
timelineS(cpdr_timeline
          , main = "Timeline of California Parkinson's Disease Registry Legislation"
          , label.direction = "up"
          , label.length = c(0.3, 0.4, 0.2, 0.1)
          , label.position = 3
          #, line.color = "blue"
          , label.color = "blue"
          , point.color = "blue"
          , pch = "-")
timelineS(cpdr_timeline
          , main = "Timeline of California Parkinson's Disease Registry Legislation"
          , label.direction = "up"
          , label.length = c(0.3, 0.4, 0.7, 0.3)
          , label.position = 3
          , label.color = "#59BEE3"
          # , point.color = "#59BEE3"
          , scale.orient = 3
)


###Another option for timeline format ####

library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)

#Create data to plot
data <- tribble( ~start_date, ~event, ~displ,
                 ymd(20040930), "California State Assembly Bill \n(AB) 2248\n", 0.5,
                 ymd(20170710), "California State Senate Bill \n(SB) 97\n", 0.4,
                 ymd(20180101), "California State Health and Safety Code (HSC) \nRichard Paul Hemann \nParkinson's Disease Program\n", 0.5,
                 ymd(20191012), "California State Assembly Bill \n(AB) 715\n", 0.3,)


#Function to shift x-axis to 0 adapted from link shown above

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}


#Conditionally set whether text will be above or below the point
vjust = ifelse(data$displ > 0, -1, 1.5)

#plot
p1 <- data %>% 
  ggplot(aes(start_date, displ)) +
  geom_lollipop(point.size = 1) +
  geom_text(aes(x = start_date, y = displ, label = event), data = data,
            hjust = 0, vjust = vjust, size = 2.5) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 8)) +
  expand_limits(x = c(ymd(20040601), ymd(20200301)), y = 1.2) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9))

#and run the function from above
timeline <- shift_axis(p1, ymd(20040601), ymd(20200301))

return(timeline)


##edit of functions
function (df, main = NA, xlab = NA, buffer.days = 600, line.width = 5, 
          line.color = "gray44", scale = "year", scale.format = "%Y", 
          scale.font = 2, scale.orient = 1, scale.above = FALSE, scale.cex = 1, 
          scale.tickwidth = 2, labels = paste(df[[1]], df[[2]]), label.direction = "downup", 
          label.length = c(0.5, 0.5, 0.8, 0.8), label.position = c(1, 
                                                                   3), label.color = "gray44", label.cex = 0.8, label.font = 1, 
          label.angle = 0, pch = 20, point.cex = 1, point.color = "gray44") 
{
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }
  df <- df[rowSums(is.na(df)) == 0, ]
  event.names <- df[[1]]
  event.dates <- df[[2]]
  if (label.direction == "downup") {
    d = c(-1, 1)
    h = 0
  }
  else if (label.direction == "updown") {
    d = c(1, -1)
    h = 0
  }
  else if (label.direction == "up") {
    d = 1
    h = -0.7
  }
  else if (label.direction == "down") {
    d = -1
    h = 0.7
  }
  else {
    d = c(-1, 1)
    h = 0
    print("incorrect label.direction, plot used default")
  }
  range.events <- range(min(event.dates) - buffer.days, max(event.dates) + 
                          buffer.days)
  r1 <- range.events[1]
  r2 <- range.events[2]
  plot(NA, ylim = c(-1, 1), xlim = range.events, ann = FALSE, 
       axes = FALSE)
  title(main = main, xlab = xlab
        #, cex.main = 1.1
        )
  points <- rep_len(d * label.length, length.out = nrow(df))
  events <- rep_len(label.position, length.out = nrow(df))
  segments(event.dates, h, event.dates, points + h, col = label.color)
  axis.Date(ifelse(scale.above == TRUE, 3, 1), at = seq(as.Date(paste0(lubridate::year(r1), 
                                                                       "-", lubridate::month(r1), "-", 1)), as.Date(paste0(lubridate::year(r2), 
                                                                                                                           "-", lubridate::month(r2) + 1, "-", 1)), by = scale), 
            format = scale.format, cex.axis = scale.cex, pos = h, 
            lwd.tick = scale.tickwidth, col = line.color, font = scale.font, 
            las = scale.orient)
  abline(h = h, lwd = line.width, col = line.color)
  points(x = event.dates, y = points + h, pch = pch, cex = point.cex, 
         col = point.color)
  text(x = event.dates, y = points + h, labels = labels, cex = label.cex, 
       pos = events, font = label.font, srt = label.angle)
}


