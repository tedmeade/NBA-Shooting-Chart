library(tidyverse)
library(gapminder)
library(maps)
library(ggplot2)  
suppressPackageStartupMessages(library(tidyverse))
library(readr)
install.packages("imager")
library(grid)
library (jpeg)
library(RCurl)
library(png)
library(imager)

#Importing data

nba_shot <- read_csv("datacamp_workspace_export_2022-10-27 12_39_18.csv")
View(nba_shot)

sapply(nba_shot, class)

#Importing Court Image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

#Seth Curry

sethcurry <- nba_shot[1:147,]
View(sethcurry)

d <-ggplot(sethcurry, aes(x=X, y=Y)) + 
  annotation_custom(court, -30, 30, 0, 40) +
  geom_point(aes(colour = SCORE)) +
  xlim(-30, 30) +
  ylim(-0, 40) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", "Seth Curry", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, lineheight = 0.9, face = "bold"))

title = textGrob("2021 NBA Playoffs", gp = gpar(face = "bold", cex = 2))

#img <- readPNG(system.file("img", "chrispaulimg.png", package="png"))
img <- load.image('/Users/tysphone/Eco 490W/ECO490_project/sc.jpeg')
g <- rasterGrob(img)
size = unit(2, "cm")


heights = unit.c(size, unit(1, "npc") - size)
widths = unit.c(unit(1, "npc") - size, size)
lo = grid.layout(2, 2, widths = widths, heights = heights)

grid.show.layout(lo)

grid.newpage()
pushViewport(viewport(layout = lo))

pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1:2))
print(d, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2))
print(grid.draw(g), newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
print(grid.draw(title), newpage=FALSE)
popViewport()
popViewport()

g = grid.grab()

grid.newpage()
grid.draw(g)

#Chris Paul

chrispaul <- nba_shot[148:363,]

d <-ggplot(chrispaul, aes(x=X, y=Y)) + 
  annotation_custom(court, -30, 30, 0, 40) +
  geom_point(aes(colour = SCORE)) +
  xlim(-30, 30) +
  ylim(-0, 40) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", "Chris Paul", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, lineheight = 0.9, face = "bold"))

title = textGrob("2021 NBA Playoffs", gp = gpar(face = "bold", cex = 2))

#img <- readPNG(system.file("img", "chrispaulimg.png", package="png"))
img <- load.image('/Users/tysphone/Eco 490W/ECO490_project/chrispaulimg.png')
g <- rasterGrob(img)
size = unit(2, "cm")

getwd()
heights = unit.c(size, unit(1, "npc") - size)
widths = unit.c(unit(1, "npc") - size, size)
lo = grid.layout(2, 2, widths = widths, heights = heights)

grid.show.layout(lo)

grid.newpage()
pushViewport(viewport(layout = lo))

pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1:2))
print(d, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2))
print(grid.draw(g), newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
print(grid.draw(title), newpage=FALSE)
popViewport()
popViewport()

g = grid.grab()

grid.newpage()
grid.draw(g)

#Russell Westbrook
russellwest <- nba_shot[364:447,]
View(russellwest)

d <-ggplot(russellwest, aes(x=X, y=Y)) + 
  annotation_custom(court, -30, 30, 0, 40) +
  geom_point(aes(colour = SCORE)) +
  xlim(-30, 30) +
  ylim(-0, 40) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", "Russell Westbrook", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, lineheight = 0.9, face = "bold"))

title = textGrob("2021 NBA Playoffs", gp = gpar(face = "bold", cex = 2))

#img <- readPNG(system.file("img", "chrispaulimg.png", package="png"))
img <- load.image('/Users/tysphone/Eco 490W/ECO490_project/rw.png')
g <- rasterGrob(img)
size = unit(2, "cm")

getwd()
heights = unit.c(size, unit(1, "npc") - size)
widths = unit.c(unit(1, "npc") - size, size)
lo = grid.layout(2, 2, widths = widths, heights = heights)

grid.show.layout(lo)

grid.newpage()
pushViewport(viewport(layout = lo))

pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1:2))
print(d, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2))
print(grid.draw(g), newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
print(grid.draw(title), newpage=FALSE)
popViewport()
popViewport()

g = grid.grab()

grid.newpage()
grid.draw(g)

#Trae Young
traeyoung <- nba_shot[448:776,]
#View(traeyoung)

d <-ggplot(traeyoung, aes(x=X, y=Y)) + 
  annotation_custom(court, -30, 30, 0, 40) +
  geom_point(aes(colour = SCORE)) +
  xlim(-30, 30) +
  ylim(-0, 40) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", "Trae Young", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, lineheight = 0.9, face = "bold"))

title = textGrob("2021 NBA Playoffs", gp = gpar(face = "bold", cex = 2))

#img <- readPNG(system.file("img", "chrispaulimg.png", package="png"))
img <- load.image('/Users/tysphone/Eco 490W/ECO490_project/ty.jpeg')
g <- rasterGrob(img)
size = unit(2, "cm")

getwd()
heights = unit.c(size, unit(1, "npc") - size)
widths = unit.c(unit(1, "npc") - size, size)
lo = grid.layout(2, 2, widths = widths, heights = heights)

grid.show.layout(lo)

grid.newpage()
pushViewport(viewport(layout = lo))

pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1:2))
print(d, newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2))
print(grid.draw(g), newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
print(grid.draw(title), newpage=FALSE)
popViewport()
popViewport()

g = grid.grab()

grid.newpage()
grid.draw(g)

