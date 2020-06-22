# Prepare data
library(tidyverse)
library(tourr)
library(geozoo)
library(RColorBrewer)
library(randomForest)

quartz() # or X11()

olive <- read_csv("http://www.ggobi.org/book/data/olive.csv") %>%
  rename(name=X1)
olive <- olive %>%
  filter(region == 1) %>%
  mutate(area = factor(area))

# tour data
pal <- brewer.pal(4, "Dark2")
col <- pal[olive$area]
# drop eicosenoic, all low for south
animate_xy(olive[,4:10], axes="bottomleft", col=col) 
# Drop Sicily
animate_xy(olive[olive$area!=4,4:10], axes="bottomleft", col=col[olive$area!=4]) 

# Fit model
olive_rf <- randomForest(area~., data=olive[,-c(1, 2, 11)], importance=TRUE, proximity=TRUE)
olive_rf

# Extract votes
vt <- data.frame(olive_rf$votes)
vt$area <- olive$area
proj <- t(geozoo::f_helmert(4)[-1,])
vtp <- as.matrix(vt[,-5])%*%proj
vtp <- data.frame(vtp, area=vt$area)
pal <- brewer.pal(4, "Dark2")
col <- pal[as.numeric(vtp[, 4])]
animate_xy(vtp[,1:3], col=col, axes = "bottomleft")

# Add simplex
simp <- simplex(p=3)
sp <- data.frame(simp$points)
colnames(sp) <- c("x1", "x2", "x3")
colnames(vtp) <- c("x1", "x2", "x3")
vtp_s <- bind_rows(sp, vtp[,1:3])
animate_xy(vtp_s, col=col, axes = "bottomleft", edges=as.matrix(simp$edges), center=TRUE)

