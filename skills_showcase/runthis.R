# Code from the runthis chunks
library("tourr")
library("spinifex")
library("geozoo")

library(tidyverse)
library(palmerpenguins)
penguins <- penguins %>% filter(!is.na(bill_length_mm)) 

library(ochRe)
ggplot(penguins, 
       aes(x=flipper_length_mm, 
           y=body_mass_g, 
           colour=species,
           shape=species)) +
  geom_point(alpha=0.7, 
             size=2) +
  scale_colour_ochre(
    palette="nolan_ned") + 
  theme(aspect.ratio=1,
        legend.position="bottom")

# First tour
clrs <- ochre_pal(
  palette="nolan_ned")(3)
col <- clrs[
  as.numeric(
    penguins$species)]
penguins <- penguins %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm, 
         fl = flipper_length_mm, 
         bm = body_mass_g)
animate_xy(penguins[,3:6], 
           col=col, 
           axes="off", 
           fps=15)

# Some geometric shapes
sphere1 <- sphere.hollow(p=4)$points %>% as_tibble()
animate_xy(sphere1, axes="bottomleft")

sphere2 <- sphere.solid.random(p=4)$points %>% as_tibble()
animate_xy(sphere2, axes="bottomleft")

cube1 <- cube.face(p=4)$points %>% as_tibble()
animate_xy(cube1, axes="bottomleft")

cube2 <- cube.solid.random(p=4)$points %>% as_tibble()
animate_xy(cube2, axes="bottomleft")

torus <- torus(p = 4, n = 5000, radius=c(8, 4, 1))$points %>% as_tibble()
animate_xy(torus, axes="bottomleft")

mobius <- mobius()$points %>% as_tibble()
animate_xy(mobius, axes="bottomleft")

# Checking important variables
animate_xy(penguins[,3:6], 
           col=col, 
           axes="off", 
           fps=15)

ggplot(penguins, 
       aes(x=flipper_length_mm, 
           y=bill_depth_mm,
           colour=species,
           shape=species)) +
  geom_point(alpha=0.7, 
             size=2) +
  scale_colour_ochre(
    palette="nolan_ned") + 
  theme(aspect.ratio=1,
        legend.position="bottom")

ggplot(penguins, 
       aes(x=bill_length_mm, 
           y=body_mass_g,
           colour=species,
           shape=species)) +
  geom_point(alpha=0.7, 
             size=2) +
  scale_colour_ochre(
    palette="nolan_ned") + 
  theme(aspect.ratio=1,
        legend.position="bottom")

# Guided tour 
animate_xy(penguins[,3:6], grand_tour(),
           axes = "bottomleft", col=col)
animate_xy(penguins[,3:6], guided_tour(lda_pp(penguins$species)),
           axes = "bottomleft", col=col)
best_proj <- matrix(c(0.940, 0.058, -0.253, 0.767, 
                      -0.083, -0.393, -0.211, -0.504), ncol=2,
                    byrow=TRUE)

# Local tour
animate_xy(penguins[,3:6], local_tour(start=best_proj, 0.9),
           axes = "bottomleft", col=col)

# Other displays
animate_dist(penguins[,3:6], half_range=1.3)
animate_density2d(penguins[,3:6], col=col, axes="bottomleft")

