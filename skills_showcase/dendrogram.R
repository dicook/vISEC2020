# Show a dendrogram

# Function to generate edges of dendrogram
hierfly <- function(data, h=NULL, metric="euclidean", method="ward.D2", scale=TRUE) {
  if (scale) data <- rescaler(data)
  id <- 1:nrow(data)
  cat <- sapply(data, is.factor)
  if (is.null(h))
    h <- hclust(dist(data[,!cat], metric), method)
  #h <- hclust(dist(data, metric), method)
  data_hc <- data
  
  data_hc$ORDER <- order(h$order)
  data_hc$HEIGHT <- 0
  data_hc$LEVEL <- 0
  data_hc$POINTS <- 1
  
  #newr_df <- NULL
  for (i in 1:nrow(h$merge)) {
    newr <- combinerows(data_hc[as.character(-h$merge[i,]),], cat)
    #newr <- combinerows(data_hc[as.character(-h$merge[i,]),], rownames(data))
    #newr$id <- nrow(data_hc) + i
    newr$HEIGHT <- h$height[i]
    newr$LEVEL <- i
    rownames(newr) <- as.character(-i)
    
    data_hc <- rbind(data_hc, newr)
  }
  data_hc$id <- 1:nrow(data_hc)
  data_hc$node <- (as.numeric(rownames(data_hc)) < 0) + 0
  
  vars <- c("ORDER", "POINTS")
  
  leaves <- subset(data_hc, node == 0)
  nodes <- subset(data_hc, node == 1)
  
  # < 0 = observation, > 0 = cluster
  edges <- h$merge
  edges[edges > 0] <- edges[edges > 0] + nrow(leaves)
  edges[edges < 0] <- abs(edges[edges < 0])
  
  return(list(data=data_hc, edges=edges))
}

# Utility functions
combinerows <- function(df, cat) {
  same <- function(x) if (length(unique(x)) == 1) x[1] else x[2]
  points <- df$POINTS
  
  cont <- as.data.frame(lapply(df[, !cat, drop=FALSE] * points,
                               sum)) / sum(points)
  cat <- as.data.frame(lapply(df[, cat, drop=FALSE], same))
  
  df <- if (nrow(cont) > 0 && nrow(cat) > 0) {
    cbind(cont, cat)
  } else if (nrow(cont) > 0) {
    cont
  } else {
    cat
  }
  df$POINTS <- sum(points)
  df
}

rescaler <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  df
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Make the dendrogram in p-D. Need to add nodes, and edges
data(flea)
df_hc3 <- hclust(dist(flea[,1:6]), method="average")
df_hc5 <- hclust(dist(flea[,1:6]), method="ward.D2")

flea_clw <- flea %>% mutate(cl = factor(cutree(df_hc5, 3)))
flea_cla <- flea %>% mutate(cl = factor(cutree(df_hc3, 3)))
flea_w_hfly <- hierfly(flea_clw, df_hc5)
flea_a_hfly <- hierfly(flea_cla, df_hc3)

library(RColorBrewer)
library(plotly)
library(htmltools)

pal <- brewer.pal(length(unique(flea_w_hfly$data$cl)), "Dark2")
colw <- pal[flea_w_hfly$data$cl]
cola <- pal[flea_a_hfly$data$cl]
glyphs <- c(16, 46)
pchw <- glyphs[flea_w_hfly$data$node+1]
pcha <- glyphs[flea_a_hfly$data$node+1]

# Use plotly to make animation
bases <- save_history(flea_w_hfly$data[, 1:6], max = 10)
tour_path <- interpolate(bases, 0.1)
d <- dim(tour_path)
hcw <- NULL
hcwe <- NULL
hca <- NULL
hcae <- NULL
for (i in 1:d[3]) {
  cat(i, "\n")
  d1 <- as.matrix(flea_w_hfly$data[,1:6]) %*% matrix(tour_path[,,i], ncol=2)
  colnames(d1) <- c("x", "y")
  d1 <- apply(d1, 2, function(x) x-mean(x))
  hcw <- rbind(hcw, cbind(d1, flea_w_hfly$data$cl, flea_w_hfly$data$node,                           rep(i+10, nrow(d1)))) # Add 10 bc plotly can't count single digits
  d2 <- as.matrix(flea_a_hfly$data[,1:6]) %*% matrix(tour_path[,,i], ncol=2)
  colnames(d2) <- c("x", "y")
  d2 <- apply(d2, 2, function(x) x-mean(x))
  hca <- rbind(hca, cbind(d2, flea_a_hfly$data$cl, flea_a_hfly$data$node,                           rep(i+10, nrow(d2))))
  e1 <- cbind(d1[flea_w_hfly$edges[,1],1],
              d1[flea_w_hfly$edges[,2],1],
              d1[flea_w_hfly$edges[,1],2],
              d1[flea_w_hfly$edges[,2],2],
              flea_w_hfly$data$cl[flea_w_hfly$edges[,1]],
              rep(i+10, nrow(flea_w_hfly$edges)))
  colnames(e1) <- c("x", "xend", "y", "yend", "cl", "indx")
  hcwe <- rbind(hcwe, e1)
  e2 <- cbind(d2[flea_a_hfly$edges[,1],1],
              d2[flea_a_hfly$edges[,2],1],
              d2[flea_a_hfly$edges[,1],2],
              d2[flea_a_hfly$edges[,2],2],
              flea_a_hfly$data$cl[flea_a_hfly$edges[,1]],
              rep(i+10, nrow(flea_a_hfly$edges)))
  colnames(e2) <- c("x", "xend", "y", "yend", "cl", "indx")
  hcae <- rbind(hcae, e2)
}
colnames(hcw)[3:5] <- c("cl", "node", "indx")
colnames(hca)[3:5] <- c("cl", "node", "indx")
hcw <- as_tibble(hcw)
hca <- as_tibble(hca)
hcw$cl <- as.factor(hcw$cl)
hca$cl <- as.factor(hca$cl)
hcw$node <- as.factor(hcw$node)
hca$node <- as.factor(hca$node)
hcwe <- as_tibble(hcwe)
hcae <- as_tibble(hcae)
hcwe$cl <- as.factor(hcwe$cl)
hcae$cl <- as.factor(hcae$cl)
# Plot it and make it into an animation
# Wards
p <- ggplot() +
  geom_segment(data = hcwe, aes(x=x, xend=xend, y=y, yend=yend, frame = indx, colour=cl)) +
  geom_point(data = hcw, aes(x = x, y = y, frame = indx, colour=cl, shape=node), size=1) +
  scale_shape_manual(values=c(16, 46)) +
  theme_void() +
  coord_fixed() +
  theme(legend.position="none") +
  scale_color_brewer("", palette="Dark2")
pg <- ggplotly(p, width=450, height=450) %>% animation_opts(200, redraw = FALSE, easing = "linear", transition=0)
save_html(pg, file="cluster_ward.html")
# Average
p <- ggplot() +
  geom_segment(data = hcae, aes(x=x, xend=xend, y=y, yend=yend, frame = indx, colour=cl)) +
  geom_point(data = hca, aes(x = x, y = y, frame = indx, colour=cl, shape=node), size=1) +
  scale_shape_manual(values=c(16, 46)) +
  theme_void() +
  coord_fixed() +
  theme(legend.position="none") +
  scale_color_brewer("", palette="Dark2")
pg <- ggplotly(p, width=450, height=450) %>% animation_opts(200, redraw = FALSE, easing = "linear", transition=0)
save_html(pg, file="cluster_average.html")
