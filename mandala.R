# Load in libraries
library(ggplot2)
library(dplyr)
library(deldir)
library(sf)
library(viridis)

# Parameters to change as you like
iter=3 # Number of iterations (depth)
points=6 # Number of points
radius=3.8 # Factor of expansion/compression

# Angles of points from center
angles=seq(0, 2*pi*(1-1/points), length.out = points)+pi/2
  
# Initial center
df=data.frame(x=0, y=0)

# Iterate over centers again and again
for (k in 1:iter)
  {
    temp=data.frame()
    for (i in 1:nrow(df))
    {
      data.frame(x=df[i,"x"]+radius^(k-1)*cos(angles), 
                 y=df[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
    }
    df=temp
  }

# Obtain Voronoi regions
df %>%
  select(x,y) %>% 
  deldir(sort=TRUE) -> tmp

#turn into a list of polygons keeping up the area for later use
tmp <- tile.list(tmp)
polys <- vector(mode='list',length=length(tmp))
dat <- NULL
for(i in seq_along(polys)){
  mat <- matrix(c(tmp[[i]]$x,tmp[[i]]$x[1],tmp[[i]]$y,tmp[[i]]$y[1]),
                ncol=2,byrow=FALSE)
  polys[[i]] <- st_polygon(list(mat))
  dat <- rbind(dat,data.frame(AREA=tmp[[i]]$area))
}


## create multipolygon sf objects
pp <- st_sf(dat,st_sfc(polys),row.names=1:length(tmp)) 

#a quick one, color depending on polygon area
plot(pp,pal=viridis(19,direction = -1,option = "B"),
     breaks=seq(1,max(pp$AREA),length.out = 20))
#color depending on row index
pp$Nb <- 1:nrow(pp)
plot(pp["Nb"],pal=viridis(19,direction = -1,option = "D"),
     breaks=seq(1,nrow(pp),length.out = 20))

#choose the colors
pp <- arrange(pp,AREA) #arranging to find out which polygon belong to which area
cols <- c("darkgoldenrod3","brown3","yellow2","seashell","salmon","salmon4","seashell3","seashell","red3",
          "red3","palevioletred3","seashell","seashell2","yellow2","red3","palevioletred3")

png(filename = "Mand1.png",width = 600,height=600,res=200)
plot(pp["AREA"],pal=cols,
     breaks=c(0,unique(pp$AREA)),main = "",key.pos=NULL)
dev.off()


