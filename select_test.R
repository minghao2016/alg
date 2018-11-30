library("rPref")
library("plotly")
library("dplyr")

data("mtcars")



invert_items <- function(x){
  x <- 1/x
  return(x)
}
dat <- mtcars %>% select(mpg, hp, qsec)
                         
nd <- lapply(dat, invert_items)
nd <- matrix(unlist(nd), ncol = 3, byrow = TRUE)
colnames(nd) <- c("x", "y", "z")
rownames(nd) <- 1:nrow(nd)

p <- low(x)*low(y)*low(z)

nd <- data.frame(nd)

pf <- non_dom_sort(nd, p)


pf1 <- pf1 %>% select(1:3)

ip <- compute_ideal_point(pf1)
tpf <- translate_objectives(pf1, ip)
ep <- get_extreme_points(tpf)
inter <- get_intercepts(ep)
npf <- normalize_objectives(tpf, inter, ip)

#p1 <- plot_ly(npf, x=~x, y=~y, z=~z, type="scatter3d", mode="markers")
#p1

rp <- ref_points(3)

ans <- npf %>% gen_refs(rp) %>% sel_points(npf,5)


p1 <- plot_ly(x=rp[,1], y=rp[,2], z=rp[,3], type="scatter3d", mode = 'markers') 
p2 <- plot_ly(npf, x=~x, y=~y, z=~z, type="scatter3d", mode = 'markers')
p3 <- plot_ly(ans, x=~x, y=~y, z=~z, type="scatter3d", mode = 'markers')

p <- subplot(p1,p2,p3)
p
