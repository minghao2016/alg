library(tidyverse)
library(caret)
library(pROC)
library(dummies)
library()


df <- readRDS("...path.../german.rds")

df <- df %>% 
  mutate(GOOD = recode(df$BAD, 
                       "BAD" = "0", 
                       "GOOD" = "1"))
df <- df %>% select(-BAD)

pop <- generate_init_pop(df, 50)

#start here

obj_list <- c(accuracy, get_sensitivity)


start_time <- Sys.time()

o <- evaluate_ind(ind = pop[[1]],df = df, target = "GOOD", objectives = obj_list)


#

start_time <- Sys.time()

epop <- evaluate_population(pop = pop,df = df, target = "GOOD", objectives = obj_list)
colnames(epop)<-c("acc", "sens", "nf")

pareto <- high(acc)*high(sens)*low(nf)
spop <- non_dom_sort(epop, pareto)

plt <- plot_ly(spop, x=~acc, y=~sens, z=~nf, 
                color= ~.level, type="scatter3d", mode = 'markers')
plt

children <- create_children(pop) 
mchildren <- mutate_pop(children, 0.1)

echildren <- evaluate_population(pop = mchildren,df = df, target = "GOOD", 
                                 objectives = obj_list)
colnames(echildren)<-c("acc", "sens", "nf")
rownames(echildren) <- (length(pop)+1):(length(pop)+length(children))

comb_pop <- rbind(epop,echildren)

combined_pop <- c(pop, mchildren)

sorted_comb_pop <- non_dom_sort(comb_pop, pareto)

plt2 <- plot_ly(sorted_comb_pop, x=~acc, y=~sens, z=~nf, 
               color= ~.level, type="scatter3d", mode = 'markers')
plt2

ordered_comb_pop <- sorted_comb_pop[order(sorted_comb_pop$.level),]

end_time <- Sys.time()
end_time - start_time

#pf = pareto front

#by using pipes unique rownames get lost
pf <- ordered_comb_pop #%>% filter(.level == 6) %>% select(-.level) %>% rownames()
# use simple selection
pf <- pf[which(pf$.level==6),]
pf <- pf[,-ncol(pf)]

ans <- execute_selection(pf, 5)


p1 <- plot_ly(x=rp[,1], y=rp[,2], z=rp[,3], type="scatter3d", mode = 'markers') 
p2 <- plot_ly(npf, x=~x, y=~y, z=~z, type="scatter3d", mode = 'markers')
p3 <- plot_ly(ans, x=~x, y=~y, z=~z, type="scatter3d", mode = 'markers')
p <- subplot(p1,p2,p3)
p
