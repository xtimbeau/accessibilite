source("access.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu35701 <- iris15 %>% filter(UU2010=="35701") %>% st_union()
uu35701plus5 <- uu35701 %>% st_buffer(5000)
c200_35701 <- c200 %>% filter(st_within(., uu35701, sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu35701))/pi)
bre <- map_dfr(seq(-rmax,50000, 1000),
               ~{
                 uu35701plus5 <- uu35701 %>% st_buffer(.x)
                 iris <- iris15 %>% filter(st_within(., uu35701plus5, sparse=FALSE)) %>% st_drop_geometry()
                 iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>%
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>%
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

ggplot(bre %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()

# uu758 -> Les opportunités de l'aire urbaine
# uu758plus20 -> Les opportunités de l'aire urbaine étendue
# c200_758 -> Le carroyage de l'aire urbaine stricte