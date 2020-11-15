source("access.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu44701 <- iris15 %>% filter(UU2010=="44701") %>% st_union()
uu44701plus20 <- uu44701 %>% st_buffer(20000)
c200_44701 <- c200 %>% filter(st_within(., uu44701, sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu44701))/pi)
pdloire <- map_dfr(seq(-rmax,50000, 1000),
               ~{
                 uu44701plus <- uu44701 %>% st_buffer(.x)
                 iris <- iris15 %>% filter(st_within(., uu44701plus, sparse=FALSE)) %>% st_drop_geometry()
                 iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>%
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>%
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

ggplot(pdloire %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()

# uu758 -> Les opportunités de l'aire urbaine
# uu758plus20 -> Les opportunités de l'aire urbaine étendue
# c200_758 -> Le carroyage de l'aire urbaine stricte