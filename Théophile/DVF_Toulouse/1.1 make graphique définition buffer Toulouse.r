source("access.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu31701 <- iris15 %>% filter(UU2010=="31701") %>% st_union()
uu31701plus20 <- uu31701 %>% st_buffer(20000)
c200_31701 <- c200 %>% filter(st_within(., uu31701, sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu31701))/pi)
lgdrous <- map_dfr(seq(-rmax,50000, 1000),
               ~{
                 uu31701plus <- uu31701 %>% st_buffer(.x)
                 iris <- iris15 %>% filter(st_within(., uu31701plus, sparse=FALSE)) %>% st_drop_geometry()
                 iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>%
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>%
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

ggplot(lgdrous %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()

# uu758 -> Les opportunités de l'aire urbaine
# uu758plus20 -> Les opportunités de l'aire urbaine étendue
# c200_758 -> Le carroyage de l'aire urbaine stricte