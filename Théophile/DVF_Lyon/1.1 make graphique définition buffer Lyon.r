source("dvf.r")

c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu758 <- iris15 %>% filter(UU2010=="00758") %>% st_union()
uu758plus20 <- uu758 %>% st_buffer(20000)
c200_758 <- c200 %>% filter(st_within(., uu758, sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu758))/pi)
rha <- map_dfr(seq(-rmax,50000, 1000),
               ~{
                 uu758plus <- uu758 %>% st_buffer(.x)
                 iris <- iris15 %>% filter(st_within(., uu758plus, sparse=FALSE)) %>% st_drop_geometry()
                 iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>%
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>%
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

ggplot(rha %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()

# uu758 -> Les opportunités de l'aire urbaine
# uu758plus20 -> Les opportunités de l'aire urbaine étendue
# c200_758 -> Le carroyage de l'aire urbaine stricte