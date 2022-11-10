mod <- glm.nb(total ~ ipa*site + veg + yday + I(yday^2), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
summary(mod)


coefs <- coef(mod)
coef.names <- names(coefs)
rest.index <- grep("ipaRestored:", coef.names)

restored.by.site <- coefs[rest.index]

rest.age.by.site <- net_list[[1]] %>%
  group_by(site) %>%
  summarise(restage = mean(rest_age))


newdf <- data.frame(site =  c("COR",names(restored.by.site)),
                    siteeffect = c(0, restored.by.site),
                    restAge <- rest.age.by.site$restage)

with(newdf,plot(x = restAge, y = siteeffect))

# wah wah