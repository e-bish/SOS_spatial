mod <- glm.nb(total ~ ipa*site + veg + yday + I(yday^2), data = net_list[[1]], link = "log", control = glm.control(maxit = 500))
summary(mod)
