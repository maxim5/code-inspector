data(mtcars)

mtcars[,c(2,8:11)] <- lapply(mtcars[,c(2,8:11)], as.factor)

plot(mtcars$am, mtcars$mpg)
pairs(mtcars, panel = panel.smooth, col = 3 + (mtcars$am == 1))
cor(mtcars[sapply(mtcars, is.numeric)])

summary(lm(mpg ~ ., data = mtcars))$adj.r.squared
summary(lm(mpg ~ am, data = mtcars))$adj.r.squared

summary(lm(mpg ~ . - cyl - gear - carb - vs - disp - drat - hp, data = mtcars))
summary(lm(mpg ~ am + wt + qsec, data = mtcars))

mdl <- lm(mpg ~ . - cyl - gear - carb - vs - disp - drat - hp, data = mtcars)

## residual diagnostics
par(mfcol = c(2,2))
plot(mdl)

sort(rstandard(mdl), decreasing = T)[1:4]

sort(hatvalues(mdl), decreasing = TRUE)[1:4]

sort(dfbetas(mdl), decreasing = T)[1:4]

press <- resid(mdl) / (1 - hatvalues(mdl))
sort(press, decreasing = T)[1:4]

mtcars[rownames(mtcars) %in% c("Chrysler Imperial", "Fiat 128", "Toyota Corolla"),]
