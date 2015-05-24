# 3

par(mfrow=c(2,3))

subMtcars = mtcars[, c("mpg", "disp","hp", "wt", "drat" )]

plot(x=subMtcars$disp, y=subMtcars$hp, xlab="displacement", ylab="horsepower", main="Mtcars: Displacement - Horsepower")
plot(x=subMtcars$disp, y=subMtcars$wt, xlab="displacement", ylab="weight", main="Mtcars: Displacement - Weight")
plot(x=subMtcars$disp, y=subMtcars$drat, xlab="displacement", ylab="rear axle ration", main="Mtcars: Displacement - Rear Axle Ratio")
plot(x=subMtcars$hp, y=subMtcars$wt, xlab="horsepower", ylab="weight", main="Mtcars: Horsepower - Weight")
plot(x=subMtcars$hp, y=subMtcars$drat, xlab="horsepower", ylab="rear axle ration", main="Mtcars: Horsepower - Rear Axle Ratio")
plot(x=subMtcars$wt, y=subMtcars$drat, xlab="weight", ylab="rear axle ration", main="Mtcars: Weight - Rear Axle Ratio")


fm <- lm(subMtcars$mpg ~ subMtcars$disp + subMtcars$hp + subMtcars$drat + subMtcars$wt)
summary(fm)
