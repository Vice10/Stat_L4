servo <- read.csv("C:\\Statistics_Labs\\L4_Disperse\\data\\servo.data")

servo$motor <- as.factor(servo$motor)
servo$screw <- as.factor(servo$screw)
servo$pgain <- as.factor(servo$pgain)
servo$vgain <- as.factor(servo$vgain)

plot(servo$pgain, servo$error)

hist(log(servo$error))

oneway2 <- aov(log(error) ~ motor, data=servo)
oneway <- aov(log(error) ~ motor+screw, data=servo)
m1 <- lm(log(error) ~ motor*pgain+screw*vgain, data=servo)
m2 <- lm(log(error) ~ motor, data=servo)

summary(m2)
hist(oneway$residuals, las= 1, breaks = 15)
contrasts(servo$motor) <- contr.sum(5)

library(multcomp)
TukeyHSD(oneway)
summary(glht(oneway2, linfct = mcp(motor = "Tukey")))
pairwise.t.test(log(servo$error), servo$motor,
                p.adjust.method = "BH")

