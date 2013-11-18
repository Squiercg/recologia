massa<-c(6,8,5,7,9,11)
regiao<-factor(c("Campo Grande","Campo Grande","Campo Grande","Campo Grande","Dourados","Dourados"))

jpeg("01.jpg")
plot(massa~regiao,frame=F)
dev.off()

regiao

levels(regiao)

as.integer(regiao)

levels(regiao)[as.integer(regiao)]

t.test(massa~regiao,var.equal=T)

lm(massa~regiao)

resid(lm(massa~regiao))

model.matrix(~regiao)

summary(lm(massa~regiao))

model.matrix(~regiao-1)

summary(lm(massa~regiao-1))

aggregate(massa,list(regiao),mean)

sqrt(deviance(lm(massa~regiao))/df.residual(lm(massa~regiao)))





