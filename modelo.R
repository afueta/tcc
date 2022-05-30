df<- read_fst("base.fst")

colnames(df)

model1 <- lm(logwageCLT ~ loghhi , df)
model2 <- lm(logwageCLT ~ loghhi + `Conta Própria + sem CLT` , df)
model3 <- lm(logwageCLT ~ loghhi + `Conta Própria + sem CLT` + loghhi*`Conta Própria + sem CLT` , df)
model4 <- lm(logwageCLT ~ loghhi + `Conta Própria + sem CLT` + loghhi*`Conta Própria + sem CLT`
             + i0 + i5 + i10 + i15 + i18 + i20 + i25 + i30 + i35 + i40 + i45 + i50 + i55 + i60
             + mulher
             + fundamental + medio + superior
             + logpop, df)
summary(model4)

stargazer(model1,model2,model3)

model1 <- lm(logwage ~ loghhi , df)
model2 <- lm(logwage ~ loghhi + `Conta Própria + sem CLT` , df)
model3 <- lm(logwage ~ loghhi + `Conta Própria + sem CLT` + loghhi*`Conta Própria + sem CLT` , df)
stargazer(model1,model2,model3)



#homem e mulheres
#etaria fração
#escolaridade 
#log pop
#idh por municipio
