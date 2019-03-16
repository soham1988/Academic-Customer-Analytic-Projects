setwd("C:/Users/soham/OneDrive/Desktop/Sem 3/Customer Analytics/WD")
df1<-as.data.frame(read.csv("coupon_data1.csv", head=TRUE,stringsAsFactors = TRUE))
head(df1)

lm2 = lm(sales ~ coupon + age, data = df1)
summary(lm2)

N_treat = sum(df1$coupon)
ate = as.numeric(lm2$coefficients["coupon"])
inc_rev = N_treat*ate
inc_rev

library(describer)
df2<-as.data.frame(read.csv("coupon_data2.csv", head=TRUE,stringsAsFactors = TRUE))
head(df2)

head(df2[df2$coupon == 0,])
describe(df2$age[df2$coupon==0])
describe(df2$age[df2$coupon==1])



df3<-as.data.frame(read.csv("nutella_data.csv", head=TRUE,stringsAsFactors = TRUE))


head(df3)
tail(df3)


model_1a<-lm(sales~price+promo,data=df3)
summary(model_1a)


model_1b<-lm(sales~price+promo+month+I(month^2),data=df3)
summary(model_1b)

pre_m_1b=predict(model_1b,newdata=X)
pre_m_1b
X = data.frame(price=0, promo=0,month=6)



model_1c<-lm(sales~price+promo+factor(month),data=df3)
summary(model_1c)
pre_m_1c=predict(model_1c,newdata=X)
pre_m_1c



model_1d<-lm(sales~price+promo+as.numeric(month)+factor(store),data=df3)
summary(model_1d)
X1 = data.frame(price=c(1.01,1.01), promo=c(0,0),month=c(0,0),store=c(1,2))

pre_m_1d=predict(model_1d,newdata=X1)
pre_m_1d



newdf3 = df3
newdf3$price = 1.01*df3$price
model3L.yhat2 = predict(model_1d,newdata=X1)
model3L.yhat2_1=model3L.yhat2.X1
model3L.yhat2_1
model3L.yhat2


model_1e<-lm(sales~price:promo+as.numeric(month)+factor(store),data=df3)
summary(model_1e)
ate = as.numeric(model_1e$coefficients["price:promo"])
ate



df4<-as.data.frame(read.csv("cracker_data.csv", head=TRUE,stringsAsFactors = TRUE))
head(df4)
library(mlogit)
DF_long = mlogit.data(df4,shape='wide',choice='choice',sep=".",varying=3:10)
head(DF_long)

model_2a = mlogit(choice ~ price+feat | 1 | 0, data=DF_long)
summary(model_2a)


P1 = fitted(model_2a, outcome = FALSE)
colMeans(P1)



me_nb_Pricenb = mean(P1[,2]*(1-P1[,2])*model_2a$coefficients["price"])
me_nb_Pricenb

me_nb_Pricenb*.75


eff_4.2.3 = -.025*mean(df4$price.nabisco)*me_nb_Pricenb
eff_4.2.3


me_sh_Pricenb = mean(-P1[,4]*P1[,2]*model_2a$coefficients["price"])
me_sh_Pricenb


model_2b = mlogit(choice ~ price+feat | 1+loyalty | 0, data=DF_long)
summary(model_2b)


P2 = fitted(model_2b, outcome = FALSE)
colMeans(P2)

table(df4$choice)/length(df4$choice)


