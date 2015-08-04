emerge.glms14 <-
  function()
{
### Purpose:- Emerge tab requires various GLMs
### ----------------------------------------------------------------------
### Modified from:- emerge.glms
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Oct 2014, 14:28
### ----------------------------------------------------------------------
### Revisions:-

  ## adult.glm <- glm(Adults ~ TempRec + offset(log(Cocoons)), family = poisson,
  ##                  data = emerge.df)
  ## summary(adult.glm)
  ## anova(adult.glm, test = "Chi")
  pred.dat <- unique(emerge14.df[,1:2]) # data used to get predictions
  pred.dat$Cocoons <- 100
  rownames(pred.dat) <- levels(emerge14.df$NomTemp)
require(MASS)
  ## anova(glm(Adults ~ TempRec + offset(log(Cocoons)), family = poisson,
  ##                  data = emerge14.df), test = "Chi")
  ## anova(glm(Adults ~ NomTemp + offset(log(Cocoons)), family = poisson,
  ##                  data = emerge14.df), test = "Chi")

  ## anova(glm(Adults ~ TempRec + offset(log(Cocoons)), family = quasipoisson,
  ##                  data = emerge14.df), test = "Chi")
  ## anova(glm(Adults ~ NomTemp + offset(log(Cocoons)), family = quasipoisson,
  ##                  data = emerge14.df), test = "Chi")

  ## Predicted proportion of emerging adults
  ## adult.pred <- predict(adult.glm, pred.dat, type = "response", se.fit = TRUE)

  ## Models for numbers of emerging adults
  adult.glmB <- glm(cbind(Adults, Cocoons - Adults) ~ TempRec,
                    family = binomial, data = emerge14.df) # Binomial
  adult.predB <- predict(adult.glmB, pred.dat, type = "response", se.fit = TRUE)
  summary(adult.glmB)
  anova(adult.glmB, test = "Chi") # p=0.5981

  adult.glmQB <- glm(cbind(Adults, Cocoons - Adults) ~ TempRec,
                     family = quasibinomial, data = emerge14.df) # Quasi-binomial
  summary(adult.glmQB)
  anova(adult.glmQB, test = "Chi") # p=0.4803

  adult.glmNB <- glm.nb(Adults ~ TempRec+ offset(log(Cocoons)),  # negativebinomial
                        data = emerge14.df, maxit = 150)
  summary(adult.glmNB)
  anova(adult.glmNB, test = "Chi") # p=0.7493

  ## Predicted proportion of emerging adults
  adult.predB <- predict(adult.glmB, pred.dat, type = "response", se.fit = TRUE)
  adult.predQB <- predict(adult.glmQB, pred.dat, type = "response", se.fit = TRUE)
  adult.predNB <- predict(adult.glmNB, pred.dat, type = "response", se.fit = TRUE)
  as.data.frame(adult.predB[-3])
  as.data.frame(adult.predQB[-3])
  as.data.frame(adult.predNB[-3])

############################################################################
  ## then as Nominal temperature levels
  adult.glmB_F<- glm(cbind(Adults, Cocoons - Adults) ~ NomTemp,
                     family = binomial, data = emerge14.df) # Binomial _F >> factors
  adult.predB_F <- predict(adult.glmB_F, pred.dat, type = "response", se.fit = TRUE)
  summary(adult.glmB_F)
  anova(adult.glmB_F, test = "Chi") # p=0.03224
  browser()

  adult.glmQB_F <- glm(cbind(Adults, Cocoons - Adults) ~ NomTemp,  # ~~~
                       family = quasibinomial, data = emerge14.df) # Quasi-binomial
  summary(adult.glmQB_F)  # ~~~
  options(digits = 7)
  anova(adult.glmQB_F, test = "Chi") # p=0.1987  # ~~~

  adult.glmNB_F <- glm.nb(Adults ~ NomTemp+ offset(log(Cocoons)), # NegativeBinomial
                          data = emerge14.df, maxit = 200)
  summary(adult.glmNB_F)
  anova(adult.glmNB_F, test = "Chi") # p=0.9918

  ## Predicted proportion of emerging adults
  adult.predB_F <- predict(adult.glmB_F, pred.dat, type = "response", se.fit = TRUE)
  adult.predQB_F <- predict(adult.glmQB_F, pred.dat, type = "response", se.fit = TRUE)
  adult.predNB_F <- predict(adult.glmNB_F, pred.dat, type = "response", se.fit = TRUE)
  as.data.frame(adult.predB_F[-3])
  as.data.frame(adult.predQB_F[-3])  # ~~~
  as.data.frame(adult.predNB_F[-3])



  browser()
  ## Models for numbers of unemerged larvae
  larvae.glmB <- glm(cbind(Larvae, Cocoons - Larvae) ~ TempRec,
                     family = binomial, data = emerge14.df) # Binomial
  larvae.predB <- predict(larvae.glmB, pred.dat, type = "response", se.fit = TRUE)
  summary(larvae.glmB)
  anova(larvae.glmB, test = "Chi") # p=0.3274

  larvae.glmQB <- glm(cbind(Larvae, Cocoons - Larvae) ~ TempRec,
                      family = quasibinomial, data = emerge14.df) # Quasi-binomial
  summary(larvae.glmQB)   
  anova(larvae.glmQB, test = "Chi") # p=0.4803 

  larvae.glmNB <- glm.nb(Larvae ~ TempRec+ offset(log(Cocoons)),  # negativebinomial
                         data = emerge14.df, maxit = 150)
  summary(larvae.glmNB)
  anova(larvae.glmNB, test = "Chi") # p=0.7493

  ## Predicted proportion of emerging adults
  adult.predB <- predict(adult.glmB, pred.dat, type = "response", se.fit = TRUE)
  adult.predQB <- predict(adult.glmQB, pred.dat, type = "response", se.fit = TRUE)
  adult.predNB <- predict(adult.glmNB, pred.dat, type = "response", se.fit = TRUE)
  as.data.frame(adult.predB[-3])
  as.data.frame(adult.predQB[-3])
  as.data.frame(adult.predNB[-3])


  ## Predicted proportion of emerging adults
  larvae.glm <- glm(Larvae ~ TempRec + offset(log(Cocoons)), family = poisson,
                    data = emerge14.df)
  summary(larvae.glm)
  anova(larvae.glm, test = "Chi")

  larvae.glmQ <- glm(Larvae ~ TempRec+ offset(log(Cocoons)), family = quasipoisson,
                     data = emerge14.df)
  summary(larvae.glmQ)
  anova(larvae.glmQ, test = "Chi")

  larvae.glmNB <- glm.nb(Larvae ~ TempRec+ offset(log(Cocoons)), 
                         data = emerge14.df)
  summary(larvae.glmNB)
  anova(larvae.glmNB, test = "Chi")

  unemerg.glm <- glm(Unemerged ~ TempRec + offset(log(Cocoons)), family = poisson,
                     data = emerge14.df)
  summary(unemerg.glm)
  anova(unemerg.glm, test = "Chi")

  unemerg.glmQ <- glm(Unemerged ~ TempRec+ offset(log(Cocoons)),
                      family = quasipoisson, data = emerge14.df)
  summary(unemerg.glmQ)
  anova(unemerg.glmQ, test = "Chi")

  unemerg.glmNB <- glm.nb(Unemerged ~ TempRec+ offset(log(Cocoons)), 
                          data = emerge14.df)
  summary(unemerg.glmNB)
  anova(unemerg.glmNB, test = "Chi")
  emerge14.df <- within(emerge14.df, NomTemp <- ordered(NomTemp))

  ## overwrite those glms with Temp as a factor


  adult.glm <- glm(Adults ~ NomTemp + offset(log(Cocoons)), family = poisson,
                   data = emerge14.df)
  summary(adult.glm)
  anova(adult.glm, test = "Chi")

  adult.glmQ <- glm(Adults ~ NomTemp+ offset(log(Cocoons)), family = quasipoisson,
                    data = emerge14.df)
  summary(adult.glmQ)
  anova(adult.glmQ, test = "Chi")

  larvae.glm <- glm(Larvae ~ NomTemp + offset(log(Cocoons)), family = poisson,
                    data = emerge14.df)
  summary(larvae.glm)
  anova(larvae.glm, test = "Chi")

  larvae.glmQ <- glm(Larvae ~ NomTemp+ offset(log(Cocoons)), family = quasipoisson,
                     data = emerge14.df)
  summary(larvae.glmQ)
  anova(larvae.glmQ, test = "Chi")

  larvae.glmNB <- glm.nb(Larvae ~ NomTemp+ offset(log(Cocoons)), 
                         data = emerge14.df)
  summary(larvae.glmNB)
  anova(larvae.glmNB, test = "Chi")

  unemerg.glm <- glm(Unemerged ~ NomTemp + offset(log(Cocoons)), family = poisson,
                     data = emerge14.df)
  summary(unemerg.glm)
  anova(unemerg.glm, test = "Chi")

  unemerg.glmQ <- glm(Unemerged ~ NomTemp+ offset(log(Cocoons)),
                      family = quasipoisson, data = emerge14.df)
  summary(unemerg.glmQ)
  anova(unemerg.glmQ, test = "Chi")

  unemerg.glmNB <- glm.nb(Unemerged ~ NomTemp+ offset(log(Cocoons)), 
                          data = emerge14.df)
  summary(unemerg.glmNB)
  anova(unemerg.glmNB, test = "Chi")
  emerge14.df <- within(emerge.df, NomTemp <- factor(NomTemp))



  gg

  cocoon.glm <- glm(Cocoons ~ NomTemp, family = poisson,
                    data = emerge14.df)
  summary(cocoon.glm)
  anova(cocoon.glm, test = "Chi")

  cocoon.glmQ <- glm(Cocoons ~ NomTemp, family = quasipoisson,
                     data = emerge14.df)
  summary(cocoon.glmQ)
  anova(cocoon.glmQ, test = "Chi")

  
}
