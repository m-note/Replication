library(coefplot)
results <- fitsur$eq[1]
coefplot(results)
coefplot(fitsur)
summary(fitsur)$eq[1]


coefplot(fitsur$eq[[1]], title="Elections and Political Processes (AID110) and Civil\n Society (AID130)  measured by Voice and Accountability", 
         coefficients=c("AID110","AID130","RAID110","RAID130", "DIF02"), newNames=c("AID110"="AID110(†)","RAID110"="RAID110(*)","DIF02"="DIF02"), zeroColor = "black", decreasing=T, intercept=F)
coefplot(fitsur$eq[[2]], title="Rule of Law measured by Rule of Law in WGI",
         coefficients=c("AID122","RAID122","DIF02"), newNames=c("AID120"="AID110","RAID120"="RAID110"), zeroColor = "black", decreasing=T, intercept=F)
coefplot(fitsur$eq[[3]], title="Governance measured by Control of Corruption", 
         coefficients=c("AID140","RAID140","DIF02"), newNames=c("AID140"="AID140","RAID140"="RAID140"), zeroColor = "black", decreasing=T, intercept=F)
coefplot(fitsur$eq[[4]], title="Governance measured by Government Effectiveness",
         coefficients=c("AID140","RAID140","DIF02"), newNames=c("AID140"="AID140","RAID140"="RAID140"), zeroColor = "black", decreasing=T, intercept=F)
coefplot(fitsur$eq[[5]], title="Governance measured by Regulatory Quality",
         coefficients=c("AID140","RAID140","DIF02"), newNames=c("AID140"="AID140(†)","RAID140"="RAID140"), zeroColor = "black", decreasing=T, intercept=F)
coefplot(fitsur$eq[[6]], title="Human Rights measured by CIRI",
         coefficients=c("AID121","RAID121","DIF02"), newNames=c("AID140"="AID121","RAID121"="RAID121(*)"), zeroColor = "black", decreasing=T, intercept=F)

coefplot(fitsur, title="Coefficient with interesting results", 
         coefficients=c("M1_AID110","M1_AID130", "M2_AID122", "M3_AID140", "M4_AID140", "M5_AID140", #"M6_AID121",
                        "M1_DIF02", "M3_DIF02", "M4_DIF02", "M5_DIF02", "M6_DIF02", 
                        "M2_PRF01", "M4_PRF01", "M5_PRF01",
                        "M1_DEP04", "M4_DEP04", "M5_DEP04","M6_DEP04", 
                        "M1_ODA03", "M2_ODA03", "M5_ODA03",
                        "M4_SOC09"), 
         
         newNames=c("M1_AID110"="M1_AID110(†)", "M5_AID140"="M5_AID140(†)",
                    "M1_DIF02"="M1_DIF02(***)", "M3_DIF02"="M3_DIF02(**)", "M4_DIF02"="M4_DIF02(**)", "M5_DIF02"="M5_DIF02(†)", "M6_DIF02"="M6_DIF02(***)", 
                    "M2_PRF01"="M2_PRF01(**)", "M4_PRF01"="M4_PRF01(***)", "M5_PRF01"="M5_PRF01(***)", 
                    "M1_DEP04"="M1_DEP04(*)", "M4_DEP04"="M4_DEP04(*)", "M5_DEP04"="M5_DEP04(**)","M6_DEP04"="M6_DEP04(***)", 
                    "M1_ODA03"="M1_ODA03(**)", "M2_ODA03"="M2_ODA03(**)", "M5_ODA03"="M5_ODA03(*)",
                    "M4_SOC09"="M4_SOC09(*)"), 
         zeroColor = "black", decreasing=T, intercept=F, sort=c("natural"))

########################################################
###For Essay
########################################################
coefplot(fitsur, title="AID Coefficient ", 
         coefficients=c("M1_AID110","M1_AID130", "M2_AID122", "M3_AID140", "M4_AID140", "M5_AID140", "M6_AID121"), 
         newNames=c("M1_AID110"="M1_AID110(†)", "M5_AID140"="M5_AID140(†)"), 
         zeroColor = "black", decreasing=T, intercept=F)

coefplot(fitsur, title="Coefficient with interesting results (Spillover Hypothesis)", 
         coefficients=c( "M1_DIF02", "M3_DIF02", "M4_DIF02", "M5_DIF02", "M6_DIF02"), 
         newNames=c("M1_DIF02"="M1_DIF02(***)", "M3_DIF02"="M3_DIF02(**)", "M4_DIF02"="M4_DIF02(**)", "M5_DIF02"="M5_DIF02(†)", "M6_DIF02"="M6_DIF02(***)"), 
         zeroColor = "black", decreasing=T, intercept=F, sort=c("natural"))

coefplot(fitsur, title="Coefficient with statistical significance", 
         coefficients=c(
                        "M2_PRF01", "M4_PRF01", "M5_PRF01",
                        "M1_DEP04", "M4_DEP04", "M5_DEP04","M6_DEP04", 
                        "M1_ODA03", "M2_ODA03", "M5_ODA03"), 
         
         newNames=c(
                    "M2_PRF01"="M2_PRF01(**)", "M4_PRF01"="M4_PRF01(***)", "M5_PRF01"="M5_PRF01(***)", 
                    "M1_DEP04"="M1_DEP04(*)", "M4_DEP04"="M4_DEP04(*)", "M5_DEP04"="M5_DEP04(**)","M6_DEP04"="M6_DEP04(***)", 
                    "M1_ODA03"="M1_ODA03(**)", "M2_ODA03"="M2_ODA03(**)", "M5_ODA03"="M5_ODA03(*)"), 
         zeroColor = "black", decreasing=T, intercept=F, sort=c("natural"))


coefplot(fitsur_P, title="AID Coefficient (measured with Polity)", 
         coefficients=c("P1_AID110","P1_AID130", "P2_AID122", "P3_AID140", "P4_AID121",
                        "P1_DIF02", "P2_DIF02", "P3_DIF02", "P4_DIF02"), 
         newNames=c("P1_DIF02"="P1_DIF02(**)", "P2_DIF02"="P2_DIF02(**)", "P3_DIF02"="P3_DIF02(**)", "P4_DIF02"="P4_DIF02(**)"), 
         zeroColor = "black", decreasing=T, intercept=F)

coefplot(fitsur_FH, title="AID Coefficient (measured with Freedom House)", 
         coefficients=c("F1_AID110","F1_AID130", "F2_AID122", "F3_AID140", "F4_AID121",
                        "F1_DIF02", "F2_DIF02", "F3_DIF02", "F4_DIF02"), 
         newNames=c("F1_DIF02"="F1_DIF02(***)", "F2_DIF02"="F2_DIF02(***)", "F3_DIF02"="F3_DIF02(***)", "F4_DIF02"="F4_DIF02(***)"), 
         zeroColor = "black", decreasing=T, intercept=F)

coefplot(fitsur_a, title="AID Coefficient \n (All AID are put in each regression equation)", 
         coefficients=c("A1_DIF02", "A3_DIF02", "A3_DIF02", "A4_DIF02",
                        "A1_ODA03", "A2_ODA03",
                        "A1_DEP04", "A4_DEP04",
                        "A2_PRF01", "A4_PRF01",
                        "A2_SOC09", "A4_SOC09"), 
         newNames=c("A1_DIF02"="A1_DIF02(***)", "A3_DIF02"="A3_DIF02(**)", "A4_DIF02"="A4_DIF02(*)",
                    "A1_ODA03"="A1_ODA03(*)", "A2_ODA03"="A2_ODA03(**)",
                    "A1_DEP04"="A1_DEP04(*)", "A4_DEP04"="A4_DEP04(*)",
                    "A2_PRF01"="A2_PRF01(**)", "A4_PRF01"="A4_PRF01(***)",
                    "A2_SOC09"="A2_SOC09(*)", "A4_SOC09"="A4_SOC09(*)"), 
         zeroColor = "black", decreasing=T, intercept=F)


# http://www.inside-r.org/packages/cran/arm/docs/coefplot にデータの調整法が載っている。






summary(fitsur)
str(fitsur)
fitsur$eq
fitsur$eqnLabel
str(coef(fitsur))
coef(summary(fitsur))[, "Std. Error"]

str(summary(fitsur))
summary(fitsur)["eq"][1]



detach("package:coefplot", unload=TRUE)
detach("package:texreg", unload=TRUE)
library(texreg)
testing <- summary(fitsur$eq[[1]])
#for MSWord  HTML-->Copy and Paste
stargazer(testing, type="html", out=".html",
          title="Regression Results", single.row=TRUE)

htmlreg(fitsur, file = "/Users/S/Desktop/texreg.html" , digits=4, custom.model.names = c("M1", "M2", "M3", "M4", "M5", "M6" ),
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,  stars = c(0.001, 0.01, 0.05, 0.1), symbol = "†",
        head.tag = TRUE, body.tag = TRUE)

htmlreg(fitsur_a, file = "/Users/S/Desktop/texreg.html" , digits=4,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
        head.tag = TRUE, body.tag = TRUE)

htmlreg(list(fitsur_a, fitsur_P,fitsur_FH), file = "/Users/S/Desktop/texreg2.html", digits=4,
        custom.model.names = c("A1", "A2", "A3", "A4", "A5", "A6", "P1", "P2", "P3", "P4", "F1", "F2", "F3", "F4" ),
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, stars = c(0.001, 0.01, 0.05, 0.1), symbol = "†",
        head.tag = TRUE, body.tag = TRUE)

screenreg(fitsur)

#######Distribution########
par(mfrow = c(1, 2),width = 680, height = 350)
hist(polity_diff, breaks=10, xlab="difference", main="Fluctuation in Polity")
hist(FrHo_diff, breaks=10, xlab="difference",main="Fluctuation in Freedom House")


par(mfrow = c(3, 2),width = 580, height = 600)
hist(WGI_VAdiff, breaks=25, xlab="difference", main="Histogram of difference in \n WGI Voice and Accountability")
hist(WGI_RLdiff, breaks=25, xlab="difference", main="Histogram of difference in \n WGI Rule of Law")
hist(WGI_CCdiff, breaks=25,  xlab="difference", main="Histogram of difference in \n WGI Control of Corruption")
hist(WGI_GEdiff, breaks=25, xlab="difference", main="Histogram of difference in \n WGI Government Effectiveness")
hist(WGI_RQdiff, breaks=25, xlab="difference", main="Histogram of difference in \n WGI Regulatory Quality")
hist(CIRI_HRdiff, xlab="difference", main="Histogram of difference in \n CIRI")

par(mfrow = c(3, 2))
plot(density(WGI_VAdiff, na.rm=T), xlab="difference", main="Histogram of difference in \n WGI Voice and Accountability")
plot(density(WGI_RLdiff, na.rm=T), xlab="difference", main="Histogram of difference in \n WGI Rule of Law")
plot(density(WGI_CCdiff, na.rm=T))
plot(density(WGI_GEdiff, na.rm=T) )
plot(density(WGI_RQdiff, na.rm=T))
plot(density(CIRI_HRdiff, na.rm=T))
