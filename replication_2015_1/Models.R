r1 <- WGI_VAdiff ~ AID110 + AID130 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, 

r1 <- WGI_VAdiff ~ AID110lag + AID130lag + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)

#####################################################################################

r1 <- WGI_VAdiff ~ AID110 + AID130 + AID000 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + AID000  + SOC09 + SOC10 + Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, 

r1 <- WGI_VAdiff ~ AID110lag + AID130lag + AID000lag + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + AID000lag  + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)

# Roubust
rp1 <- polity_diff ~ DG01lag + AID110lag + AID130lag + AID000lag + PRF01 + SOC09 + SOC10  + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
rp2 <- polity_diff ~ DG01lag + AID120lag + AID000lag  + PRF01 + SOC09 + SOC10  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
rp3 <- polity_diff ~ DG01lag + AID140lag + AID000lag  + PRF01 + SOC09 + SOC10  + Inc_Consensus # Control of Corruption = Governance(AID140)
#rp4 <- polity_diff ~ WGI_GElag + AID140lag + PRF01 + Inc_Consensus # Government Effectiveness = Governance(AID140)
#rp5 <- polity_diff ~ WGI_RQlag + AID140lag + PRF01 + Inc_Consensus # Regulatory Quality = Governance(AID140)
rp6 <- polity_diff ~ DG01lag + AID121lag + AID000lag  + PRF01 + SOC09 + SOC10  + Inc_Consensus # Human Rights = CIRI (AID121)

rp1 <- polity_diff ~ AID110 + AID130 + AID000 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
rp2 <- polity_diff ~ AID120 + AID000 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
rp3 <- polity_diff ~ AID140 + AID000 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Control of Corruption = Governance(AID140)
#rp4 <- polity_diff ~ AID140 + PRF01 + Inc_Consensus # Government Effectiveness = Governance(AID140)
#rp5 <- polity_diff ~ AID140 + PRF01 + Inc_Consensus # Regulatory Quality = Governance(AID140)
rp6 <- polity_diff ~ AID121 + AID000 + PRF01 + SOC09 + SOC10  + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(VA = rp1, RL = rp2, CC = rp3, HR = rp6), data=data)
summary(fitsur)

#####################################################################################

r1 <- WGI_VAdiff ~ AID110 + AID130 + AID000 + RAID110 + RAID130 + RAID000 + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + AID000 + RAID120 + RAID000   + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + AID000 + RAID121 + RAID000   + SOC09 + SOC10 + Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, RAID100: Regional Programs in Democracy and Governance,
#RAID000: Total Regional Investment in Other Sectors

r1 <- WGI_VAdiff ~ AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag  + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)

#####################################################################################

r1 <- WGI_VAdiff ~ AID110 + AID130 + AID000 + RAID110 + RAID130 + RAID000 + PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + AID000 + RAID120 + RAID000   + SOC09 + SOC10 + FPP01+ Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01+ Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01+ Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01+ Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + AID000 + RAID121 + RAID000   + SOC09 + SOC10 + FPP01+ Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, RAID100: Regional Programs in Democracy and Governance,
#RAID000: Total Regional Investment in Other Sectors, FPP01: Military Assistance Priority

r1 <- WGI_VAdiff ~ AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag  + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110 + AID130 + AID000 + RAID110 + RAID130 + 
        RAID000 + PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120 + AID000 + RAID120 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140 + AID000 + RAID140 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140 + AID000 + RAID140 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140 + AID000 + RAID140 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121 + AID000 + RAID121 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)

rp1 <- polity_diff ~ AID110 + AID130 + AID000 + RAID110 + RAID130 + 
        RAID000 + PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
rp2 <- polity_diff ~ AID120 + AID000 + RAID120 + RAID000 +
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
rp3 <- polity_diff ~ AID140 + AID000 + RAID140 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Control of Corruption = Governance(AID140)
rp6 <- polity_diff ~ AID121 + AID000 + RAID121 + RAID000 + 
        PRF01 + SOC09 + SOC10 + FPP01 + Inc_Consensus # Human Rights = CIRI (AID121)

fitsur_P <- systemfit(list(VA = rp1, RL = rp2, CC = rp3, HR = rp6), data=data)
summary(fitsur)



#####################################################################################

r1 <- WGI_VAdiff ~ AID110 + AID130 + AID000 + RAID110 + RAID130 + RAID000 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120 + PRF01 + AID000 + RAID120 + RAID000   + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140 + PRF01 + AID000 + RAID140 + RAID000   + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121 + PRF01 + AID000 + RAID121 + RAID000   + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  + Inc_Consensus # Human Rights(CIRI) =  Human Rights(AID121)
#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, RAID100: Regional Programs in Democracy and Governance,
#RAID000: Total Regional Investment in Other Sectors, FPP01: Military Assistance Priority,
#DEP04: Fuel Exports (Percentage of Merchandise Exports), L222I: Income Inequality with imputation, 
#PRF05: Capital Mobility, ODA03: Bilateral Non-US assistance (in Millions of USD, All Sectors)


r1 <- WGI_VAdiff ~ AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag  + SOC09 + SOC10 + PRF01 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

r1 <- WGI_VAdiff ~ WGI_VAlag + AID110lag + AID130lag + AID000lag + RAID110lag + RAID130lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID120lag + AID000lag + RAID120lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Rule of Law = Rule of Law(AID120)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140lag + AID000lag + RAID140lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121lag + AID000lag + RAID121lag + RAID000lag + PRF01 + SOC09 + SOC10 + Inc_Consensus # Human Rights = CIRI (AID121)

library(systemfit)
data$RAID122 <- data$RAID120 - data$RAID121
r1 <- WGI_VAdiff ~ WGI_VAlag + AID110 + AID130 + AID000 + RAID110 + RAID130 + ODA03 + DIF02 +
            RAID000 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + AID122 + AID000 + RAID122 + RAID000 + ODA03 + DIF02 +
            PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Rule of Law = Rule of Law other than human rights(AID122)
r3 <- WGI_CCdiff ~ WGI_CClag + AID140 + AID000 + RAID140 + RAID000 + ODA03 + DIF02 +
            PRF01 + SOC09 + SOC10 + FPP01 +  DEP04 + L222I + PRF05  # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + AID140 + AID000 + RAID140 + RAID000 + ODA03 + DIF02 +
            PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + AID140 + AID000 + RAID140 + RAID000 + ODA03 + DIF02 +
            PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + AID121 + AID000 + RAID121 + RAID000 + ODA03 + DIF02 +
            PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Human Rights = CIRI (AID121)

fitsur <- systemfit(list(M1 = r1, M2 = r2, M3 = r3, M4 = r4, M5 = r5, M6 = r6), data=data, method="SUR")
summary(fitsur)

rp7 <- polity_diff ~ data$DG01lag + AID110 + AID130 + AID000 + RAID110 + RAID130 + RAID000 + ODA03 + DIF02 +
          PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Voice and Accountability = Elections and Political Processes + Civil Society
rp8 <- polity_diff ~ data$DG01lag + AID122 + AID000 + RAID122 + RAID000 + ODA03 + DIF02 +
          PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Rule of Law = Rule of Law(AID120)
rp9 <- polity_diff ~ data$DG01lag + AID140 + AID000 + RAID140 + RAID000 + ODA03 + DIF02 +
          PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Control of Corruption = Governance(AID140)
rp10 <- polity_diff ~ data$DG01lag + AID121 + AID000 + RAID121 + RAID000 + ODA03 + DIF02 +
          PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Human Rights = CIRI (AID121)

fitsur_P <- systemfit(list(P1 = rp7, P2 = rp8, P3 = rp9, P4 = rp10), data=data, method = "SUR")
summary(fitsur_P)

rfh7 <- FrHo_diff ~ data$DG02lag + AID110 + AID130 + AID000 + RAID110 + RAID130 + RAID000 + ODA03 + DIF02 + 
        PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05   # Voice and Accountability = Elections and Political Processes + Civil Society
rfh8 <- FrHo_diff ~ data$DG02lag + AID122 + AID000 + RAID122 + RAID000 + ODA03 + DIF02 +
        PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Rule of Law = Rule of Law(AID120)
rfh9 <- FrHo_diff ~ data$DG02lag + AID140 + AID000 + RAID140 + RAID000 + ODA03 + DIF02 + 
        PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Control of Corruption = Governance(AID140)
rfh10 <- FrHo_diff ~ data$DG02lag + AID121 + AID000 + RAID121 + RAID000 + ODA03 + DIF02 +
        PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Human Rights = CIRI (AID121)

fitsur_FH <- systemfit(list(F1 = rfh7, F2 = rfh8, F3 = rfh9, F4 = rfh10), data=data, method = "SUR")
summary(fitsur_FH)

#####################################################################################
# Model: All are put
r1a <- WGI_VAdiff ~ WGI_VAlag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + DIF02 # Voice and Accountability = Elections and Political Processes + Civil Society
r2a <- WGI_RLdiff ~ WGI_RLlag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + DIF02  # Rule of Law = Rule of Law other than human rights(AID122)
r3a <- WGI_CCdiff ~ WGI_CClag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 +  DEP04 + L222I + PRF05 + DIF02  # Control of Corruption = Governance(AID140)
r4a <- WGI_GEdiff ~ WGI_GElag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + DIF02 # Government Effectiveness = Governance(AID140)
r5a <- WGI_RQdiff ~ WGI_RQlag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + DIF02  # Regulatory Quality = Governance(AID140)
r6a <- CIRI_HRdiff ~ CIRI_HRlag + AID110 + AID121 + AID122 + AID130 + AID140  + ODA03 + DIF02 + AID000 + RAID000 +
  RAID110 + RAID121 + RAID122 + RAID130 + RAID140 + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 + DIF02  # Human Rights = CIRI (AID121)

fitsur_a <- systemfit(list(A1 = r1a, A2 = r2a, A3 = r3a, A4 = r4a, A5 = r5a, A6 = r6a), data=data, method="SUR")
summary(fitsur_a)

#####################################################################################

#AID log モデル
data$RAID122 <- data$RAID120 - data$RAID121
data$log_ODA03 <- log(data$ODA03)
r1 <- WGI_VAdiff ~ WGI_VAlag + log(AID110) + log(AID130) + log(AID000) + log(RAID110) + log(RAID130) + log(ODA03) +　 DIF02 + 
  log(RAID000) + PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Voice and Accountability = Elections and Political Processes + Civil Society
r2 <- WGI_RLdiff ~ WGI_RLlag + log(AID122) + log(AID000) + log(RAID122) + log(RAID000) + log(ODA03) +　 DIF02 + 
  PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Rule of Law = Rule of Law other than human rights(AID122)
r3 <- WGI_CCdiff ~ WGI_CClag + log(AID140) + log(AID000) + log(RAID140) + log(RAID000) + log(ODA03) + DIF02 + 
  PRF01 + SOC09 + SOC10 + FPP01 +  DEP04 + L222I + PRF05  # Control of Corruption = Governance(AID140)
r4 <- WGI_GEdiff ~ WGI_GElag + log(AID140) + log(AID000) + log(RAID140) + log(RAID000) + log(ODA03) + DIF02 + 
  PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05 # Government Effectiveness = Governance(AID140)
r5 <- WGI_RQdiff ~ WGI_RQlag + log(AID140) + log(AID000) + log(RAID140) + log(RAID000) + log(ODA03) + DIF02 + 
  PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Regulatory Quality = Governance(AID140)
r6 <- CIRI_HRdiff ~ CIRI_HRlag + log(AID121) + log(AID000) + log(RAID121) + log(RAID000) + log(ODA03) + DIF02 + 
  PRF01 + SOC09 + SOC10 + FPP01 + DEP04 + L222I + PRF05  # Human Rights = CIRI (AID121)

#PRF01: Economic growth, SOC09: Religious Fractionalization, SOC10: Ethnic Fractionalization,
#AID000: Total Investment in Other Sectors than DG, RAID100: Regional Programs in Democracy and Governance,
#RAID000: Total Regional Investment in Other Sectors, FPP01: Military Assistance Priority,
#DEP04: Fuel Exports (Percentage of Merchandise Exports), L222I: Income Inequality with imputation, 
#PRF05: Capital Mobility, ODA03: Bilateral Non-US assistance (in Millions of USD, All Sectors)

fitsur <- systemfit(list(VA = r1, RL = r2, CC = r3, GE = r4, RQ = r5, HR = r6), data=data)
summary(fitsur)
