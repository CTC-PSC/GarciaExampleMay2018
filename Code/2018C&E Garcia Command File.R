#Remove all the objects in the memory
 rm(list = ls(all=TRUE)) 

###########################
# LOAD                    #
# FUNCTIONS               #
# 2018ERA STOCK, FISHERY  #
# GARCIA LOOKUPS          #
# Escapement Data         #
# Auxiliary Data          #
# HRT Data                #
# 2018ERA HRJ DATA        #
###########################
 load("Data/2018ERA MRE Data.Rdata")

################
# CYER TESTING #
# DIFF OPTIONS #
################
  stkloc = grep("SRH",z.cy$stknames)

  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="addtoesc", ages=c(2,4:5))
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="ignore"  , ages=c(2,4:5))
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=c(2,4:5))

  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="addtoesc", ages=c(2:6))
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="ignore"  , ages=c(2:6))
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=c(2:6))

################################
# Apply External HR Adjustment #
# and Check Results            #
################################
 #SRH
  stkloc = grep("SRH",z.cy$stknames)
  SRH = cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate")
  SRH
  summary(SRH)
 #Nehalem
  z.cy = externalHRadjustment(z.cy, hrt=hrt.nehalem, hrjstk="SRH", type=c("tm"), newstkname="nehalem")
  Nehalem=cyer(hrj=subset(z.cy$HRJ_P,stock==58), esc=subset(z.cy$ESC_CY,stock==58), fmap=flookup, type="AEQTot", strays="separate")
  Nehalem
  summary(Nehalem)
 #Siletz
  z.cy = externalHRadjustment(z.cy, hrt=hrt.siletz, hrjstk="SRH", type=c("tm"), newstkname="siletz")
  Siletz=cyer(hrj=subset(z.cy$HRJ_P,stock==59), esc=subset(z.cy$ESC_CY,stock==59), fmap=flookup, type="AEQTot", strays="separate")
  Siletz
  summary(Siletz)
 #Siuslaw
  z.cy = externalHRadjustment(z.cy, hrt=hrt.siuslaw, hrjstk="SRH", type=c("tm"), newstkname="siuslaw")
  Siuslaw = cyer(hrj=subset(z.cy$HRJ_P,stock==60), esc=subset(z.cy$ESC_CY,stock==60), fmap=flookup, type="AEQTot", strays="separate")
  Siuslaw
  summary(Siuslaw)
 #Generic stock CYER
  stkloc = grep("LYF",z.cy$stknames)
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate")

################
#LYF for Larrie#
################
  stkloc = grep("LYF",z.cy$stknames)
 ###############
 #ALL AGES: 2-5#
 ###############
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=2:6)
  summary(cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=2:6))
 ###############
 #AGES 3-4 only#
 ###############
  cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=3:4)
  summary(cyer(hrj=subset(z.cy$HRJ_P,stock==stkloc), esc=subset(z.cy$ESC_CY,stock==stkloc), fmap=flookup, type="AEQTot", strays="separate", ages=3:4))

#############
# MRE CALCS #
#############
 #DATA MANIPULATION
  HRJ=convertHRJtoMRE(z.cy$HRJ_P , datatype="fishery")
  ESC=convertHRJtoMRE(z.cy$ESC_CY, datatype="escapement")
 #MRE CALCS for all ERA stocks
  MRE=calcMREAll(HRJ, ESC, flookup, "guess", "guess")

##################################
# CREATE THE PLOT INPUT FILE,    #
# WHICH APPLIES THE MRE CRITERIA #
# AND CREATE THE FIGURES         #
##################################
#Oregon Coastal Special
 garciaplotdata_orc = MRE2Plot(esc=Escap, mre=MRE, smap=slookup_orc, stknames=z.cy$stknames, mrecriteria=TRUE, auxdata=NULL)
 plotGarciaAll(Garcia=garciaplotdata_orc, outdir=outputDir, outtype="pdf", pdffilename="2018C&E Garcia Plots - Oregon Coast Special 8May2018.pdf")
#2018MRE
 garciaplotdata = MRE2Plot(esc=Escap, mre=MRE, smap=slookup, stknames=z.cy$stknames, mrecriteria=TRUE, auxdata=AuxMRE)
 plotGarciaAll(Garcia=garciaplotdata, outdir=outputDir, outtype="pdf", pdffilename="2018C&E Garcia Plots - All Stocks 8May2018.pdf")
#Write output as an excel file
 MRE2Excel(x=MRE, stknames=z.cy$stknames, filename="Results/2018ERA MRE Calcs 8May2018.xlsx")
