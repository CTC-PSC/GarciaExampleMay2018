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
 setwd(choose.dir())
 load("Data/2018ERA MRE Data.Rdata")

#################
# CYER FUNCTION #
# DIFF OPTIONS  #
#################
  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="addtoesc", ages=c(2,4:5))
  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="ignore"  , ages=c(2,4:5))
  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=c(2,4:5))

  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="addtoesc", ages=c(2:6))
  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="ignore"  , ages=c(2:6))
  cyer(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=c(2:6))

################################
# Apply External HR Adjustment #
# and Check Results            #
################################
 #Nehalem
  z.cy = externalHRadjustment(z.cy, hrt=hrt.nehalem, hrjstk="SRH", type=c("tm"), newstkname="nehalem")
  Nehalem=cyer(stkname="nehalem", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  Nehalem
  summary(Nehalem)
 #Siletz
  z.cy = externalHRadjustment(z.cy, hrt=hrt.siletz, hrjstk="SRH", type=c("tm"), newstkname="siletz")
  Siletz=cyer(stkname="siletz", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  Siletz
  summary(Siletz)
 #Siuslaw
  z.cy = externalHRadjustment(z.cy, hrt=hrt.siuslaw, hrjstk="SRH", type=c("tm"), newstkname="siuslaw")
  Siuslaw = cyer(stkname="siuslaw", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  Siuslaw
  summary(Siuslaw)
 #SRH
  stkloc = grep("SRH",z.cy$stknames)
  SRH = cyer(hrj=subset(stkname="SRH", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  SRH
  summary(SRH)
 #WAC SPECIAL ADJUSTMENT COMMENT !!!!!!!READ ME READ ME READ ME!!!!!!!
  #the hrt files specify that US terminal net & sport term HR's are equal to 0. This is not a 1:1 mapping, 
  #simply meaning that the number of ERA are involved. I am not, at present, overwriting ERA results via the HRT file
  #for all ERA fisheries involved b/c there's 0 harvest in these fisheries (and hence already equal to 0). 
  #You will want to ALWAYS check and confirm this (see below code).
  hrj=subset(z.cy$HRJ_P,stock==grep("QUE",z.cy$stknames))
  with(hrj, tapply(AEQTot4, list(cy,fishery), sum))[,c(31,15,29,26,38,67,60,54,58,56,75,76)]
 #Hoh
  z.cy = externalHRadjustment(z.cy, hrt=hrt.hoh, hrjstk="QUE", type=c("tm"), newstkname="hoh")
  Hoh = cyer(stkname="hoh", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  Hoh
  summary(Hoh)
 #Quillayute
  z.cy = externalHRadjustment(z.cy, hrt=hrt.quillayute, hrjstk="QUE", type=c("tm"), newstkname="quillayute")
  Quillayute = cyer(stkname="quillayute", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  Quillayute
  summary(Quillayute)
 #QUE
  QUE = cyer(stkname="QUE", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")
  QUE
  summary(QUE)
 #Generic stock CYER
  hold=cyer(stkname="RBT", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate")


################
#LYF for Larrie#
################
 ###############
 #ALL AGES: 2-5#
 ###############
  cyer(stkname="LYF", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=2:6)
  summary(cyer(stkname="LYF", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=2:6))
 ###############
 #AGES 3-4 only#
 ###############
  cyer(stkname="LYF", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=3:4)
  summary(cyer(stkname="LYF", hrjobj=z.cy, fmap=flookup, type="AEQTot", strays="separate", ages=3:4))

#############
# MRE CALCS #
#############
 #DATA MANIPULATION
  HRJ=convertHRJtoMRE(z.cy$HRJ_P , datatype="fishery")
  ESC=convertHRJtoMRE(z.cy$ESC_CY, datatype="escapement")
  startagedf = merge(x=slookup, y=data.frame(stknum=1:length(z.cy$stknames),stknames=z.cy$stknames), by.x="ERIS", by.y="stknames")
 #MRE CALCS for all ERA stocks
  MRE=calcMREAll(HRJ=HRJ, ESC=ESC, fisheryinfotable=flookup, mre_startage=startagedf, eris_startage=startagedf)
  
##################################
# CREATE THE PLOT INPUT FILE,    #
# WHICH APPLIES THE MRE CRITERIA #
# AND CREATE THE FIGURES         #
##################################
 #Oregon Coast Special
  garciaplotdata_orc = MRE2Plot(esc=Escap, mre=MRE, smap=slookup_orc, stknames=z.cy$stknames, mrecriteria=TRUE, auxdata=NULL)
  plotGarciaAll(Garcia=garciaplotdata_orc, outdir="Results/", outtype="pdf", pdffilename="2018C&E Garcia Plots - Oregon Coast Special.pdf")
 #WAC Special
  garciaplotdata_wac = MRE2Plot(esc=Escap, mre=MRE, smap=slookup_wac, stknames=z.cy$stknames, mrecriteria=TRUE, auxdata=NULL)
  plotGarciaAll(Garcia=garciaplotdata_wac, outdir="Results/", outtype="pdf", pdffilename="2018C&E Garcia Plots - WAC Special.pdf")
 #2018 C&E Garcia Plots
  garciaplotdata = MRE2Plot(esc=Escap, mre=MRE, smap=slookup, stknames=z.cy$stknames, mrecriteria=TRUE, auxdata=AuxMRE)
  plotGarciaAll(Garcia=garciaplotdata, outdir="Results/", outtype="pdf", pdffilename="2018C&E Garcia Plots - All Stocks.pdf")
 #2018Synoptic Synoptic
  odir=getwd()
  setwd("Results/")
  plotSynopticSynoptic(garcia=garciaplotdata, year=2016, outtype="tiff")
  setwd(odir)
 #Write output as an excel file
  MRE2Excel(x=MRE, stknames=z.cy$stknames, filename="Results/2018ERA MRE Calcs.xlsx")
  write.csv(garciaplotdata, "Results/2018ERA MRE Data Used for Figures.csv", row.names=FALSE)