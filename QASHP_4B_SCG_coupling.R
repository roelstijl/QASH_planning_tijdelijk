#Koppelingen SCG
#Created by: Jacco Heres, Alliander
#In this script, the data of the project Asset Health Analytics is agregated to meet the requirements of Smart Cable Guard 
SCG_combine <- function(){
#Load data
load(paste0(settings$Analyse_Datasets,"/3. MVA output/MVA_Final_MSmoffenP_faalgestoordAsset_th0.3.Rda"))
MSmoffen <- fullSet
MSmoffen <- freqfromprob(MSmoffen)
load(paste0(settings$Analyse_Datasets,"/3. MVA output/MVA_Final_MSkabelsP_faalgestoordAsset_th0.3.Rda"))
MSkabels <- fullSet
MSkabels <- freqfromprob(MSkabels)
rm(fullSet)

activeSetMof    <- MSmoffen[Status_ID=="Active",] # filter naar alleen actieve moffen
activeSetKabel  <- MSkabels[Status_ID=="Active",] # filter naar alleen actieve kabels

load(paste0(settings$Ruwe_Datasets,"/6. NOR/ELCVERBINDINGSKNOOPPUNTEN_1501.Rda")) #Inladen moffendata met statussen
NOR_Mof <- mindataset
load(paste0(settings$Ruwe_Datasets,"/6. NOR/ELCVERBINDINGSDELEN_1501.Rda")) #Inladen kabeldata met statussen
NOR_Kabel <- mindataset
rm(mindataset)

#Moffen
setkey(NOR_Mof,ID_NAN); setkey(activeSetMof,ID_NAN)

activeSetMof[,Status:=NULL]
activeSetMof <- NOR_Mof[,c("ID_NAN","Status"),with=F][activeSetMof]
activeSetMof <- activeSetMof[Status==c("In Bedrijf"),]

setnames(activeSetMof, "ID_Verbinding","ID_Kabel")
setkey(activeSetMof, "ID_Kabel"); setkey(NOR_Kabel, "ID_Kabel")
activeSetMof <- NOR_Kabel[,c("ID_Kabel","ID_Verbinding"),with=F][activeSetMof]
activeSetMof[is.na(ID_Verbinding)]$ID_Verbinding <- activeSetMof[is.na(ID_Verbinding)]$ID_Verbinding_present_BAR
#activeSetMof[ID_Verbinding==""]$ID_Verbinding <- activeSetMof[ID_Verbinding==""]$ID_Verbinding_present_BAR

system.time(HLDsetMof <- activeSetMof[,list(sum(f_faalVoorspelgestoordAsset_th0.3),
                          length(ID_unique),
                          sum(Constructie=="Overig" | Constructie=="Onbekend" ),
                          sum(Constructie=="Nekaldietmof"),
                          sum(Constructie=="Oliemof"),
                          sum(Constructie=="Krimpmof Raychem"),
                          sum(Constructie=="Krimpmof Cellpack"),
                          sum(Constructie=="Krimpmof overig"),
                          sum(Constructie=="Gietharsmof"),
                          sum(Constructie=="Wikkelmof Cellpack"),
                          sum(Constructie=="Wikkelmof Filoform"),
                          sum(Constructie=="Wikkelmof overig")
                          ), by=ID_Verbinding])
  setnames(HLDsetMof,"V1", "Faalfrequentie")
  setnames(HLDsetMof,"V2", "Aantal moffen")
  setnames(HLDsetMof,"V3", "Aantal onbekende/overige moffen")
  setnames(HLDsetMof,"V4", "Aantal nekaldietmoffen")
  setnames(HLDsetMof,"V5", "Aantal oliemoffen")
  setnames(HLDsetMof,"V6", "Aantal krimpmoffen Raychem")
  setnames(HLDsetMof,"V7", "Aantal krimpmoffen Cellpack")
  setnames(HLDsetMof,"V8", "Aantal krimpmoffen overig")
  setnames(HLDsetMof,"V9", "Aantal gietharsmoffen")
  setnames(HLDsetMof,"V10", "Aantal wikkelmof Cellpack")
  setnames(HLDsetMof,"V11", "Aantal wikkelmof Filoform")
  setnames(HLDsetMof,"V12", "Aantal wikkelmof overig")

setkey(HLDsetMof,"ID_Verbinding")

#Kabels
activeSetKabel <- activeSetKabel[Status==c("In Bedrijf"),]

system.time(HLDsetKabel <- activeSetKabel[,list(sum(f_faalVoorspelgestoordAsset_th0.3),
                                                sum(Lengte),
                                                sum(Lengte*(Isolatie=="GPLK"))/sum(Lengte)*100,
                                                sum(Lengte*(Isolatie=="XLPE"))/sum(Lengte)*100,
                                                sum(Lengte*(Isolatie=="Overig Kunststof"))/sum(Lengte)*100,
                                                sum(Lengte*(Isolatie=="Onbekend"|Isolatie=="Overig"))/sum(Lengte)*100
                            ), by=ID_Verbinding])

setnames(HLDsetKabel,"V1", "Faalfrequentie")
setnames(HLDsetKabel,"V2", "Lengte kabels")
setnames(HLDsetKabel,"V3", "Perc. GPLK")
setnames(HLDsetKabel,"V4", "Perc. XLPE")
setnames(HLDsetKabel,"V5", "Perc. Overig Kunststof")
setnames(HLDsetKabel,"V6", "Perc. Onbekend en Overig")

setkey(HLDsetKabel,"ID_Verbinding")

#Kalibratie
HLDsetMof[,Faalfrequentie := 2.5*Faalfrequentie] #ca 250 mofstoringen per jaar
HLDsetKabel[,Faalfrequentie := 0.5*Faalfrequentie] #ca 100 kabelstoringen per jaar

#samenvoegen
HLDsetSCG <- merge(HLDsetKabel,HLDsetMof,all=T,suffixes=c("Kabels","Moffen"))
HLDsetSCG[is.na(HLDsetSCG)] <- 0

setkey(MSkabels,ID_Verbinding)
HLDsetSCG<-unique(MSkabels[,c("ID_Verbinding","Routenaam_Present","PC_4","PC_6_van","Woonplaats","Gemeente","Provincie_Naam"),with=F][HLDsetSCG])
HLDsetSCG <- MSkabels[,max(HLDStoringenPerJaar),by=ID_Verbinding][HLDsetSCG]
setnames(HLDsetSCG, "V1", "HLDStoringenPerJaar")
HLDsetSCG$Faalfrequentie <- HLDsetSCG$FaalfrequentieKabels + HLDsetSCG$FaalfrequentieMoffen
sum(HLDsetSCG$FaalfrequentieMoffen)
sum(HLDsetSCG$FaalfrequentieKabels)

#Load Connection data
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"))
nettopo$EAN_koppel <- nettopo$EAN_koppel[!is.na(Coo_X),]
n_klant = nettopo$EAN_koppel[,length(ID_EAN),by=Routenaam_MS]
n_klant = n_klant[(!is.na(Routenaam_MS))&(Routenaam_MS!="")]
setnames(n_klant,"V1","Aantal Klanten")
setkey(n_klant,Routenaam_MS); setkey(HLDsetSCG,"Routenaam_Present")
HLDsetSCG <- n_klant[HLDsetSCG]

#opslaan
save(HLDsetSCG,file=paste0(settings$Results,"/3. MVA output/Faalfrequenties_SCG.Rda"))
write.csv(HLDsetSCG,file=paste0(settings$Results,"/3. MVA output/Faalfrequenties_SCG.csv"))
}

# source('//fspaka02/userdata5b$/AL5696/Documents/Asset Health Analytics/Klad Rscripts/SCG dataset.R')
# MSkabels_BAR <- mindataset 
# load("C:/Data/AHAdata/1. Ruwe Datasets/1. BARlog/MH_NRG_MS_KABELS_Geospatial.Rda")
# MSkabels_GEO <- mindataset 
