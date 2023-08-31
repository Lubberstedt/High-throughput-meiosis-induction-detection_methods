
#How to analyze one data file (1 tube) for GFP,RFP,WT(non-fluorescing),and both(GFP/RFP)

# How to use: Replace {Variable#} with a biologically relevant name given each sample

## i.e. {Variable1}=read.csv('{SAMPLE}.csv')  to GFP=read.csv("GFP.csv") - this corresponds with the name provided for .fcs to .csv conversion


#Load  Libraries
###################################################################
library("ggplot2")
library("dplyr")
library(ggpubr)

#Load CSV as variables
#####################################################################
#Be sure to set working directory to file where raw data is
{Variable1}=read.csv('{SAMPLE}.csv')

#Remove Cell Number column

{Variable2}={Variable1}[-c (1)] 

#Isolate "BV605.A", "BV510.A","FSC.H","SSC.H","FITC.A","PE.CF594.A" columns for thresholding
{Variable3}={Variable2}[,c("BV605.A", "BV510.A","FSC.H","SSC.H","FITC.A","PE.CF594.A")] 

# Threshold based on RFP and GFP values and quality values. Note~ will be different for each flow cytometer

{Variable3}_GFP=dplyr::filter({Variable3}, {Variable3}$FITC.A > 560, {Variable3}$FITC.A <10000, #Trying to get rid of outliers 
                   {Variable3}$PE.CF594.A <1000,
                   {Variable3}$FSC.H>55000, 
                   {Variable3}$FSC.H<200000,
                   {Variable3}$SSC.H>450,
                   {Variable3}$SSC.H<3500,
                   {Variable3}$BV605.A >800,
                   {Variable3}$BV510.A >800)
{Variable4}<- ggplot({Variable3}_GFP, aes(x=(FITC.A), y=(PE.CF594.A))) +geom_point()
{Variable4
  
{Variable3}_WT=dplyr::filter({Variable3}, {Variable3}$FITC.A < 560, 
                               {Variable3}$PE.CF594.A <1000,
                               {Variable3}$FSC.H>55000, 
                               {Variable3}$FSC.H<200000,
                               {Variable3}$SSC.H>450,
                               {Variable3}$SSC.H<3500,
                               {Variable3}$BV605.A >800,
                               {Variable3}$BV510.A >800)
{Variable5}<- ggplot({Variable3}_WT, aes(x=(FITC.A), y=(PE.CF594.A))) +geom_point()
 {Variable5}
  
  

{Variable3}_RFP=dplyr::filter({Variable3}, {Variable3}$FITC.A < 560, 
                                {Variable3}$PE.CF594.A >1000,{Variable3}$PE.CF594.A >10000,#Trying to get rid of outliers 
                                {Variable3}$FSC.H>55000, 
                                {Variable3}$FSC.H<200000,
                                {Variable3}$SSC.H>450,
                                {Variable3}$SSC.H<3500,
                                {Variable3}$BV605.A >800,
                                {Variable3}$BV510.A >800)
{Variable6}<- ggplot({Variable3}_RFP, aes(x=(FITC.A), y=(PE.CF594.A))) +geom_point()
{Variable6}
    

  {Variable3}_Both=dplyr::filter({Variable3}, {Variable3}$FITC.A > 560,  {Variable3}$FITC.A <10000, #Trying to get rid of outliers 
                               {Variable3}$PE.CF594.A >1000,{Variable3}$PE.CF594.A >10000,#Trying to get rid of outliers
                               {Variable3}$FSC.H>55000, 
                               {Variable3}$FSC.H<200000,
                               {Variable3}$SSC.H>450,
                               {Variable3}$SSC.H<3500,
                               {Variable3}$BV605.A >800,
                               {Variable3}$BV510.A >800)
  {Variable7}<- ggplot({Variable3}_WT, aes(x=(FITC.A), y=(PE.CF594.A))) +geom_point(color="Gold")
  {Variable7}
  

  #Summary and graph
  
  Summary{Variable3}=rbind(NROW({Variable3}_GFP),
                                     NROW({Variable3}_WT),
                                     NROW({Variable3}_RFP),
                                     NROW({Variable3}_Both))
  
  
  Summary{Variable3}
  
  
 {Variable3}_four={Variable3}_Both_test + geom_point(data={Variable3}_GFP, color="green") +geom_point(data={Variable3}_RFP, color="red") + geom_point(data={Variable3}_WT, color="black") +xlim(0,10000)+ylim(0,10000)+theme_classic(base_size = 22)
  
 {Variable3}_four
  
  