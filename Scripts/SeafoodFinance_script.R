# import shareholders x companies
Share<-read.delim("Shares.txt",h=T,dec=",")

# unpivot the data
library(reshape)
unpivotShare<-melt(Share, id = c("Shareholder"))

# remove the investors and companies with a total of 0 
# refactor the subset to avoid ghost levels
# and rename columns
unpivotShare<-subset(unpivotShare, unpivotShare$value!=0)
unpivotShare<-droplevels(unpivotShare)
names(unpivotShare)<-c("Shareholder", "Company.Code","Share")

# import companies and shareholders
Company<-read.delim("Companies.txt",h=T,dec=",")
Investor<-read.delim("Shareholders.txt",h=T,dec=",")

# merge the datasets by "Shareholder" and then by "Company_Code" 
dat<-merge(Investor, unpivotShare, "Shareholder")
dat<-merge(dat, Company, "Company.Code")
dat<-droplevels(dat)




############################################
## NETWORK ANALYSIS - UNIQUE SHAREHOLDER$ ##
#####################################################################################

# select data for network with unique shareholder
subdat<-dat[,c(2,7,6)]
subdat<-droplevels(subdat)

# subset for different value of % share threshold: >0%, >=5%, >=10%
subdat0<-subset(subdat, subdat$Share>0)
subdat0<-droplevels(subdat0)
subdat5<-subset(subdat, subdat$Share>=5)
subdat5<-droplevels(subdat5)
subdat10<-subset(subdat, subdat$Share>=10)
subdat10<-droplevels(subdat10)

# select data for network with unique shareholders for a given threshold
netdat<-subdat5
netdat<-subdat10

netdat<-droplevels(netdat)

# pivot the network data and transform 1st column into rown names
netdat<- cast(netdat, Shareholder ~ Company, sum) # unique
netdat<- data.frame(netdat[,-1], row.names=netdat[,1])

# convert to a matrix
library(igraph)
netdat<-as.matrix(netdat)

# create the igraph object based on the matrix
net <- graph_from_incidence_matrix(netdat) 

# Shareholder are red squares, Companies are blue circles
Shape.node <- c("square", "circle")[V(net)$type+1]
Size.node<-c(3,3)[V(net)$type+1]
V(net)$color <- c("#C1403D", "#36A9E1")[V(net)$type+1] 
V(net)$color <- c("#C1403D", "#9FD2F9")[V(net)$type+1] 

# Plot the network
par(mar=c(0,0,0,0))
plot.igraph(net, vertex.shape=Shape.node,vertex.size=Size.node,vertex.label=NA,
            edge.color="black", edge.width=0.5,vertex.frame.color="white")

# Network density
round(graph.density(net,loop=FALSE),3)

## Minimum Degree
deg <- degree(net)
min(deg)
max(deg)

# Plot the network with weighted nodes
plot.igraph(net, vertex.shape=Shape.node,vertex.size=deg,vertex.label=NA,
            edge.color="black", edge.width=0.5,vertex.frame.color="white")

# One-mode projections
par(mfrow=c(1,2))
net.bp <- bipartite.projection(net)
plot(net.bp$proj1, vertex.shape="circle",edge.color="black",
     vertex.size=deg[rownames(netdat)],vertex.label=NA,vertex.frame.color="white")
plot(net.bp$proj2, vertex.shape="square",edge.color="black",
     vertex.size=deg[colnames(netdat)]*2,vertex.label=NA,vertex.frame.color="white")
par(mfrow=c(1,1))

# Network density (of the one-mode projections)
round(graph.density(net.bp$proj1,loop=FALSE),3)
round(graph.density(net.bp$proj2,loop=FALSE),3)

## Degree (of the one-mode projections)
deg1 <- degree(net.bp$proj1, mode="all")
min(deg1)
max(deg1)

deg2 <- degree(net.bp$proj2, mode="all")
min(deg2)
max(deg2)

#########################
## INTERACTIVE NETWORK ##
#########################

library("visNetwork")
library("plyr") # needed to use mapvalues

V(net)$shape = mapvalues(V(net)$type, from = c(0, 1), to = c("square", "dot"))
V(net)$group = mapvalues(V(net)$type, from = c(0, 1), to = c("Shareholders", "Companies"))
V(net)$color = mapvalues(V(net)$type, from = c(0, 1), to = c("#C1403D", "#36A9E1"))
E(net)$color = "black" #color of the edges

legendNodes<- data.frame(label = c("Shareholder", "Company"),
                         shape = c( "square","dot"),
                         color = c("#C1403D", "#36A9E1"))

visIgraph(net, smooth = FALSE) %>% 
  visLegend(useGroups = FALSE, addNodes =legendNodes) %>%
  visOptions(selectedBy = "group", highlightNearest = TRUE, nodesIdSelection = TRUE)




############################################
## NETWORK ANALYSIS - PARENT SHAREHOLDER$ ##
#####################################################################################

# select data for network with parent shareholder
subdat<-dat[,c(3,7,6)]
subdat<-droplevels(subdat)

# subset for different value of % share threshold: >0%, >=5%, >=10%
subdat0<-subset(subdat, subdat$Share>0)
subdat0<-droplevels(subdat0)
subdat5<-subset(subdat, subdat$Share>=5)
subdat5<-droplevels(subdat5)
subdat10<-subset(subdat, subdat$Share>=10)
subdat10<-droplevels(subdat10)

# select data for network with parent shareholders for a given threshold
netdat<-subdat5
netdat<-subdat10

netdat<-droplevels(netdat)

# pivot the network data and transform 1st column into rown names
netdat<- cast(netdat, Ultimate.Parent ~ Company, sum)
netdat<- data.frame(netdat[,-1], row.names=netdat[,1])

# convert to a matrix
library(igraph)
netdat<-as.matrix(netdat)

# create the igraph object based on the matrix
net <- graph_from_incidence_matrix(netdat) 

# Shareholder are red squares, Companies are blue circles
Shape.node <- c("square", "circle")[V(net)$type+1]
Size.node<-c(3,3)[V(net)$type+1]
V(net)$color <- c("#C1403D", "#36A9E1")[V(net)$type+1] 
V(net)$color <- c("#C1403D", "#9FD2F9")[V(net)$type+1] 

# Plot the network
par(mar=c(0,0,0,0))
plot.igraph(net, vertex.shape=Shape.node,vertex.size=Size.node,vertex.label=NA,
            edge.color="black", edge.width=0.5,vertex.frame.color="white")

# Network density
round(graph.density(net,loop=FALSE),3)

## Minimum Degree
deg <- degree(net)
min(deg)
max(deg)

# Plot the network with weighted nodes
plot.igraph(net, vertex.shape=Shape.node,vertex.size=deg,vertex.label=NA,
            edge.color="black", edge.width=0.5,vertex.frame.color="white")

# One-mode projections
par(mfrow=c(1,2))
net.bp <- bipartite.projection(net)
plot(net.bp$proj1, vertex.shape="circle",edge.color="black",
     vertex.size=deg[rownames(netdat)],vertex.label=NA,vertex.frame.color="white")
plot(net.bp$proj2, vertex.shape="square",edge.color="black",
     vertex.size=deg[colnames(netdat)]*2,vertex.label=NA,vertex.frame.color="white")
par(mfrow=c(1,1))

# Network density (of the one-mode projections)
round(graph.density(net.bp$proj1,loop=FALSE),3)
round(graph.density(net.bp$proj2,loop=FALSE),3)

## Degree (of the one-mode projections)
deg1 <- degree(net.bp$proj1, mode="all")
min(deg1)
max(deg1)

deg2 <- degree(net.bp$proj2, mode="all")
min(deg2)
max(deg2)

#########################
## INTERACTIVE NETWORK ##
#########################

library("visNetwork")
library("plyr") # needed to use mapvalues

V(net)$shape = mapvalues(V(net)$type, from = c(0, 1), to = c("square", "dot"))
V(net)$group = mapvalues(V(net)$type, from = c(0, 1), to = c("Shareholders", "Companies"))
V(net)$color = mapvalues(V(net)$type, from = c(0, 1), to = c("#C1403D", "#36A9E1"))
E(net)$color = "black" #color of the edges

legendNodes<- data.frame(label = c("Shareholder", "Company"),
                         shape = c( "square","dot"),
                         color = c("#C1403D", "#36A9E1"))

visIgraph(net, smooth = FALSE) %>% 
  visLegend(useGroups = FALSE, addNodes =legendNodes) %>%
  visOptions(selectedBy = "group", highlightNearest = TRUE, nodesIdSelection = TRUE)




############################
## DESCRIPTIVE STATISTICS ##
#####################################################################################

# investigate company and shareholder countries/regions
company_country<-data.frame(table(Company$Company.Country))
colnames(company_country)<-c("Company.Country","Frequency")
company_country$Percent<-round(company_country$Frequency/160*100,1)

company_region<-data.frame(table(Company$Company.Region))
colnames(company_region)<-c("Company.Region","Frequency")
company_region$Percent<-round(company_region$Frequency/160*100,1)

investor_region<-data.frame(table(Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/3250*100,1)

# investigate shareholder category
investor_category<-data.frame(table(Investor$Shareholder.Category))
colnames(investor_category)<-c("Shareholder.Category","Frequency")
investor_category$Percent<-round(investor_category$Frequency/3250*100,1)

# investigate company and shareholder countries/regions by subset
dat2<-subset(dat,dat$Company.Region=="Asia")
dat2<-droplevels(dat2)
dat2<-dat2[,c(2,4)]
dat2<-unique(dat2)
investor_region<-data.frame(table(dat2$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/1815*100,1)

dat2<-subset(dat,dat$Company.Region!="Asia")
dat2<-droplevels(dat2)
dat2<-dat2[,c(2,4)]
dat2<-unique(dat2)
investor_region<-data.frame(table(dat2$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/1858*100,1)

dat2<-subset(dat,dat$Company.Region=="Europe")
dat2<-droplevels(dat2)
dat2<-dat2[,c(2,4)]
dat2<-unique(dat2)
investor_region<-data.frame(table(dat2$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/1096*100,1)

dat2<-subset(dat,dat$Company.Region=="Africa")
dat2<-droplevels(dat2)
dat2<-dat2[,c(2,4)]
dat2<-unique(dat2)
investor_region<-data.frame(table(dat2$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/432*100,1)

# subset for different categories of shareholder and investigate the regions
sub_Investor<-subset (Investor, Investor$Shareholder.Category=="Financial investor")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/1832*100,1)

sub_Investor<-subset (Investor, Investor$Shareholder.Category=="Individual")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/732*100,1)

sub_Investor<-subset (Investor, Investor$Shareholder.Category=="Non-financial corporation")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/610*100,1)

sub_Investor<-subset (Investor, Investor$Shareholder.Category=="Foundation/Non-Profit")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/18*100,1)

sub_Investor<-subset (Investor, Investor$Shareholder.Category=="Government")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/12*100,1)

sub_Investor<-subset (Investor, Investor$Shareholder.Category=="-")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/46*100,1)




#######################################################
## DESCRIPTIVE STATISTICS FOR DIFFERENT SHARE VALUES ##
#####################################################################################

# subset the data
subdat<-dat[,c(1,7,2,6,5,4)]
subdat<-droplevels(subdat)

# subset for different value of % share threshold: >0%, >=5%, >=10%
subdat0<-subset(subdat, subdat$Share>0)
subdat0<-droplevels(subdat0)
subdat5<-subset(subdat, subdat$Share>=5)
subdat5<-droplevels(subdat5)
subdat10<-subset(subdat, subdat$Share>=10)
subdat10<-droplevels(subdat10)

# calculate the number of unique investors and companies for different thresholds
levels(subdat0$Shareholder) # 3250
levels(subdat5$Shareholder) # 305
levels(subdat10$Shareholder) # 166

levels(subdat0$Company) # 160
levels(subdat5$Company) # 137
levels(subdat10$Company) # 113

# investigate shareholder categories and regions for different thresholds
# >5%
subdat5_category<-subdat5[,c(3,5,6)]
subdat5_category<-droplevels(subdat5_category)
subdat5_category<-unique(subdat5_category)

investor_region<-data.frame(table(subdat5_category$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/305*100,1)

investor_category<-data.frame(table(subdat5_category$Shareholder.Category))
colnames(investor_category)<-c("Shareholder.Category","Frequency")
investor_category$Percent<-round(investor_category$Frequency/305*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="Financial investor")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/75*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="Individual")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/120*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="Non-financial corporation")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/87*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="Foundation/Non-Profit")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/5*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="Government")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/5*100,1)

sub_Investor<-subset (subdat5_category, subdat5_category$Shareholder.Category=="-")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/13*100,1)

# >10%
subdat10_category<-subdat10[,c(3,5,6)]
subdat10_category<-droplevels(subdat10_category)
subdat10_category<-unique(subdat10_category)

investor_region<-data.frame(table(subdat10_category$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/166*100,1)

investor_category<-data.frame(table(subdat10_category$Shareholder.Category))
colnames(investor_category)<-c("Shareholder.Category","Frequency")
investor_category$Percent<-round(investor_category$Frequency/166*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="Financial investor")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/28*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="Individual")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/77*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="Non-financial corporation")
sub_Investor<-droplevels(sub_Investor)
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/50*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="Foundation/Non-Profit")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/2*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="Government")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/3*100,1)

sub_Investor<-subset (subdat10_category, subdat10_category$Shareholder.Category=="-")
investor_region<-data.frame(table(sub_Investor$Shareholder.Region))
colnames(investor_region)<-c("Shareholder.Region","Frequency")
investor_region$Percent<-round(investor_region$Frequency/6*100,1)




###############################################
## MARKET CAPITALIZATION BY SHAREHOLDER TYPE ##
#######################################################################################

# import shareholders x companies by market capitalization and unpivot data
Market<-read.delim("MarketCap.txt",h=T,dec=",")
unpivotMarket<-melt(Market, id = c("Shareholder"))
unpivotMarket<-subset(unpivotMarket, unpivotMarket$value!=0)
unpivotMarket<-droplevels(unpivotMarket)
names(unpivotMarket)<-c("Shareholder", "Company.Code","Market")

# import shareholders
Investor<-read.delim("Shareholders.txt",h=T,dec=",")

# merge the datasets by "Shareholder"
dat<-merge(Investor, unpivotMarket, "Shareholder")

# subset to retain only financial investors, individuals and non-financial corporations
dat<-subset(dat, dat$Shareholder.Category=="Financial investor" | dat$Shareholder.Category=="Individual"| dat$Shareholder.Category=="Non-financial corporation")

# calculate the sum of market capitalization by shareholder type
library(plyr)
SUM<-ddply(dat, .(Shareholder.Category), summarize,  Result=sum(Market))

# divide by one billion for readibility 
SUM$Result<-SUM$Result/1000000000

# plot the barplot
ggplot(SUM,aes(x=Shareholder.Category, y=Result))+
  geom_bar(stat="identity")+
  labs(x="Shareholder type", y="Market capitalization (billion USD)")
