

top500ID <- read.csv("./CITIZEN SCIENCE/iNaturalistLU/paper iNat/iNatIDfierstop500.csv")
top500ID$Number <- as.numeric(gsub("\\,","", as.character(top500ID$Number)))
top500IDper <- as.numeric(top500ID$Number)/sum(inat2[,23]+inat2[,24])


totalIDfiers <- 7130 # accessed on site

### Focus on the number or % of IDfiers (decreasing number: cumulative sum)

# Top 500
500/totalIDfiers # 7% of IDfiers
sum(top500IDper)*100 # 89.2% of IDs

# Top 5% IDfiers
7130*0.05
sum(top500IDper[1:357])*100 # 84.4% of IDs

# Top 100 IDfiers
sum(top500IDper[1:100])*100 # 60.9 of IDs

# Top 1% IDfiers
7130*0.01
sum(top500IDper[1:71])*100 # 53.7% of IDs

# Top 0.1% IDfiers
sum(top500IDper[1:7])*100 # 15.6% of IDs

# Top 5 IDfiers
sum(top500IDper[1:5])*100 # 12.5% of IDs

# Top 3 IDfiers
sum(top500IDper[1:3])*100 # 8.5% of IDs

# First IDfier
top500IDper[1]*100 # 3.1% of IDs

### Focus on % of IDs then look at % of IDfiers (look at cumsum)

# 10% of IDs are made by the first 4 IDfiers 
4/totalIDfiers*100 # 0.056% of IDfiers

# 25% of IDs are made by the first 16 IDfiers
16/totalIDfiers*100 # 0.22% of IDfiers

# 50% of IDs are made by the first 60 IDfiers
60/totalIDfiers*100 # 0.84% of IDfiers

# 75% of IDs are made by the first 202 IDfiers
202/totalIDfiers*100 # 2.8% of IDfiers

### Plotting

# Plot showing the quickly saturating empirical "curve": most identifications are made by few people
plot(cumsum(top500IDper), type="s", xlim=c(1,7130), ylim=c(0:1), col="red")
abline(h=1)
abline(v=500, col="red", lty=5)
abline(h=0)

# Log scale like in the original paper
plot(cumsum(top500IDper), type="s", xlim=c(1,7130), ylim=c(0:1), col="red", log="x")
