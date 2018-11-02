
devtools::install_github("danielhorn/SABeR")
library(SABeR)
devtools::load_all()
load("mainHiera.RData")

# Parameter main function
data = mainHiera
perfName = "ydist"
expParName = c("a","b","cc","d")
algoName = "confName"
replName = "i"
clusterFunction = "clusterFunctionKMeans"

# parameter plot function
h = 2
v = 3# diese Maassen finde ich bis jetzt am besten. Muss noch
# automatisiert werden
shape = "circle"# oder square / sphere usw fuer Form der vertexes
edge.width = 1
testNiveaus = c(1e-12, 1e-6, 1e-2, 0.1)


saber.result = saberIt(data, perfName, expParName, algoName, replName, 
                       clusterFunction)
plot(saber.result, testNiveaus = 0.05)
summarizeCluster(saber.result)
########
