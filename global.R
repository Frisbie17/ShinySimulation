
#Sets the path to the JaamSimJar file - update when moved to server.

JarFilePath <- "JaamSim2017-05.jar"

BaseCfgFilePath <- "MM1_exp_base.cfg"

OutputFilePath <- "MM1_exp_app.dat"

CfgOutputFilePath <- "MM1_exp_app.cfg"


#######################################################################################
###Function to make the specific configuration file for the MM1 example of
###the JaamSim Discrete Event Simulation Model.
###
###The base file contains everything but the user specified parameters.
###This function adds the user parameters to the bottom of the file and saves a new version
###as the user specified outputFileName.
#######################################################################################

makeCfg <- function(baseInputFile, outputFileName, meanIAT, meanST){
  
  outFile <- file(outputFileName, "w")
  for (i in baseInputFile){ 
    x <- readLines(i) 
    writeLines(x[1:(length(x))], outFile) 
  }
  
  writeLines(paste("ArrivalTimeExpDist Mean {  ", meanIAT, " s }"), outFile)
  writeLines(paste("ServerTimeExpDist Mean {  ", meanST, " s }"), outFile)

  close(outFile)  
  
}

####Function to run the JaamSim DES using a CLI call.
runJaamSim <- function(JarFilePath, CfgFilePath){
  
  SetCommand <- paste("java -jar ", JarFilePath, " ", CfgFilePath, " -h")
  
  system(SetCommand)
  
}

####Reads the configured output file and returns a list object with the mean queue length and mean queue time.
readJaamSimOutput <- function(outputFilePath){
  
  results.table <- test <- read.table(outputFilePath, header = FALSE, skip =2 , sep="\t")

  result.means <- colMeans(results.table)
  
  aveQueLength <- result.means[2]
  aveQueTime <- result.means[3]*60*60
  
  results <- c(aveQueLength, aveQueTime)
  
  return(results)
  
}  


#####L(q) AQL formula

closedAQL <- function(mIAT, mST){
  
  rho <- (1/mIAT)/(1/mST)
  
  Lq <- (rho^2)/(1-rho)
  
  return(Lq)
  
}

####W(q) AQT formula
closedAQT <- function(AQL, mIAT){
  
  Wq <- AQL/(1/mIAT)
  
  return(Wq)
}


