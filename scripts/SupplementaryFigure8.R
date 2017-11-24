library(DESeq2)
library(countsimQC)

load("../data/SupplementaryFigure8.RData")

countsimQCReport(ddsList = countsimExample, 
                 outputFile = "SupplementaryFigure8.html", 
                 outputDir = "../pdfs", 
                 savePlots = TRUE,
                 description = "This is a comparison of three count data sets.")
