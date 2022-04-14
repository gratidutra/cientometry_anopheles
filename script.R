library(tidyverse)
library(bibliometrix)
library(plotly)

# read and convert references -------------------------------------------------------------------
df <-
  convert2df("savedrecs.bib",
    dbsource = "wos",
    format = "bibtex"
  )

results <- biblioAnalysis(df)

S <- summary(object = results, k = 10, pause = FALSE)

results$Years[results$Years == 0] <- NA

plot(x = results, k =20)

# Create a country collaboration network -----------------------------------------------------------------------

M <- 
  metaTagExtraction(df, Field = "AU_CO", sep = ";")
NetMatrix <- 
  biblioNetwork(M, 
                analysis = "collaboration", 
                network = "countries", 
                sep = ";")

# Plot the network
net <- 
  networkPlot(NetMatrix,
  n = dim(NetMatrix)[1],
  Title = "Country Collaboration",
  type = "circle", size = TRUE, remove.multiple = FALSE, labelsize = 0.8
)


# Create a co-citation network don't exists in data-----------------------------------------------------------------------


NetMatrix <- 
  biblioNetwork(df,
  analysis = "co-citation",
  network = "references",
  sep = ";"
)

# Plot the network
net <- 
  networkPlot(NetMatrix,
  n = 50,
  Title = "Co-Citation Network",
  size = T,
  remove.multiple = FALSE, 
  labelsize = 0.7, edgesize = 5
)


# Create keyword co-occurrences network -----------------------------------------------------------------------

NetMatrix <- 
  biblioNetwork(
    df,
  analysis = "co-occurrences",
  network = "keywords", sep = ";"
)

# Plot the network
net <- 
  networkPlot(NetMatrix,
  normalize = "association",
  weighted = T, n = 30,
  Title = "Keyword Co-occurrences",
  type = "auto",
  size = T,
  edgesize = 5,
  labelsize = 0.7
)


# Journal (Source) co-citation analysis -----------------------------------

M=metaTagExtraction(df,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-citation", 
                           network = "sources", sep = ";")
net=networkPlot(NetMatrix,
                n = 50, 
                Title = "Co-Citation Network", 
                type = "kamada", 
                size.cex=TRUE, 
                size=15, 
                remove.multiple=FALSE, 
                labelsize=1,
                edgesize = 10, 
                edges.min=5)



# Histograph --------------------------------------------------------------

histResults <- histNetwork(M, sep = ";")

net <- histPlot(histResults, n=20, size = 5, labelsize = 4)


# Co-word  ----------------------------------------------------------------

NetMatrix <- 
  biblioNetwork(df, analysis = "co-occurrences", network = "keywords", sep = ";")

net <-
  networkPlot(NetMatrix, 
                normalize="association", 
                n = 50, Title = "Keyword Co-occurrences", 
                type = "fruchterman", 
                size.cex=TRUE, size=20, 
                remove.multiple=F, 
                edgesize = 10, 
                labelsize=5,
                label.cex=TRUE,label.n=30,edges.min=2)


# Conceptual Structure using keywords (method="CA") -----------------------------------------------------------------------

CS <-
  conceptualStructure(df,
    field = "ID",
    method = "MDS",
    minDegree = 4,
    k.max = 8,
    stemming = FALSE,
    labelsize = 10,
    documents = 10
  )
