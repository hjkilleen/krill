library(superheat)

ic <- data.frame(ep = interannualCoefficients[[2]][,2],
                 ts = interannualCoefficients[[3]][,2],
                 nd = interannualCoefficients[[4]][,2],
                 p = interannualCoefficients[[1]][,2])
row.names(ic) <- interannualCoefficients[[1]][,1]
icMat <- as.matrix(ic)

superheat(icMat, 
          heat.pal = c("#b35806", "white", "#542788"), 
          X.text = round(icMat, 3), 
          heat.na.col = "black",
          row.title = "Predictor",
          row.title.size = 10)
