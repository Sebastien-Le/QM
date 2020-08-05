
singleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "singleClass",
    inherit = singleBase,
    private = list(
      
      
      #### Init + run functions ----      
      .run = function(){

        ready <- TRUE
        if (is.null(self$options$asser) || length(self$options$asser) < 2){
          return()
          ready <- FALSE
        }
        private$.errorCheck()
        
        if (ready) {
          
          data <- private$.buildData()
          res.mca <- private$.compute(data)
          
          #1
          data.distri=t(data[1,1:length(self$options$asser)])
          distri=data.frame(table(data.distri))
          colnames(distri)=c("Categories", "Distribution")
          imagedistri=self$results$distribution
          imagedistri$setState(distri)
          #2
          private$.printeigenTable(res.mca)
          #3
          self$results$ploteigen$setState(res.mca) #Histogramme des v.p.
          #4
          imageindiv=self$results$plotindiv #Individus
          imageindiv$setState(res.mca)
          
          imagevar=self$results$plotvar #Assertions
          imagevar$setState(res.mca)
          
          self$results$plotmod$setState(res.mca) #Assertions au niveau des modalités
          
          imagequalisup=self$results$plotqualisup #Quali sup.
          imagequalisup$setState(res.mca)
          
          imagequantisup=self$results$plotquantisup #Quanti sup.
          imagequantisup$setState(res.mca)
          
          dimdesc=private$.dimdesc(res.mca)
          self$results$dimdesc$setContent(dimdesc)
          
          res.clust=HCPC(res.mca, nb.clust=self$options$nbclust, graph=FALSE)
          
          imageclust=self$results$plotclust
          imageclust$setState(res.clust)
          
          clust=private$.clust(res.clust)
          self$results$clust$setContent(clust)
        }
      },
      
      .dimdesc = function(table) {
        
        proba_gui=self$options$proba
        nFactors_gui=self$options$nFactors
        
        res=dimdesc(table, axes=1:nFactors_gui, proba = proba_gui)
        print(res[-length(res)])
        
      },
      
      #### Compute results ----      
      .compute = function(data) {
        
        asser_gui=self$options$asser
        quantisup_gui=self$options$quantisup
        qualisup_gui=self$options$qualisup
        ventil=self$options$ventil
        ventil=ventil/100
        
        if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE) {
          res = FactoMineR::MCA(data, quanti.sup=(length(asser_gui)+1):(length(asser_gui)+length(quantisup_gui)), level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE) {
          res = FactoMineR::MCA(data, quali.sup=(length(asser_gui)+1):(length(asser_gui)+length(qualisup_gui)), level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE) {
          res = FactoMineR::MCA(data, quanti.sup=(length(asser_gui)+1):(length(asser_gui)+length(quantisup_gui)), quali.sup=(length(asser_gui)+length(quantisup_gui)+1):(length(asser_gui)+length(quantisup_gui)+length(qualisup_gui)), level.ventil=ventil, graph=FALSE)
        }
        else {
          res = FactoMineR::MCA(data, level.ventil=ventil, graph=FALSE)
        }
      },

      .printeigenTable = function(table){
        
        nbdim=min(dim(table$eig)[1],10)
        
        for (i in 1:nbdim){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i))) 
        }
        eigen=table$eig[,1]
        purcent=table$eig[,2]
        purcentcum=table$eig[,3]
        
        for (i in 1:nbdim) {
          row=list()
          row[["eigenvalue"]]=eigen[i] #   ? chaque nom de colonne (eigenvalue, purcent et purcentcum)
          row[["purcent"]]=purcent[i] #    on associe
          row[["purcentcum"]]=purcentcum[i] #  une valeur des calculs pr?c?dents
          self$results$eigengroup$eigen$setRow(rowNo=i, values = row)
        }
      },
      
      .plotindiv = function(image, ...) {
        
        if (is.null(self$options$asser)) return()

        else {
          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          plot=FactoMineR::plot.MCA(res.mca, axes=c(abs, ord), choix="ind", 
                                    invisible=c("var","quali.sup", "quanti.sup"), title="Respondents According to their Perception of the Concept")
          print(plot)
          TRUE
        }
      },
      
      .plotvar = function(image, ...) {
        
        if (is.null(self$options$asser)) return()
        
        else {
          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          plot=FactoMineR::plot.MCA(res.mca, axes=c(abs, ord), choix="var", invisible=c("ind","quali.sup", "quanti.sup"), title="")
          print(plot)
          TRUE
        }
      },

      .plotqualisup = function(image, ...) {
        
        if (is.null(self$options$asser)) return()
        
        else {
          
          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          
          if (is.null(self$options$varmodqualisup)==FALSE)
            plot=FactoMineR::plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var","ind"), title="Supplementary Categories")
            print(plot)
            TRUE
        }
      }, 
      
      .plotquantisup = function(image, ...) {
        
        if (is.null(self$options$asser)) return()
        
        else {
          
          res.mca=image$state
          abs=self$options$abs
          ord=self$options$ord
          
          if (is.null(self$options$quantimod)==FALSE)
            plot=FactoMineR::plot.MCA(res.mca, axes=c(abs, ord), choix="quanti.sup", title="")
            print(plot)
            TRUE
        }
      }, 
      
      .ploteigen = function(image, ...){
        
        if (is.null(self$options$asser)) return()
        
        else {
          res.mca=image$state
          plot = factoextra::fviz_eig(res.mca, addlabels = TRUE, main="")
          print(plot)
          TRUE
        }
        
      },
      
      .distribution = function(image, ...){
        if (is.null(self$options$asser)) return()
        
        else{
          distri=image$state
          plot=ggplot(distri, aes(x=Categories, y=Distribution))+geom_bar(stat="identity")+geom_text(aes(label=Distribution), vjust=1.6, color="white", size=3.5)
          print(plot)
          TRUE
        }
      },
      
      .plotmod = function(image, ...){
        
        if (is.null(self$options$asser)) return()
        
        else {
          res.mca=image$state
          abs_gui=self$options$abs
          ord_gui=self$options$ord
          modality_gui=self$options$modality
          
          selec = c()
          for(i in colnames(res.mca$call$X)){
            selec=c(selec,paste(i, "_", modality_gui, sep=""))
          }
          plot = FactoMineR::plot.MCA(res.mca, axes = c(abs_gui, ord_gui), title=paste("Representation of the Statements Categories",modality_gui), selectMod=selec, invisible = c("ind","quali.sup"))
          print(plot)
          TRUE
        }
      },
      
      .clust = function(data) {
        
        dataclust=data$desc.var$category
        modclust=self$options$modclust
        
        for (i in 1:length(dataclust)) {
          for (j in 1:length(rownames(dataclust[[i]]))) {
            rownames(dataclust[[i]])[j] <- strsplit(rownames(dataclust[[i]])[j],"=")[[1]][2]
          }
        }

        if (is.null(modclust)) return()

        else if (modclust=="All") {
          print(dataclust)
        }

        else {
          for (i in 1:length(dataclust)) {
            dataclust[[i]] <- dataclust[[i]][stringr::str_ends(rownames(dataclust[[i]]),modclust),]
          }
          print(dataclust)
        }
      },
      
      .plotclust = function(image, ...) {
        
        if (is.null(self$options$asser) || length(self$options$asser) < 2) return()
        
        else {
          res.clust=image$state
          
          plot = FactoMineR::plot.HCPC(res.clust, choice="tree")
          print(plot)
          TRUE
        }
      },
      
      ### Helper functions ----
      .errorCheck = function() {
        if (length(self$options$asser) < 2)
          jmvcore::reject(jmvcore::format('The number of factors is too low'))
        
      },
    
      .buildData = function() {
      
        dataasser=data.frame(self$data[,self$options$asser])
        colnames(dataasser)=self$options$asser
        dataquantisup=data.frame(self$data[,self$options$quantisup])
        colnames(dataquantisup)=self$options$quantisup
        dataqualisup=data.frame(self$data[,self$options$qualisup])
        colnames(dataqualisup)=self$options$qualisup
        data=data.frame(dataasser,dataquantisup,dataqualisup)
        rownames(data)=c(1:nrow(data))
        
      return(data)
      }
    )
)
