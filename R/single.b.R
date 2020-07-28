
# This file is a generated template, your changes will not be overwritten

singleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "singleClass",
    inherit = singleBase,
    private = list(
      .run = function() {
        
        if (is.null(self$options$asser)) return()
        
        else if (length(self$options$asser) < self$options$nFactors)
          self$results$dimdesc$setError("The number of factors is too low")
        
        else{
          
          dataasser=data.frame(self$data[,self$options$asser])
          colnames(dataasser)=self$options$asser
          
          dataquantisup=data.frame(self$data[,self$options$quantisup])
          colnames(dataquantisup)=self$options$quantisup 
          
          dataqualisup=data.frame(self$data[,self$options$qualisup])
          colnames(dataqualisup)=self$options$qualisup
          
          data=data.frame(dataasser,dataquantisup,dataqualisup)
          
          if (is.null(self$options$individus)==FALSE) {
            rownames(data)=self$data[[self$options$individus]]
          }
          else
            rownames(data)=c(1:nrow(data))
          
          res.mca=private$.MCA(data)
          
          dimdesc=private$.dimdesc(res.mca)
          self$results$dimdesc$setContent(dimdesc)
          
          private$.printeigenTable(res.mca)
          
          imageindiv=self$results$plotindiv
          imageindiv$setState(res.mca)
          
          imagevar=self$results$plotvar
          imagevar$setState(res.mca)
          
          imagequantisup=self$results$plotquantisup
          imagequantisup$setState(res.mca)
          
          self$results$plotmod$setState(res.mca)
          
          self$results$ploteigen$setState(res.mca)
          
          data.distri=t(dataasser[1,])
          distri=data.frame(table(data.distri))
          colnames(distri)=c("modality", "quantity")
          imagedistri=self$results$distribution
          imagedistri$setState(distri)
          
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
      
      .MCA = function(data) {
        
        asser_gui=self$options$asser
        quantisup_gui=self$options$quantisup
        qualisup_gui=self$options$qualisup
        nFactors_gui=self$options$nFactors
        ventil=self$options$ventil
        
        if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui)== TRUE) {
          FactoMineR::MCA(data, quanti.sup=(length(asser_gui)+1):(length(asser_gui)+length(quantisup_gui)), ncp=nFactors_gui, level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui)==TRUE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::MCA(data, quali.sup=(length(asser_gui)+1):(length(asser_gui)+length(qualisup_gui)), ncp=nFactors_gui, level.ventil=ventil, graph=FALSE)
        }
        else if (is.null(quantisup_gui) == FALSE && is.null(qualisup_gui) == FALSE) {
          FactoMineR::MCA(data, quanti.sup=(length(asser_gui)+1):(length(asser_gui)+length(quantisup_gui)), quali.sup=(length(asser_gui)+length(quantisup_gui)+1):(length(asser_gui)+length(quantisup_gui)+length(qualisup_gui)), ncp=nFactors_gui, level.ventil=ventil, graph=FALSE)
        }
        else {
          FactoMineR::MCA(data, ncp=nFactors_gui, level.ventil=ventil, graph=FALSE)
        }
      },
      
      .printeigenTable = function(table){
        
        nFactors=self$options$nFactors
        
        for (i in 1:nFactors){
          self$results$eigengroup$eigen$addRow(rowKey=i, values=list(component=as.character(i))) 
        } #on cr?e les lignes du tableau, avec autant de facteurs qu'il y a de variables actives
        eigen=table$eig[,1]
        purcent=table$eig[,2]
        purcentcum=table$eig[,3]
        
        for (i in 1:nFactors) {
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
          quantisup = self$options$quantimod
          qualisup = self$options$varmodqualisup
          
          if (qualisup == TRUE && quantisup == TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var"), title="")
          else if (qualisup == TRUE && quantisup == FALSE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var", "quanti.sup"), title="")
          else if (qualisup == FALSE && quantisup == TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var", "quali.sup"), title="")
          else
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="ind", invisible=c("var","quali.sup", "quanti.sup"), title="")
          
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
          quantisup = self$options$quantimod
          qualisup = self$options$varmodqualisup
          
          
          if (qualisup == TRUE && quantisup == TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", invisible=c("ind"), title="")
          else if (qualisup == TRUE && quantisup == FALSE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", invisible=c("ind", "quanti.sup"), title="")
          else if (qualisup == FALSE && quantisup == TRUE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", invisible=c("ind", "quali.sup"), title="")
          else
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="var", invisible=c("ind","quali.sup", "quanti.sup"), title="")
          
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
          
          if (is.null(self$options$quantisup)==FALSE)
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="quanti.sup", title="")
          else
            plot=plot.MCA(res.mca, axes=c(abs, ord), choix="quanti.sup", title="")
          
          print(plot)
          TRUE
        }
      }, 
      
      .ploteigen = function(image, ...){
        
        if (is.null(self$options$asser)) return()
        
        else {
          res.mca=image$state
          nFactors=self$options$nFactors
          
          plot = factoextra::fviz_eig(res.mca, ncp = nFactors, addlabels = TRUE, main="")
          print(plot)
          TRUE
        }
        
      },
      
      .distribution = function(image, ...){
        if (is.null(self$options$asser)) return()
        
        else{
          distri=image$state
          plot=ggplot(distri, aes(x=modality, y=quantity))+geom_bar(stat="identity")+geom_text(aes(label=quantity), vjust=1.6, color="white", size=3.5)
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
          
          plot = FactoMineR::plot.MCA(res.mca, axes = c(abs_gui, ord_gui), title=paste("Modality",modality_gui), selectMod=selec, invisible = "ind")
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
        
        if (is.null(self$options$asser)) return()
        
        else {
          res.clust=image$state
          
          plot = plot.HCPC(res.clust, choice="tree")
          print(plot)
          TRUE
        }
      }
    )
)
