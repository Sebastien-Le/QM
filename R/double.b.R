
# This file is a generated template, your changes will not be overwritten

doubleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "doubleClass",
    inherit = doubleBase,
    private = list(
      .run = function() {
        
        if (is.null(self$options$asser_g1) || is.null(self$options$asser_g2)) return()
        
        else if ((length(self$options$asser_g1) < self$options$nFactors) ||(length(self$options$asser_g2) < self$options$nFactors))
          self$results$dimdesc$setError("The number of factors is too low")
        
        else{
          
          dataasser1=data.frame(self$data[,self$options$asser_g1])
          colnames(dataasser1)=self$options$asser_g1
          
          dataquantisup1=data.frame(self$data[,self$options$quantisup_g1])
          colnames(dataquantisup1)=self$options$quantisup_g1
          
          dataqualisup1=data.frame(self$data[,self$options$qualisup_g1])
          colnames(dataqualisup1)=self$options$qualisup_g1
          
          dataasser2=data.frame(self$data[,self$options$asser_g2])
          colnames(dataasser2)=self$options$asser_g2
          
          dataquantisup2=data.frame(self$data[,self$options$quantisup_g2])
          colnames(dataquantisup2)=self$options$quantisup_g2
          
          dataqualisup2=data.frame(self$data[,self$options$qualisup_g2])
          colnames(dataqualisup2)=self$options$qualisup_g2
          
          data=data.frame(dataasser1,dataquantisup1,dataqualisup1,dataasser2,dataquantisup2,dataqualisup2)
          
          if (is.null(self$options$individus)==FALSE) {
            rownames(data)=self$data[[self$options$individus]]
          }
          else
            rownames(data)=c(1:nrow(data))
          
          res.mfa=private$.MFA(data)

          dimdesc=private$.dimdesc(res.mfa)
          self$results$dimdesc$setContent(dimdesc)

          private$.printeigenTable(res.mfa)
          
          imagegroup=self$results$plotgroup
          imagegroup$setState(res.mfa)

          imageindiv=self$results$plotindiv
          imageindiv$setState(res.mfa)

          self$results$ploteigen$setState(res.mfa)
          
          dataasser=data.frame(dataasser1,dataasser2,dataasser1,dataasser2)
          res.mfa.var=private$.varMFA(dataasser)
          
          coord=as.data.frame(res.mfa.var$group$coord.sup)
          coord$var=c(self$options$asser_g1,self$options$asser_g2)
          coord$group=c(rep("1", length(self$options$asser_g1)),rep("2", length(self$options$asser_g2)))
          
          imagevar=self$results$plotvar
          imagevar$setState(coord)
        }
      },
      
      .dimdesc = function(table) {
        
        proba_gui=self$options$proba
        nFactors_gui=self$options$nFactors
        
        res=dimdesc(table, axes=1:nFactors_gui, proba = proba_gui)
        print(res[-length(res)])
        
      },
      
      .varMFA = function(data) {
        
        asser_g1=self$options$asser_g1
        asser_g2=self$options$asser_g2
        nFactors=self$options$nFactors
        
        MFA(data, ncp=nFactors, group=c(length(asser_g1), length(asser_g2), rep(1,length(asser_g1)+length(asser_g2))), type=rep("n", 2+length(asser_g1)+length(asser_g2)), num.group.sup=c(3:(3+length(asser_g1)+length(asser_g2)-1)), graph=FALSE)
      },
      
      .MFA = function(data) {
        
        asser_g1=self$options$asser_g1
        quantisup_g1=self$options$quantisup_g1
        qualisup_g1=self$options$qualisup_g1
        asser_g2=self$options$asser_g2
        quantisup_g2=self$options$quantisup_g2
        qualisup_g2=self$options$qualisup_g2
        nFactors=self$options$nFactors
        
        
        if (is.null(quantisup_g1) == FALSE && is.null(qualisup_g1)== TRUE){
          if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2)== TRUE){
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(asser_g2), length(quantisup_g2)), type=c("n","s","n","s"), name.group=c("group1", "quanti.group1", "group2", "quanti.group2"), num.group.sup=c(2, 4), graph=FALSE)
          }
          else if (is.null(quantisup_g2)==TRUE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(asser_g2), length(qualisup_g2)), type=c("n","s","n","n"), name.group=c("group1", "quanti.group1", "group2", "quali.group2"), num.group.sup=c(2, 4), graph=FALSE)
          }
          else if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(asser_g2), length(quantisup_g2), length(qualisup_g2)), type=c("n","s","n","s","n"), name.group=c("group1", "quanti.group1", "group2", "quanti.group2", "quali.group2"), num.group.sup=c(2, 4, 5), graph=FALSE)
          }
          else {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(asser_g2)), type=c("n","s","n"), name.group=c("group1", "quanti.group1", "group2"), num.group.sup=c(2), graph=FALSE)
          }
        }
        else if (is.null(quantisup_g1)==TRUE && is.null(qualisup_g1) == FALSE) {
          chiffre=2
          if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2)== TRUE){
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(qualisup_g1), length(asser_g2), length(quantisup_g2)), type=c("n","n","n","s"), name.group=c("group1", "quali.group1", "group2", "quanti.group2"), num.group.sup=c(2, 4), graph=FALSE)
          }
          else if (is.null(quantisup_g2)==TRUE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(qualisup_g1), length(asser_g2), length(qualisup_g2)), type=c("n","n","n","n"), name.group=c("group1", "quali.group1", "group2", "quali.group2"), num.group.sup=c(2, 4), graph=FALSE)
          }
          else if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(qualisup_g1), length(asser_g2), length(quantisup_g2), length(qualisup_g2)), type=c("n","n","n","s","n"), name.group=c("group1", "quali.group1", "group2", "quanti.group2", "quali.group2"), num.group.sup=c(2, 4, 5), graph=FALSE)
          }
          else {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(qualisup_g1), length(asser_g2)), type=c("n","n","n"), name.group=c("group1", "quali.group1", "group2"), num.group.sup=c(2), graph=FALSE)
          }
        }
        else if (is.null(quantisup_g1) == FALSE && is.null(qualisup_g1) == FALSE) {
          if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2)== TRUE){
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(qualisup_g1), length(asser_g2), length(quantisup_g2)), type=c("n","s","n","n","s"), name.group=c("group1", "quanti.group1", "quali.group1", "group2", "quanti.group2"), num.group.sup=c(2, 3, 5), graph=FALSE)
          }
          else if (is.null(quantisup_g2)==TRUE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(qualisup_g1), length(asser_g2), length(qualisup_g2)), type=c("n","s","n","n","n"), name.group=c("group1", "quanti.group1", "quali.group1", "group2", "quali.group2"), num.group.sup=c(2, 3, 5), graph=FALSE)
          }
          else if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(qualisup_g1), length(asser_g2), length(quantisup_g2), length(qualisup_g2)), type=c("n","s","n","n","s","n"), name.group=c("group1", "quanti.group1", "quali.group1", "group2", "quanti.group2", "quali.group2"), num.group.sup=c(2, 3, 5, 6), graph=FALSE)
          }
          else {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(quantisup_g1), length(qualisup_g1), length(asser_g2)), type=c("n","s","n","n"), name.group=c("group1", "quanti.group1", "quali.group1", "group2"), num.group.sup=c(2, 3), graph=FALSE)
          }
        }
        else {
          if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2)== TRUE){
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(asser_g2), length(quantisup_g2)), type=c("n","n","s"), name.group=c("group1", "group2", "quanti.group2"), num.group.sup=c(3), graph=FALSE)
          }
          else if (is.null(quantisup_g2)==TRUE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(asser_g2), length(qualisup_g2)), type=c("n","n","n"), name.group=c("group1", "group2", "quali.group2"), num.group.sup=c(3), graph=FALSE)
          }
          else if (is.null(quantisup_g2) == FALSE && is.null(qualisup_g2) == FALSE) {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(asser_g2), length(quantisup_g2), length(qualisup_g2)), type=c("n","n","s","n"), name.group=c("group1", "group2", "quanti.group2", "quali.group2"), num.group.sup=c(3, 4), graph=FALSE)
          }
          else {
            MFA(data, ncp=nFactors, group=c(length(asser_g1), length(asser_g2)), type=c("n","n"), name.group=c("group1", "group2"), graph=FALSE)
          }
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
        
        if ((is.null(self$options$asser_g1)) || (is.null(self$options$asser_g2))) return()
        
        else {
          
          res.mfa=image$state
          abs=self$options$abs
          ord=self$options$ord
          partial=self$options$partial
          
          if (partial=="Biggest") {
            plot=plot.MFA(res.mfa, axes=c(abs, ord), choix="ind", title="", partial="all", invisible=c("quali","quali.sup"), select=names(sort(res.mfa$ind$within.inertia[,1], decreasing=TRUE)[1:5]))
          }
          else {
            plot=plot.MFA(res.mfa, axes=c(abs, ord), choix="ind", title="", partial="all", invisible=c("quali","quali.sup"), select=names(sort(res.mfa$ind$within.inertia[,1])[1:5]))
          }

          print(plot)
          TRUE
        }
      },
      
      .plotgroup = function(image, ...) {
        
        if ((is.null(self$options$asser_g1)) || (is.null(self$options$asser_g2))) return()
        
        else {
          
          res.mfa=image$state
          abs=self$options$abs
          ord=self$options$ord
          
          plot=plot.MFA(res.mfa, axes=c(abs, ord), choix="group", title="")
          print(plot)
          TRUE
        }
      },
      
      .plotvar = function(image, ...) {
        
        if ((is.null(self$options$asser_g1)) || (is.null(self$options$asser_g2))) return()
        
        else {
          coord=image$state
          abs=self$options$abs
          ord=self$options$ord
          
          df=cbind(coord[,c(abs,ord)],coord[,c("var", "group")])
          colnames(df)[1:2]=c("X","Y")
          
          plot=ggplot(df, aes(x=X, y=Y))+geom_point(aes(colour=group))+
            geom_text_repel(aes(label=var), hjust=0.5, vjust=-1)+
            labs(x=paste("Dim",abs), y=paste("Dim",ord))+
            theme(legend.title = element_text(colour = "steelblue",  face = "bold.italic"), 
                  legend.text = element_text(face = "italic", colour="steelblue4"), 
                  axis.title = element_text(size = (10), colour = "steelblue4"),
                  axis.text = element_text(colour = "cornflowerblue", size = (10)))
          print(plot)
          TRUE
        }
      },
      
      .ploteigen = function(image, ...){
        
        if ((is.null(self$options$asser_g1)) || (is.null(self$options$asser_g2))) return()
        
        else {
          res.mfa=image$state
          nFactors=self$options$nFactors
          
          plot = factoextra::fviz_eig(res.mfa, ncp = nFactors, addlabels = TRUE, main="")
          print(plot)
          TRUE
        }
        
      }
    )
)