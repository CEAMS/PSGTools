#' A Function to Automatically Merge the Revised Annotations after Interrater Revision.
#'
#' This function will loop over revised annotations after interrater difference comparisons and merge them with the original annotation. These differences will be extracted and stored in a new folders downstream to the output directory assigned.
#' *** It is recommended that raters assign their initials following an underscore after
#'
#' @import reshape2 stringdist
#' @param Rater1_Directory Directory of where the annotations were made by the first rater t <e.g. "C:/Users/__YOUR USERNAME__/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param R1_Keyword Specify keywords for the annotation files. Make sure to provide the maximum patterns shared among files.
#' @param R1_Type The type of file to import. Currently, supports  ".tsv. <default: ".txt">
#' @param R1_Deliminator If the data is stored in .txt file and the deliminator is not TAB as default, please, specify it. <default: "\t">
#' @param R1 The initial of rater 1.
#' @param Revised_Directory Directory of where the annotations were made by the second rater <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param RA_Keyword Specify keywords for the stage files. Make sure to provide the maximum patterns shared among files.
#' @param RA_Type The type of file to import. Currently, supports  ".tsv . <default: ".txt">
#' @param RA_Deliminator If the data is stored in .txt file and the deliminator is not TAB as default, please, specify it. <default: "\t">
#' @param RA The initial of rater 2.
#' @param Artifact A value serves as artifact labeling
#' @param Artifact_Action A logical parameter indicating the action used in case of artifact labeling between raters. If TRUE, artifacts mis-alignments will be treated as differences between raters. If FALSE, the non-artifact label will be used as the annotation for the mini-epoch. When FALSE was passed to the function, it will generate a new column called 'Artifacts'. <Default: "TRUE">
#' @return Export interrater differences for each annotation file for further revision
#' @keywords Merge Merger merge merger Interrater interrater Annotation
#' @export
#' @examples
#'
#' Interrater_Merger(Rater1_Directory,R1_Type=".txt", R1_Deliminator="\t",R1="",
#'                   IR_Directory,IR_Type=".txt", IR_Deliminator="\t",IR="")
#'
#'
#'
#'



Interrater_Merger <- function(Rater1_Directory, R1_Keyword=FALSE,R1_Type=".txt", R1_Deliminator="\t",R1="",
                              Revised_Directory, RA_Keyword=FALSE,RA_Type=".txt", RA_Deliminator="\t",RA="",
                              Artifact, Artifact_Action=TRUE,
                              ...){

  if(!require("stringdist",character.only = TRUE)) stop("Package 'stringdist' not found")
  if(!require("reshape2",character.only = TRUE)) stop("Package 'reshape2' not found")

  Importer(Rater1_Directory,file_type = R1_type,Keyword = R1_Keyword,Deliminator = R1_Deliminator,inList = TRUE,Meta_List_Name = R1_List)
  Importer(Revised_Directory,file_type = RA_type,Keyword = RA_Keyword,Deliminator = RA_Deliminator,inList = TRUE,Meta_List_Name = RA_List)


  ### Check Directory ------------------------
  if(dir.exists(paste0(Rater1_Directory,"/Divergence/"))) dir.create(paste0(Rater1_Directory,"/Merge/"))







  #################################  Check  Column Names   ######################################


  ### R1 ------------------------------------------
  R1_List_Name <- names(R1_List)
  R1_List_Name <- unlist(lapply(R1_List_Name,function(x){gsub(R1_Keyword,replacement = "",x)}))

  ### RA ------------------------------------------
  RA_List_Name <- names(RA_List)
  RA_List_Name <- unlist(lapply(RA_List_Name,function(x){gsub(RA_Keyword,replacement = "",x)}))


  if(any(!cOnset=="Onset"|!cDuration=="Duration"|!cAnnotation=="Annotation")){

    R1_List <- lapply(R1_List,function(x){
      if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
      if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
      if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
      x
    })
    names(R1_List) <- R1_List_Name


    RA_List <- lapply(RA_List,function(x){
      if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
      if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
      if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
      x
    })
    names(RA_List) <- RA_List_Name
  }


  List_Name <- R1_List_Name[R1_List_Name %in% RA_List_Name]



  #################################  Compare Annotations   ######################################
  for(i in List_Name){

    df <- merge.data.frame(R1_List[[x]],RA_List[[x]],by=c("Onset","Duration"),all=TRUE)

    ### Check Rater Initials ------------------------
    if(!all(c(R1=="",RA==""))){
      df$Annotation.1 <- gsub(R1,"", df$Annotation.x)
      df$Annotation.2 <- gsub(RA,"", df$Annotation.y)
    } else {
      df$Annotation.1 <- df$Annotation.x
      df$Annotation.2 <- df$Annotation.y
      df$Annotation.x <- paste0(df$Annotation.x,"_R1")
      df$Annotation.y <- paste0(df$Annotation.y,"_R2")
    }


    ### Artifacts Action ------------------------
    if(isFALSE(Artifact_Action)){
      df$Artifact <- ifelse(df$Annotation.1==Artifact|df$Annotation.2==Artifact,Artifact,"")
      df$Annotation.1[df$Annotation.1==Artifact] <- df$Annotation.2[df$Annotation.1==Artifact]
      df$Annotation.2[df$Annotation.2==Artifact] <- df$Annotation.1[df$Annotation.2==Artifact]
    }



    ### Compare Annotations ---------------------------------
    df$Note <- ifelse(!df$Annotation.1==df$Annotation.2,"FPFN","")
    dk <- df[df$Note =="FPFN",]

    if(isFALSE(Artifact_Action)){
      Final<- reshape2::melt(dk[c("Onset","Duration","Annotation.x","Annotation.y","Artifact")],id=c("Onset","Duration"))
    } else {
      Final <- reshape2::melt(dk[c("Onset","Duration","Annotation.x","Annotation.y")],id=c("Onset","Duration"))
    }


    #################################  Generate Report   ######################################
    ### Reintroduce Rater Initials ------------------------
    if(!all(c(R1=="",R2==""))){

      Final$Time <- as.POSIXct("00:00:00", format = "%H:%M:%S") + Final$Onset

    }


    ### Check Directory ------------------------
    if(dir.exists(paste0(Rater1_Directory,"/Divergence/"))) dir.create(paste0(Rater1_Directory,"/Divergence/"))
    if(isFALSE(Uniform_Output)){
      if(dir.exists(paste0(Revised_Directory,"/Divergence/"))) dir.create(paste0(Revised_Directory,"/Divergence/"))
    }


    ### Export Report ------------------------
    if(exists("Final")){
      if(nrow(Final)>0){
        write.table(Final,paste0(Rater1_Directory,"/Divergence/",i,"_",R1,"_interrater.txt"),sep="\t",quote = FALSE,row.names = FALSE)
        if(isFALSE(Uniform_Output)) write.table(Final,paste0(Revised_Directory,"/Divergence/",i,"_",R2,"_interrater.txt"),sep="\t",quote = FALSE,row.names = FALSE)
      }
    }

  }
}
