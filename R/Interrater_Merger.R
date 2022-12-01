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
#' @param Uniform_Output To specify if the user wish to export files containing interrater differences into the first rater's folder, TRUE, or in duplicates downstream of folders that contain raters annotation separately, FALSE. <Default: TRUE>
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
                              IR_Directory, IR_Keyword=FALSE,IR_Type=".txt", IR_Deliminator="\t",IR="",
                              Artifact, Artifact_Action=TRUE,
                              Uniform_Output=TRUE,...){

  if(!require("stringdist",character.only = TRUE)) stop("Package 'stringdist' not found")
  if(!require("reshape2",character.only = TRUE)) stop("Package 'reshape2' not found")

  Importer(Rater1_Directory,file_type = R1_type,Keyword = R1_Keyword,Deliminator = "\t",inList = TRUE,Meta_List_Name = R1_List)
  Importer(Rater2_Directory,file_type = R2_type,Keyword = R2_Keyword,Deliminator = "\t",inList = TRUE,Meta_List_Name = R2_List)


  # Create Directory for Check Point Results
  Directory <- paste0(Rater1_Directory,"/Check-point/",Sys.Date())
  if(dir.exists(Directory)==FALSE) dir.create(Directory)


  df <- data.frame(matrix(nrow=length(R2_List),ncol=1))
  names(df) <- "ID"





  #################################  Check  Column Names   ######################################


  ### R1 ------------------------------------------
  R1_List_Name <- names(R1_List)
  R1_List_Name <- unlist(lapply(R1_List_Name,function(x){gsub(R1_Keyword,replacement = "",x)}))

  ### R2 ------------------------------------------
  R2_List_Name <- names(R2_List)
  R2_List_Name <- unlist(lapply(R2_List_Name,function(x){gsub(R2_Keyword,replacement = "",x)}))


  if(any(!cOnset=="Onset"|!cDuration=="Duration"|!cAnnotation=="Annotation")){

    R2_List <- lapply(R2_List,function(x){
      if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
      if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
      if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
      x
    })
    names(R2_List) <- R2_List_Name

    R1_List <- lapply(R1_List,function(x){
      if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
      if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
      if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
      x
    })
    names(R1_List) <- R1_List_Name
  }


  List_Name <- R1_List_Name[R1_List_Name %in% R2_List_Name]


  #################################  Check  Stage Annotations   ######################################
  ### R1 ------------------------------------------
  names(SF_List) <- SF_List_Name
  df <- data.frame(matrix(nrow=length(SF_List),ncol=1))
  names(df) <- "ID"
  df$ID <- SF_List_Name
  df$SF_List <- unlist(lapply(SF_List,nrow))



  ### R2 ------------------------------------------
  names(AF_List) <- AF_List_Name
  df2 <- data.frame(matrix(nrow=length(AF_List),ncol=1))
  names(df2) <- "ID"
  df2$ID <- AF_List_Name
  df2$AF_List <- unlist(lapply(AF_List_Stage,nrow))


  ### Warning Missing Annotations ------------------------------------------
  df <- merge.data.frame(df,df2,by = "ID",all=TRUE)
  df <- df[df$ID %in% List_Name,]
  if(!df$R2_List == df$R1_List) {
    warning("The following files do not have the same numbers of epoches...")

    lapply(df$ID[!df$R2_List == df$R1_List],print)
    warning("Please, use the function 'Annotation_Checker' to check for missing or misplaced annotations." ,'\n',
            "The process will continue.")
  }


  #################################  Compare Annotations   ######################################
  for(i in List_Name){

    df <- merge.data.frame(R1_List[[x]],R2_List[[x]],by=c("Onset","Duration"),all=TRUE)

    ### Check Rater Initials ------------------------
    if(!all(c(R1=="",R2==""))){
      df$Annotation.1 <- gsub(R1,"", df$Annotation.x)
      df$Annotation.2 <- gsub(R2,"", df$Annotation.y)
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
      Final<- reshape2::melt(dK[c("Onset","Duration","Annotation.x","Annotation.y","Artifact")],id=c("Onset","Duration"))
    } else {
      Final <- reshape2::melt(dK[c("Onset","Duration","Annotation.x","Annotation.y")],id=c("Onset","Duration"))
    }


    #################################  Generate Report   ######################################
    ### Reintroduce Rater Initials ------------------------
    if(!all(c(R1=="",R2==""))){

      Final$Time <- as.POSIXct("00:00:00", format = "%H:%M:%S") + Final$Onset

    }


    ### Check Directory ------------------------
    if(dir.exists(paste0(Rater1_Directory,"/Divergence/"))) dir.create(paste0(Rater1_Directory,"/Divergence/"))
    if(isFALSE(Uniform_Output)){
      if(dir.exists(paste0(Rater2_Directory,"/Divergence/"))) dir.create(paste0(Rater2_Directory,"/Divergence/"))
    }


    ### Export Report ------------------------
    if(exists("Final")){
      if(nrow(Final)>0){
        write.table(Final,paste0(Rater1_Directory,"/Divergence/",i,"_",R1,"_interrater.txt"),sep="\t",quote = FALSE,row.names = FALSE)
        if(isFALSE(Uniform_Output)) write.table(Final,paste0(Rater2_Directory,"/Divergence/",i,"_",R2,"_interrater.txt"),sep="\t",quote = FALSE,row.names = FALSE)
      }
    }

  }
}
