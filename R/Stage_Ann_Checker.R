#' A Function to Automatically Check for Annotation Errors
#'
#' This function will loop over annotations in a folder checking for missing mini-epochs within a stage and misalignment in sleep stages.
#'
#' @import reshape2 stringdist
#' @param Annotation_Directory Directory of where the files that you want to import <e.g. "C:/Users/__YOUR USERNAME__/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param AF_Keyword Specify keywords for the annotation files. Make sure to provide the maximum patterns shared among files.
#' @param AF_Type The type of file to import. Currently, supports  ".tsv. <default: ".txt">
#' @param AF_Deliminator If the data is stored in .txt file and the deliminator is not TAB as default, please, specify it. <default: "\t">
#' @param Stage_Directory Directory of where the Stage files are stored  <e.g. "C:/Users/___YOUR USERNAME___/UPSTREAM FOLDER/.../FOLDER NAME/">
#' @param SF_Keyword Specify keywords for the stage files. Make sure to provide the maximum patterns shared among files.
#' @param SF_Type The type of file to import. Currently, supports  ".tsv . <default: ".txt">
#' @param SF_Deliminator If the data is stored in .txt file and the deliminator is not TAB as default, please, specify it. <default: "\t">
#' @param cOnset The name of the column that contains the onset time for each epoch in accumulated seconds in both annotation files and stage files <Default: "Onset">
#' @param cDuration The name of the column that contains durations for each epoches in both annotation files and stage files <Default: "Duration">
#' @param cAnnotation The name of the column that contains annotations for both annotation files and stage files <Default: "Annotation">
#' @param Arousal Specify the annotations indicating arousal <Default: "0">
#' @param N1 Specify the annotations indicating Stage 1 NREM <Default: "1">
#' @param N2 Specify the annotations indicating Stage 2 NREM <Default: "2">
#' @param N3 Specify the annotations indicating Stage 3 NREM <Default: "3">
#' @param N4 Specify the annotations indicating Stage 4 NREM <Default: "4">
#' @param REM Specify the annotations indicating Stage REM <Default: "5">
#' @param Unscored Specify the annotations indicating Unscored Stage  <Default: "9">
#' @param Stage_Duration Specify the duration of each stage  <Default: 30>
#' @param Epoch_Duration Specify the duration of the miniepochs  <Default: 3>
#' @param Miniepoch Specify which stages contain miniepoches  <Default: "5">
#' @param Target_Keyword Specify keywords for the error files  <Default: "_W>
#' @param ID_List a list of IDs that contains the file names
#' @return Import files into your working enviroment in R (i.e., Global Enviroment in R Studio)
#' @export
#'



Stage_Ann_Checker <- function(Annotation_Directory, AF_Keyword=FALSE,AF_Type=".txt", AF_Deliminator="\t",
                               Stage_Directory, SF_Keyword=FALSE,SF_Type=".txt", SF_Deliminator="\t",
                               cOnset="Onset", cDuration="Duration", cAnnotation="Annotation",
                               Arousal="0",N1="1",N2="2",N3="3",N4="4",REM="5",Unscored="9",Stage_Duration=30,Epoch_Duration=3,
                               Miniepoch="5",
                               Target_Keyword="_W",
                               ID_List,...){

if(!require("stringdist",character.only = TRUE)) stop("Package 'stringdist' not found")

  Importer(Annotation_Directory,file_type = AF_type,Keyword = AF_Keyword,Deliminator = "\t",inList = TRUE,Meta_List_Name = AF_List)
  Importer(Stage_Directory,file_type = SF_type,Keyword = SF_Keyword,Deliminator = "\t",inList = TRUE,Meta_List_Name = SF_List)


# Create Directory for Check Point Results
Directory <- paste0(Annotation_Directory,"/Check-point/",Sys.Date())
if(dir.exists(Directory)==FALSE) dir.create(Directory)






#################################  Check  Column Names   ######################################

### Stage Files ------------------------------------------
SF_List_Name <- names(SF_List)
SF_List_Name <- unlist(lapply(SF_List_Name,function(x){gsub(SF_Keyword,replacement = "",x)}))

### Annotation Files ------------------------------------------
AF_List_Name <- names(AF_List)
AF_List_Name <- unlist(lapply(AF_List_Name,function(x){gsub(AF_Keyword,replacement = "",x)}))


if(any(!cOnset=="Onset"|!cDuration=="Duration"|!cAnnotation=="Annotation")){

  SF_List <- lapply(SF_List,function(x){
    if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
    if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
    if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
    x
  })
  names(SF_List) <- SF_List_Name

  AF_List <- lapply(AF_List,function(x){
    if(!cOnset=="Onset")  names(x)[names(x) %in% cOnset] <- "Onset"
    if(!cDuration=="Duration") names(x)[names(x) %in% cDuration] <- "Duration"
    if(!cAnnotation=="Annotation") names(x)[names(x) %in% cAnnotation] <- "Annotation"
    x
  })
  names(AF_List) <- AF_List_Name
}




#################################  Check  Stage Annotations   ######################################
### Stage Files ------------------------------------------
names(SF_List) <- SF_List_Name
SF_List_Stage <- lapply(SF_List,function(x){ x[x$Annotation %in% c(Arousal,N1,N2,N3,N4,REM,Unscored),] })


df <- data.frame(matrix(nrow=length(SF_List),ncol=1))
names(df) <- "ID"
df$ID <- SF_List_Name
df$SF_List <- unlist(lapply(SF_List_Stage,nrow))



### Annotation Files ------------------------------------------
names(AF_List) <- AF_List_Name
AF_List_Stage <- lapply(AF_List,function(x){ x[x$Annotation %in% c(Arousal,N1,N2,N3,N4,REM,Unscored),] })


df2 <- data.frame(matrix(nrow=length(AF_List),ncol=1))
names(df2) <- "ID"
df2$ID <- AF_List_Name
df2$AF_List <- unlist(lapply(AF_List_Stage,nrow))



### Warning Missing Stage ------------------------------------------
df <- merge.data.frame(df,df2,by = "ID",all=TRUE)
if(!df$SF_List == df$AF_List) warning("The following files contains missing stage annotation. ",df$ID[!df$SF_List == df$AF_List],
                                      " Please, double check the edited files written.")




#################################  Check  Missing Annotations in REM Epoches  ######################################
for(i in names(AF_List)){


  df <- get(i)



  if(any(grepl('[^[:alnum:]]',df$Onset))){
    print(i)
    if(any(grepl(".0000000",df$Onset))) dk <- df[!grep(".0000000",df$Onset),]
  }

  if(any(schoolmath::is.decimal(df$Onset))) {
    print(paste0(i, " contains non-integer values."))
    if("dk" %in% ls()){
      dk <- rbind(dk,df[schoolmath::is.decimal(df$Onset),])
    } else {
      dk <- df[schoolmath::is.decimal(df$Onset),]
    }

  }
  if("dk" %in% ls()) dk$Note <- "Missplacement"




  if(all(grepl('[^[:alnum:]]',df$Onset))){

    df_5 <- df[df$Annotation=="5",]
    df_5_dups <- plyr::ldply(lapply(1:nrow(df_5),function(r){
      dups <- df_5[r,]
      dups[1:10,"Onset"] <- seq(dups$Onset,dups$Onset+27,by=3)
      dups$Duration <- 3
      dups$Annotation <- "5"
      dups
    }))

    df_w <- df[grep("_W",df$Annotation),]

    df_w_outrange <- df_w[!df_w$Onset %in% df_5_dups$Onset,]
    if(nrow(df_w_outrange)>0) df_w_outrange$Note <- "Out of Range"

    df_w_missing <- df_5_dups[!df_5_dups$Onset %in% df_w$Onset,]
    if(nrow(df_w_missing)>0) df_w_missing$Note <- "Score Missed"

    df_w_duplicates <- df_w[duplicated(df_w$Onset),]
    if(nrow(df_w_duplicates)>0) df_w_duplicates$Note <- "Duplicated Score"

    dm <- rbind(df_w_duplicates,df_w_missing,df_w_outrange)

    if("dk" %in% ls()){
      dk <- rbind(dk,dm)
    } else {
      dk <- dm
    }

    dk$Time <- as.POSIXct("00:00:00", format = "%H:%M:%S") + dk$Onset

  }

  names(dk)[names(dk) %in% c("Onset","Duration","Annotation")] <- c(cOnset,cDuration,cAnnotation)

  if("dk" %in% ls() & nrow(dk)>0) write.table(dk,paste0(Directory,"/",i,"_Misplacement_Error.txt"),sep="\t",quote = FALSE,row.names = FALSE)
  rm(df,dk,dm,i,df_5,df_5_dups,df_w,df_w_outrange,df_w_missing,df_w_duplicates,dups)
}
}
