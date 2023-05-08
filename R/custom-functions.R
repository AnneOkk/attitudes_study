# Read in data ------------------------------------------------------------
read_data_excel <- function(rel_directory, pattern) {
    files <- dir(paste0(dirname(getwd()),"/", rel_directory), pattern = pattern, full.names = FALSE)
    df_list <- vector("list", length(files))
    for (fname in files) {
        df_list[[fname]] <- read_excel(paste0(dirname(getwd()),"/", rel_directory ,fname))
    }
    names(df_list) <- paste0("", gsub(pattern,"",names(df_list)))
    return(df_list)
}


# Mean Median -------------------------------------------------------------

# function example - get measures of central tendency
# and spread for a numeric vector x. The user has a
# choice of measures and whether the results are printed.
mysummary <- function(x,npar=TRUE,print=TRUE) {
    if (!npar) {
        center <- mean(x); spread <- sd(x)
    } else {
        center <- median(x); spread <- mad(x)
    }
    if (print & !npar) {
        cat("Mean=", center, "\n", "SD=", spread, "\n")
    } else if (print & npar) {
        cat("Median=", center, "\n", "MAD=", spread, "\n")
    }
    result <- list(center=center,spread=spread)
    return(result)
}


# Correlation table -------------------------------------------------------

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower", "none"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  else if(removeTriangle[1]=="none"){
    Rnew <- as.matrix(Rnew)
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}


# CSplit ------------------------------------------------------------------

cSplit <- function(indt, splitCols, sep = ",", direction = "wide", 
                   makeEqual = NULL, fixed = TRUE, drop = TRUE, 
                   stripWhite = FALSE) {
  message("`cSplit` is now part of the 'splitstackshape' package (V1.4.0)")
  ## requires data.table >= 1.8.11
  require(data.table)
  if (!is.data.table(indt)) setDT(indt)
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (any(!vapply(indt[, splitCols, with = FALSE],
                  is.character, logical(1L)))) {
    indt[, eval(splitCols) := lapply(.SD, as.character),
         .SDcols = splitCols]
  }
  
  if (length(sep) == 1) 
    sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) {
    stop("Verify you have entered the correct number of sep")
  }
  
  if (isTRUE(stripWhite)) {
    indt[, eval(splitCols) := mapply(function(x, y) 
      gsub(sprintf("\\s+%s\\s+|\\s+%s|%s\\s+", 
                   x, x, x), x, y), 
      sep, indt[, splitCols, with = FALSE], 
      SIMPLIFY = FALSE)]
  }  
  
  X <- lapply(seq_along(splitCols), function(x) {
    strsplit(indt[[splitCols[x]]], split = sep[x], fixed = fixed)
  })
  
  if (direction == "long") {
    if (is.null(makeEqual)) {
      IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
      makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                          FALSE, TRUE)
    }
  } else if (direction == "wide") {
    if (!is.null(makeEqual)) {
      if (!isTRUE(makeEqual)) {
        message("makeEqual specified as FALSE but set to TRUE")
        makeEqual <- TRUE
      }
      makeEqual <- TRUE
    } else {
      makeEqual <- TRUE
    }
  }
  if (isTRUE(makeEqual)) {
    SetUp <- lapply(seq_along(X), function(y) {
      A <- vapply(X[[y]], length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(X[[y]]))
    })    
    Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]), 
                       use.names = FALSE))
    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })
    if (direction == "wide") {
      X <- lapply(seq_along(X), function(x) {
        colnames(X[[x]]) <- paste(splitCols[x], 
                                  sequence(ncol(X[[x]])), 
                                  sep = "_")
        X[[x]]
      })
      if (isTRUE(drop)) {
        cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
      } else {
        cbind(indt, do.call(cbind, X))
      }
    } else {
      indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
      X <- lapply(X, function(y) as.vector(t(y)))
      indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
    }  
  } else {
    Rep <- vapply(X[[1]], length, integer(1L))
    indt <- indt[rep(sequence(nrow(indt)), Rep)]
    indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
  }
}



# harmonic mean -----------------------------------------------------------

## The harmonic mean function
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}





# create network ----------------------------------------------------------

cpc_net <- function(year, data = data) {
  data_cpc_co <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>% 
    .[!grepl("\\|", .$`CPC - Current - DWPI`),] %>% 
    .[!is.na(.$`CPC - Current - DWPI`),] %>% 
    dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d")) 
  data_cpc_co$Pub_year <- format(data_cpc_co$`Publication Date`, format = "%Y")
  data_cpc_co <- data_cpc_co %>% 
    select(`Publication Number`, `CPC - Current - DWPI`, Pub_year) %>% 
    filter(
      if (year == "all") {
        Pub_year == "2015" | Pub_year == "2016" | Pub_year == "2017" | Pub_year == "2018" | Pub_year == "2019" | Pub_year == "2020" | Pub_year == "2021" | Pub_year == "2022"
      } else {
        Pub_year == year
      }
    ) %>% 
    mutate(`CPC - Current - DWPI` = sub("^(....)0{1,3}", "\\1", .$`CPC - Current - DWPI`)) %>% 
    fastDummies::dummy_cols(., select_columns = "CPC - Current - DWPI") %>% 
    select(-`CPC - Current - DWPI`) %>% data.frame(.) %>% 
    group_by(Publication.Number) %>% 
    summarise_if(is.numeric,sum) %>%
    mutate(sum = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>% 
    as.data.frame()
  names(data_cpc_co) <- gsub(".*_","",names(data_cpc_co))
  data_cpc_co_crossprod <- data_cpc_co[,-1] %>% 
    select(-sum) %>% 
    as.matrix(.) %>% 
    crossprod(.) 
  diag(data_cpc_co_crossprod) <- 0      
  data_cpc_co_crossprod <- data.frame(data_cpc_co_crossprod) %>% 
    mutate(Pub_Num = rownames(data_cpc_co_crossprod)) %>% 
    pivot_longer(-Pub_Num) %>% 
    group_by(Pub_Num) %>% 
    #filter(value > 7) %>% #set value for minimum cooccureences
    pivot_wider(names_from = name, values_from=value)
  
  data_links <- data_cpc_co_crossprod %>%  
    pivot_longer(., cols = -Pub_Num) %>% 
    filter(., value > 0) %>% 
    rename(source = Pub_Num, 
           target = name,
           importance = value) %>%                                     
    group_by(source, target) %>%
    dplyr::summarise(importance = sum(importance)) %>% 
    as.data.frame() %>% 
    .[!duplicated(data.frame(t(apply(.[1:2], 1, sort)))),] 
  # remove links between same CPC classes
  data_links$source_letter <- substr(data_links$source, 1, 1) 
  data_links$target_letter <- substr(data_links$target, 1, 1)
  data_links <- data_links[!as.character(data_links$source_letter) >= as.character(data_links$target_letter ),]
  nodes <- 
    data_links %>% 
    select(source, target) 
  nodes <- 
    data.frame(source = c(nodes[,"source"], nodes[,"target"])) %>% 
    mutate(Subclass = substr(.$source, 1, 4)) %>% 
    mutate(Subclass = toupper(Subclass)) %>% 
    mutate(Subclass = if_else(grepl("A61B",.$Subclass),'(A61B) Diagnostic instruments and processes',
                              if_else(grepl("A61F",.$Subclass),'(A61F) Filters implantable into blood vessels',
                              if_else(grepl("A61H",.$Subclass),'(A61H) Physical therapy apparatus',
                              if_else(grepl("A61K",.$Subclass),'(A61K) Preparations for pharmaceutical products',
                              if_else(grepl("A61M",.$Subclass),'(A61M) Devices for introducing media into or onto the body',
                              if_else(grepl("A61N",.$Subclass),'(A61N) Electrotherapy',
                              if_else(grepl("A61P",.$Subclass),'(A61P) Therapeutic activity of chemical compounds or medicinial preparations',
                              if_else(grepl("A63F",.$Subclass),'(A63F) Gaming',
                              if_else(grepl("A63H",.$Subclass),'(A63H) Toys',
                              if_else(grepl("B25J",.$Subclass),'(B25J) Manipulators',
                              if_else(grepl("C07K",.$Subclass),'(C07K) Peptides',
                              if_else(grepl("C12N",.$Subclass),'(C12N) Microorganisms or enzymes',
                              if_else(grepl("C12Q",.$Subclass),'(C12Q) Measuring or testing processes involving enzymes, nucleic acids, or microorganisms',
                              if_else(grepl("G01N",.$Subclass),'(G01N) Investigating or analyzing materials by determining their physical or chemical properties',
                              if_else(grepl("G01P",.$Subclass),'(G01P) Measuring linear or angular speed, acceleration, or shock',
                              if_else(grepl("G01R",.$Subclass),'(G01R) Measuring electric variables',
                              if_else(grepl("G01V",.$Subclass),'(G01V) Geophysics',
                              if_else(grepl("G05B",.$Subclass),'(G05B) Control or regulating systems in general',
                              if_else(grepl("G06F",.$Subclass),'(G06F) Electronic digital data processing',
                              if_else(grepl("G06K",.$Subclass),'(G06K) Graphical data reading',
                              if_else(grepl("G06N",.$Subclass),'(G06N) Computing arrangements based on computational models',
                              if_else(grepl("G06Q",.$Subclass),'(G06Q) Data processing systems or methods for administrative purposes',
                              if_else(grepl("G06T",.$Subclass),'(G06T) Image data processing or generation',
                              if_else(grepl("G06V",.$Subclass),'(G06V) Image or video recognition or understanding',
                              if_else(grepl("G07F",.$Subclass),'(G07F) Coin sorting',
                              if_else(grepl("G08B",.$Subclass),'(G08B) Signalling or calling systems',
                              if_else(grepl("G09B",.$Subclass),'(G09B) Educational or demonstration appliances',
                              if_else(grepl("G10L",.$Subclass),'(G10L) Speech analysis or synthesis',
                              if_else(grepl("G16C",.$Subclass),'(G16C) Computational chemistry',
                              if_else(grepl("G16H",.$Subclass),'(G16H) Healthcare informatics', 
                              if_else(grepl("G16Y",.$Subclass),'(G16Y) ICT for Internet of Things', 
                              if_else(grepl("G16Z",.$Subclass),'(G16Z) ICT for specific applications',
                              if_else(grepl("H04B",.$Subclass),'(H04B) Transmission',
                              if_else(grepl("H04L",.$Subclass),'(H04L) Transmission of digital information',
                              if_else(grepl("H04W",.$Subclass),'(H04W) Wireless communication networks',
                              if_else(grepl("Y02A",.$Subclass),'(Y02A) Technologies for adaptation to climate change',
                              if_else(grepl("G16B",.$Subclass),'(G16B) Bioinformatics', 'none')))))))))))))))))))))))))))))))))))))) %>% 
    rename(name = source) %>% 
    .[!duplicated(.$name), ] 
  nodes$Subclass <- gsub('(.{1,24})(\\s|$)', '\\1\n', nodes$Subclass)
  
  data_cpc_co <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>% 
    .[!grepl("\\|", .$`CPC - Current - DWPI`),] %>% 
    .[!is.na(.$`CPC - Current - DWPI`),] %>% 
    dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d")) 
  data_cpc_co$Pub_year <- format(data_cpc_co$`Publication Date`, format = "%Y")  
  data_cpc_co <- data_cpc_co %>% select(`Publication Number`, `CPC - Current - DWPI`, Pub_year) 
  # Turn it into igraph object
  network <- graph_from_data_frame(d=data_links, vertices=nodes, directed=F)
  len <- length(unique(nodes$Subclass))
  list(network = network, len = len, nodes = unique(nodes$Subclass)) 
}





# CPC Trends --------------------------------------------------------------
cpc_trends <- function(data = data) {
  data_cpc <- separate_rows(data,`CPC - Current - DWPI`,sep = " | ") %>% 
      dplyr::mutate("Publication Date" = as.Date(.$`Publication Date`, format = "%Y-%m-%d"))
  data_cpc$year_month <- floor_date(data_cpc$`Publication Date`,  # Create year-month column
                                  "month")
  data_cpc <- data_cpc[!grepl("\\|", data_cpc$`CPC - Current - DWPI`),] 
  data_cpc <- data_cpc[!is.na(data_cpc$`CPC - Current - DWPI`),]
  data_cpc$cpc_short <- substr(data_cpc$`CPC - Current - DWPI`, 1, 4)   
  # Top CPC codes over all years
  data_cpc_trend <- data_cpc %>%
    count(year_month,cpc_short) %>% 
    group_by(cpc_short, year_month) %>%
    mutate(mean_n = mean(n)) 
  data_cpc_trend <- data_cpc_trend[data_cpc_trend$cpc_short %in% names(which(table(data_cpc_trend$cpc_short) > 9)), ] %>% 
    data.frame(.) %>% 
    mutate(cpc_short_new = 
if_else(grepl("A61B",.$cpc_short),'(A61B) Diagnostic instruments and processes',
if_else(grepl("A61F",.$cpc_short),'(A61F) Filters implantable into blood vessels',
if_else(grepl("A61H",.$cpc_short),'(A61H) Physical therapy apparatus',
if_else(grepl("A61K",.$cpc_short),'(A61K) Preparations for pharmaceutical products',
if_else(grepl("A61M",.$cpc_short),'(A61M) Devices for introducing media into or onto the body',
if_else(grepl("A61N",.$cpc_short),'(A61N) Electrotherapy',
if_else(grepl("A61P",.$cpc_short),'(A61P) Therapeutic activity of chemical compounds or medicinial preparations',
if_else(grepl("A63F",.$cpc_short),'(A63F) Gaming',
if_else(grepl("A63H",.$cpc_short),'(A63H) Toys',
if_else(grepl("B25J",.$cpc_short),'(B25J) Manipulators',
if_else(grepl("C07K",.$cpc_short),'(C07K) Peptides',
if_else(grepl("C12N",.$cpc_short),'(C12N) Microorganisms or enzymes',
if_else(grepl("C12Q",.$cpc_short),'(C12Q) Measuring or testing processes involving enzymes, nucleic acids, or microorganisms',
if_else(grepl("G01N",.$cpc_short),'(G01N) Investigating or analyzing materials by determining their physical or chemical properties',
if_else(grepl("G01P",.$cpc_short),'(G01P) Measuring linear or angular speed, acceleration, or shock',
if_else(grepl("G01R",.$cpc_short),'(G01R) Measuring electric variables',
if_else(grepl("G01V",.$cpc_short),'(G01V) Geophysics',
if_else(grepl("G05B",.$cpc_short),'(G05B) Control or regulating systems in general',
if_else(grepl("G06F",.$cpc_short),'(G06F) Electronic digital data processing',
if_else(grepl("G06K",.$cpc_short),'(G06K) Graphical data reading',
if_else(grepl("G06N",.$cpc_short),'(G06N) Computing arrangements based on computational models',
if_else(grepl("G06Q",.$cpc_short),'(G06Q) Data processing systems or methods for administrative purposes',
if_else(grepl("G06T",.$cpc_short),'(G06T) Image data processing or generation',
if_else(grepl("G06V",.$cpc_short),'(G06V) Image or video recognition or understanding',
if_else(grepl("G07F",.$cpc_short),'(G07F) Coin sorting',
if_else(grepl("G08B",.$cpc_short),'(G08B) Signalling or calling systems',
if_else(grepl("G09B",.$cpc_short),'(G09B) Educational or demonstration appliances',
if_else(grepl("G10L",.$cpc_short),'(G10L) Speech analysis or synthesis',
if_else(grepl("G16C",.$cpc_short),'(G16C) Computational chemistry',
if_else(grepl("G16H",.$cpc_short),'(G16H) Healthcare informatics', 
if_else(grepl("G16Y",.$cpc_short),'(G16Y) ICT for Internet of Things', 
if_else(grepl("G16Z",.$cpc_short),'(G16Z) ICT for specific applications',
if_else(grepl("H04B",.$cpc_short),'(H04B) Transmission',
if_else(grepl("H04L",.$cpc_short),'(H04L) Transmission of digital information',
if_else(grepl("H04W",.$cpc_short),'(H04W) Wireless communication networks',
if_else(grepl("Y02A",.$cpc_short),'(Y02A) Technologies for adaptation to climate change',
if_else(grepl("G16B",.$cpc_short),'(G16B) Bioinformatics', 'none'))))))))))))))))))))))))))))))))))))))
  data_cpc_trend$cpc_short_new <- gsub('(.{1,24})(\\s|$)', '\\1\n', data_cpc_trend$cpc_short_new) 
  data_cpc_trend <- data_cpc_trend %>%  group_by(year_month) %>% mutate(overall_trend = sum(n)) %>% 
    mutate(rel_trend = n/overall_trend)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  data_cpc_trend$overall_trend <- range01(data_cpc_trend$overall_trend)
  colors <- unique(data_cpc_trend$cpc_short_new)
  names(colors) <- colors
  color_codes <- colors[intersect(names(dis_pal), names(colors))] <- dis_pal[intersect(names(dis_pal), names(colors))]
  list(cpc_df = data_cpc_trend, color_codes = color_codes)
}



# Create map data ----------------------------------------------------------------

library(mapdeck)

mapping_data <- function(data = data) {
  
  # improve assignee and inventor columns - to title case
data$Assignee_corrected <-gsub("\\|.*","",data$`Assignee - DWPI`) %>% 
  str_to_title() 
data$Inventor_corrected <-gsub("\\|.*","",data$`Inventor - DWPI`) %>% str_to_title() 

# crate columns with number of patents per assignee and inventor
data_map<- data %>%
  group_by(`Assignee Country`) %>%
  mutate(ass_count_n = as.numeric(n())) %>% 
  ungroup(.) %>% 
  group_by(Assignee_corrected) %>% 
  mutate(ass_n = as.numeric(n())) %>% 
  ungroup(.) %>% 
  group_by(Inventor_corrected) %>% 
  mutate(invent_n = as.numeric(n())) %>% 
  ungroup(.)

# remove all rows with < 3 patents per assignee/ inventor
data_map$Assignee_corrected[data_map$ass_n < 3]=NA
data_map$Inventor_corrected[data_map$invent_n < 3]=NA
data_map$ass_n[data_map$ass_n < 3]=NA
data_map$invent_n[data_map$invent_n < 3]=NA

# create lat and lon for assignee location
data_map$assign_city_lat[data_map$Assignee_corrected == "Medtronic Ardian Luxembourg Sarl"] <- 45.069 #finance holding in Luxemburg, but headquater minneapolis
data_map$assign_city_lon[data_map$Assignee_corrected == "Medtronic Ardian Luxembourg Sarl"] <- -93.250 

data_map$assign_city_lat[data_map$Assignee_corrected == "Konink Philips Nv"] <- 51.407 
data_map$assign_city_lon[data_map$Assignee_corrected == "Konink Philips Nv"] <- 5.459

data_map$assign_city_lat[data_map$Assignee_corrected == "Korea Advanced Sci & Technology Inst"] <- 36.371
data_map$assign_city_lon[data_map$Assignee_corrected == "Korea Advanced Sci & Technology Inst"] <- 127.359

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Hanyang Iucf-Hyu"] <- 37.557
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Hanyang Iucf-Hyu"] <- 127.048

data_map$assign_city_lat[data_map$Assignee_corrected == "Int Business Machines Corp"] <- 41.107
data_map$assign_city_lon[data_map$Assignee_corrected == "Int Business Machines Corp"] <- -73.718

data_map$assign_city_lat[data_map$Assignee_corrected == "X Dev Llc"] <- 37.422
data_map$assign_city_lon[data_map$Assignee_corrected == "X Dev Llc"] <- -122.084

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Leland Stanford Junior "] <- 37.424
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Leland Stanford Junior "] <- -122.169

data_map$assign_city_lat[data_map$Assignee_corrected == "Ginger.io Inc"] <- 37.787
data_map$assign_city_lon[data_map$Assignee_corrected == "Ginger.io Inc"] <- -122.400

data_map$assign_city_lat[data_map$Assignee_corrected == "Blackthorn Therapeutics Inc"] <- 37.664
data_map$assign_city_lon[data_map$Assignee_corrected == "Blackthorn Therapeutics Inc"] <- -122.389

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Third Military Medical Pla Third "] <- 29.535
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Third Military Medical Pla Third "] <- 106.448

data_map$assign_city_lat[data_map$Assignee_corrected == "Univ Zhejiang Technology"] <- 30.224
data_map$assign_city_lon[data_map$Assignee_corrected == "Univ Zhejiang Technology"] <- 120.048

# rename assignees 
data_map$Assignee_corrected <- gsub('Medtronic Ardian Luxembourg Sarl', 'USA: Medtronic', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Konink Philips Nv', 'NL: Koninklijke Philips NV', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Korea Advanced Sci & Technology Inst', 'KR: Advanced Sci. & Tech. Inst.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('X Dev Llc', 'USA: X Development', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Int Business Machines Corp', 'USA: IBM', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Leland Stanford Junior ', 'USA: Stanford Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Ginger.io Inc', 'USA: Ginger.io', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Third Military Medical Pla Third ', 'CN: Third Military Medical Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Blackthorn Therapeutics Inc', 'USA: Blackthorn Therapeutics', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Zhejiang Technology', 'CN: Zhejiang Tech. Univ.', data_map$Assignee_corrected)
data_map$Assignee_corrected <- gsub('Univ Hanyang Iucf-Hyu', 'KR: Hanyang Univ.', data_map$Assignee_corrected)


# add number of patents behind assignee name
data_map$Assignee_corrected <- paste0(stringr::str_replace_na(data_map$Assignee_corrected, replacement=""), " (", stringr::str_replace_na(data_map$ass_n, replacement=""), ")") 

is.na(data_map$Assignee_corrected) <- data_map$Assignee_corrected == " ()"

capitals$`Assignee Country` <- capitals$country

data_map <- merge(data_map,capitals, by  = "Assignee Country", all.x=TRUE) 

data_map $capital[data_map$`Assignee Country` == "Czechia"] <- "Prague"
data_map$lat[data_map$`Assignee Country` == "Czechia"] <- 50.07
data_map$lon[data_map$`Assignee Country` == "Czechia"] <- 14.41

data_map$capital[data_map$`Assignee Country` == "Japan"] <- "Tokyo"
data_map$lat[data_map$`Assignee Country` == "Japan"] <- 35.67
data_map$lon[data_map$`Assignee Country` == "Japan"] <- 139.65

data_map$capital[data_map$`Assignee Country` == "Luxemburg"] <- "Luxemburg"
data_map$lat[data_map$`Assignee Country` == "Luxemburg"] <- 49.81
data_map$lon[data_map$`Assignee Country` == "Luxemburg"] <- 6.12

data_map$capital[data_map$`Assignee Country` == "Singapore"] <- "Singapore"
data_map$lat[data_map$`Assignee Country` == "Singapore"] <- 1.35
data_map$lon[data_map$`Assignee Country` == "Singapore"] <- 103.81

data_map$capital[data_map$`Assignee Country` == "South Korea"] <- "Seoul"
data_map$lat[data_map$`Assignee Country` == "South Korea"] <- 37.56
data_map$lon[data_map$`Assignee Country` == "South Korea"] <- 126.97

data_map$capital[data_map$`Assignee Country` == "UK"] <- "London"
data_map$lat[data_map$`Assignee Country` == "UK"] <- 51.50
data_map$lon[data_map$`Assignee Country` == "UK"] <- -0.12

data_map$capital[data_map$`Assignee Country` == "USA"] <- "Washington, D.C."
data_map$lat[data_map$`Assignee Country` == "USA"] <- 38.90
data_map$lon[data_map$`Assignee Country` == "USA"] <- -77.16
data_map
}



# Fit models SEM ----------------------------------------------------------


## Defining function for fitting all competing models to Case study data ##
fit_models <- function(models, data) {
  model_fits <- list()
  for(i in 1:length(models)) {
    model_fit <- sem(models[[i]], data, estimator= "ML", meanstructure=TRUE)
    if(model_fit@Fit@converged)
      model_fits[[length(model_fits)+1]] <- model_fit
    else
      warning(paste0('model: ',model, ' - fit did not converge'))
  }
  return (model_fits)
}



## Defining function for averaging the selected best models ##
average_models <- function(model_fits, sel_models, sel_wt, vars, coefs) {
  
  intercepts <- paste0(vars,'~1')
  coefs_mat <- matrix(0,nrow=length(sel_models),ncol=length(coefs))
  intercepts_mat <- matrix(0,nrow=length(sel_models),ncol=length(vars))
  colnames(coefs_mat) <- coefs
  colnames(intercepts_mat) <- intercepts
  for(i in 1:length(sel_models)) {
    est <- coef(model_fits[[sel_models[i]]])[coefs]
    est <- est[which(!is.na(est))]
    coefs_mat[i,names(est)] <- est*sel_wt[i]
    intercepts_mat[i,] <- coef(model_fits[[sel_models[i]]])[intercepts]*sel_wt[i]
  }
  coef_avg <- colSums(coefs_mat)
  intercepts_avg <- colSums(intercepts_mat)
  return (list(coef=coef_avg[coef_avg!=0],intercepts=intercepts_avg[intercepts_avg!=0]))
}



## Defining function for ranking competing models based on adjusted-BIC ##
BICc.lavaan <- function(object, second.ord = TRUE, c.hat = 1, return.K = FALSE){
  object <- as.list(fitMeasures(object))
  npar<-object$baseline.df - object$df
  if(return.K==T) return(object$npar)
  if(second.ord==F && c.hat>1) return(object$chisq) #this should be QBIC once defined
  if(second.ord==F) return(object$bic)
  if(c.hat>1) return(object$ntotal) #this should be aQBIC once defined
  object$bic2
}

BICctab.lavaan <- function(cand.set, modnames, sort = TRUE, c.hat = 1, second.ord = TRUE, nobs = NULL){
  
  if(is.null(modnames)) modnames<-1:length(cand.set)
  
  # check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
  # if (length(unique(check.resp)) > 1)
  #     stop("You must use the same response variable for all models\n")
  
  Results <- data.frame(Modnames = modnames)
  Results$K <- unlist(lapply(X = cand.set, FUN = BICc.lavaan,
                             return.K = TRUE, c.hat = c.hat,second.ord = second.ord))
  
  Results$BIC2 <- unlist(lapply(X = cand.set, FUN = BICc.lavaan,
                                return.K = FALSE, c.hat = c.hat,second.ord = second.ord))
  
  Results$Delta_BIC2 <- Results$BIC2 - min(Results$BIC2)
  Results$ModelLik <- exp(-0.5 * Results$Delta_BIC2)
  Results$BIC2Wt <- Results$ModelLik/sum(Results$ModelLik)
  
  if (length(unique(Results$BIC2)) != length(cand.set))
    warning("\nCheck model structure carefully as some models may be redundant\n")
  
  if (second.ord == TRUE && c.hat == 1) {
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
  }
  
  if (second.ord == TRUE && c.hat > 1) {
    colnames(Results) <- c("Modnames", "K", "QAICc", "Delta QAICc", "ModelLik", "QAICcWt")
    LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    Results$Quasi.LL <- LL/c.hat
    Results$c_hat <- c.hat #this should be aQBIC once defined
  }
  
  if (second.ord == FALSE && c.hat == 1) {
    colnames(Results) <- c("Modnames", "K", "BIC", "Delta BIC", "ModelLik", "BICWt")
    Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
  }
  
  if (second.ord == FALSE && c.hat > 1) {
    colnames(Results) <- c("Modnames", "K", "QAIC", "Delta QAIC", "ModelLik", "QAICWt")
    LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    Results$Quasi.LL <- LL/c.hat
    Results$c_hat <- c.hat #this should be QBIC once defined
  }
  
  if (sort) {
    Results <- Results[rev(order(Results[, 6])), ]
    Results$Cum.Wt <- cumsum(Results[, 6])
  }
  else {
    Results$Cum.Wt <- NULL
  }
  
  class(Results) <- c("aictab", "data.frame")
  return(Results)
}


# Change multiple column names --------------------------------------------

library(data.table)
setnames_across <- function(df, range, name) {
  setnames(df, old = names(data_t31_w1[range]), new = paste(name, seq_along(data_t31_w1[range]), sep = "_"))
}


# Simulate from existing df -----------------------------------------------
library(pgirmess)
rnorm_multi <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0,
                        varnames = NULL, empirical = FALSE, 
                        as.matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
    #   # reinstate system seed after simulation
    #   gs <- global_seed(); on.exit(global_seed(gs))
  }
  
  # error handling ----
  if ( !is.numeric(n) || n %% 1 > 0 || n < 1 ) {
    stop("n must be an integer > 0")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  # try to guess vars if not set ----
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    }
    
    if (is.null(vars)) {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    #mu <- as.matrix(mu) %>% as.vector()
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    #sd <- as.matrix(sd) %>% as.vector()
  }
  
  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r # for name-checking later
  } else {
    # get data from mvn ----
    cor_mat <- tryCatch(
      { cormat(r, vars) }, 
      error = function(e) {
        stop("The correlation you specified is impossible: ", e$message, call. = FALSE)
      }
    )
    
    sigma <- (sd %*% t(sd)) * cor_mat
    # tryCatch({
    #   mvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
    # }, error = function(e) {
    #   stop("The correlated variables could not be generated. If empirical = TRUE, try increasing the N or setting empirical = FALSE.")
    # })
    
    err <- "The correlated variables could not be generated."
    if (empirical) err <- paste(err, "Try increasing the N or setting empirical = FALSE.")
    
    # code from MASS:mvrnorm
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) stop(err)
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -1e-06 * abs(ev[1L]))) stop(paste(err))
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch({
      X <- drop(mu) + eS$vectors %*% 
        diag(sqrt(pmax(ev, 0)), p) %*%  t(X)
    }, error = function(e) { stop(err) })
    
    mvn <- t(X)
  }
  # coerce to matrix if vector when n == 1
  if (n == 1) mvn <- matrix(mvn, nrow = 1)
  
  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(mvn) <- colnames(cor_mat)
  } else if (!is.null(names(mu))) {
    #use mu names 
    colnames(mvn) <- names(mu)
  } else if (!is.null(names(sd))) {
    #use sd names 
    colnames(mvn) <- names(sd)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }
  
  if (as.matrix == TRUE) mvn else data.frame(mvn, check.names = FALSE)
}

sim_df <- function (data, n = 100, within = c(), between = c(), 
                    id = "id", dv = "value",
                    empirical = FALSE, long = FALSE, seed = NULL, 
                    missing = FALSE, sep = faux_options("sep")) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
    #   # reinstate system seed after simulation
    #   gs <- global_seed(); on.exit(global_seed(gs))
    #   set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  # error checking ------
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  if (length(within) > 0 & all(within %in% names(data))) {
    # convert long to wide
    data <- long2wide(data = data, 
                      within = within, 
                      between = between, 
                      dv = dv, id = id, sep = sep)
  }
  
  if (is.numeric(between)) between <- names(data)[between]
  
  if (length(between) > 0 & !is.character(between)) {
    stop("between must be a numeric or character vector")
  }
  
  numvars <- setdiff(names(data), c(id, dv, between))
  is_num <- sapply(data[numvars], is.numeric)
  numvars <- numvars[is_num]
  
  grps <- data[between]
  if (length(grps) == 0) grps <- rep(1, nrow(data))
  
  simdat <- by(data, grps, function(x) {
    y <- x[numvars]
    z <- rnorm_multi(
      n = n,
      vars = ncol(y),
      mu = sapply(y, mean, na.rm = TRUE),
      sd = sapply(y, sd, na.rm = TRUE),
      r = cor(y, use = "complete.obs"),
      varnames = names(y),
      empirical = empirical
    )
    
    # simulate missing data pattern
    if (missing) {
      na_cells <- dplyr::mutate_all(y, is.na) %>% 
        sim_joint_dist(n = n)
      
      z <- mapply(function(sim_col, nc_col) {
        sim_col[nc_col] <- NA
        sim_col
      }, z, na_cells, SIMPLIFY = FALSE) %>%
        as.data.frame()
    }
    
    ## add between vars
    for (b in between) z[b] <- unique(x[[b]])
    z[ , c(between, numvars), drop = FALSE]
  }) %>% do.call(rbind, .)
  
  # fix names
  nm <- names(simdat)
  simdat[id] <- make_id(nrow(simdat))
  simdat <- simdat[c(id, nm)]
  rownames(simdat) <- c()
  
  # convert to long
  if (long) {
    simdat <- wide2long(simdat, 
                        within_factors = within, 
                        within_cols = numvars,
                        dv = dv, id = id, sep = sep)
  }
  
  return(simdat)
}



# Set names across dailies --------------------------------------------------------

setnames_across_dailies <- function(df) {
  setnames_across(df, 10:12, "w1_engphys")
  setnames_across(df, 13:15, "w1_engemo")
  setnames_across(df, 16:18, "w1_engcog")
  
  setnames_across(df, 19:21, "w1_fatphys")
  setnames_across(df, 22:24, "w1_fatment")
  setnames_across(df, 25:27, "w1_fatcog")
  
  setnames_across(df, 28, "w1_jobsat")
  setnames_across(df, 29, "w1_lifesat")
  
  setnames_across(df, 30:34, "w1_posaff")
  setnames_across(df, 35:39, "w1_negaff")
  
  setnames_across(df, 40:42, "w1_turnover")
  
  setnames_across(df, 43:54, "w1_sf12")
  
  setnames_across(df, 55:57, "w1_quantworkload")
  
  setnames_across(df, 58, "w1_ineffwork")
  setnames_across(df, 59, "w1_workoutsideresp")
  
  setnames_across(df, 60:62, "w1_jobaut")
  
  setnames_across(df, 63:66, "w1_cosupp")
  setnames_across(df, 67:70, "w1_counder")
  
  setnames_across(df, 71:74, "w1_susupp")
  setnames_across(df, 75:78, "w1_suunder")
  
  setnames_across(df, 79:83, "w1_ldirect")
  setnames_across(df, 84:88, "w1_lemp")
  
  setnames_across(df, 89:93, "w1_digi")
  
  setnames_across(df, 94:98, "w1_flex")
  
  setnames_across(df, 99:100, "w1_sdtaut")
  setnames_across(df, 101:102, "w1_sdtcomp")
  setnames_across(df, 103:104, "w1_sdtrel")
  
  setnames_across(df, 105:113, "w1_craft")
  
  setnames_across(df, 114, "w1_hoposs")
  setnames_across(df, 115, "w1_howork")
  
  setnames_across(df, 116, "w1_holrel")
  setnames_across(df, 117, "w1_hocrel")
  setnames_across(df, 118, "w1_hoexcl")
  setnames_across(df, 119, "w1_hoopmi")
  setnames_across(df, 120, "w1_hocomi")
  setnames_across(df, 121, "w1_hoinmi")
  
  setnames_across(df, 122, "w1_hoplac")
  setnames_across(df, 123, "w1_hoplacother")
  
  setnames_across(df, 124, "w1_hohapp")
  setnames_across(df, 125, "w1_hose")
  
  setnames_across(df, 126:128, "w1_covid")
  
  setnames_across(df, 129:133, "w1_leaderself")
  setnames_across(df, 134:146, "w1_leaderother")
  
  setnames_across(df, 147:155, "w1_workhealth")

}
