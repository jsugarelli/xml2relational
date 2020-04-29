#' @title Package 'xml2relational'
#'
#' @description Transforming a hierarchical XML document into a relational data
#'   model.
#'
#' @section What is \code{xml2relational}:
#' The \code{xml2relational} package is
#'   designed to 'flatten' XML documents with nested objects into relational
#'   dataframes. \code{xml2relational} takes an XML file as input and converts
#'   it into a set of dataframes (tables). The tables are linked among each
#'   other with foreign keys and can be exported as CSV or ready-to-use SQL code
#'   (\code{CREATE TABLE} for the data model, \code{INSERT INTO} for the data).
#'
#'
#' @section How to use \code{xml2relational}:
#' \itemize{ \item First, use
#'   \code{\link{toRelational}()} to read in an XML file and to convert into a
#'   relational data model. \item This will give you a list of dataframes, one
#'   for each table in the relational data model. Tables are linked by foreign
#'   keys. You can specify the naming convention for the tables' primary and
#'   foreign keys as arguments in \code{\link{toRelational}()}. \item You can
#'   now export the data structures of the tables (or a selection of tables)
#'   using  \code{\link{getCreateSQL}()}. It support multiple SQL dialects, and
#'   you also provide syntax and data type information for additional SQL
#'   dialects. \item You can also export the data as SQL \code{INSERT}
#'   statements with the \code{\link{getInsertSQL}()}. If you only want to
#'   export the data as CSV use \code{\link{savetofiles}()} to save the
#'   dataframes produced by \code{\link{toRelational}()} as comma-separated
#'   files.
#'   }
#'@name xml2relational
NULL



get.df <- function(l, table.name) {
  if(table.name %in% names(l)) {
    return(which(names(l)==table.name))
  }
  else return(NULL)
}


# check.all: Unique ID accross all tables or only in relation to current table?
# table.name: Table for which ID is generated
create.id <- function(l, table.name, check.all = TRUE, prefix.primary = "ID_", keys.dim = 6) {
  id <- NULL
  df.index <- get.df(l, table.name)
  if(df.index) {
    repeat {
      id <- round(stats::runif(1, 1, 10^keys.dim-1),0)
      if(check.all == TRUE) to.check <- names(l)
      else to.check <- table.name
      found <- FALSE
      for(i in 1:NROW(to.check)){
        if(id %in% l[[which(names(l)==to.check[i])]][, paste0(prefix.primary, to.check[i])]) found <- TRUE
      }
      if(found == FALSE) break
    }
  }
  return(id)
}


serial.df <- function(l, elem.df, df.name, record, prefix.primary, prefix.foreign) {
  serial <- c()
  elem.df <- data.frame(lapply(elem.df, as.character), stringsAsFactors = FALSE)
  for(i in 1:NCOL(elem.df)) {
    if(stringr::str_sub(names(elem.df)[i], 1, nchar(prefix.foreign)) == prefix.foreign) {
      table.name <- stringr::str_replace(names(elem.df)[i], prefix.foreign, "")
      df.sub <- l[[get.df(l, table.name)]]
      if(is.null(df.sub)) {
        return(NA)
      }
      else {
        if(!is.na(elem.df[record, i])) {
          serial <- append(serial, tidyr::replace_na(
            serial.df(l, df.sub, table.name, which(df.sub[, paste0(prefix.primary, table.name)] == elem.df[record, i]), prefix.primary, prefix.foreign),0))
        }
      }
    }
    else {
      if(stringr::str_sub(names(elem.df)[i], 1, nchar(prefix.primary)) != prefix.primary) {
        serial <- append(serial, tidyr::replace_na(elem.df[record, i], 0))
        names(serial)[NROW(serial)] <- paste0(df.name, "@", names(elem.df)[i])
      }
    }
  }
  if(NROW(serial) > 0) {
    return(tidyr::replace_na(serial[order(names(serial))],0))
  }
  else return(NA)
}


serial.xml <- function(obj) {
  serial <- c()
  chdr <- xml2::xml_children(obj)
  if(length(chdr) > 0) {
    for(i in 1:length(chdr)) {
      if(length(xml2::xml_children(chdr[i])) > 0) serial <- append(serial, serial.xml(chdr[i]))
      else {
        ctn <- as.character(xml2::xml_contents(chdr[i]))
        if(identical(ctn, character(0))) ctn <- NA
        serial <- append(serial, tidyr::replace_na(ctn, 0))
        names(serial)[NROW(serial)] <- paste0(xml2::xml_name(obj), "@", xml2::xml_name(chdr[i]))
      }
    }
  }
  return(serial[order(names(serial))])
}


find.object <- function(l, obj, prefix.primary, prefix.foreign) {
  ex <- NULL
  elem <- get.df(l, xml2::xml_name(obj))
  if(is.null(elem)) ex <- NULL
  else {
    elem.df <- l[[elem]]
    for(i in 1:NROW(elem.df)) {
      res.df <- serial.df(l, elem.df, xml2::xml_name(obj), i, prefix.primary, prefix.foreign)
      res.xml <- serial.xml(obj)
      if(NROW(res.df) == NROW(res.xml)) {
        if(sum(tidyr::replace_na(res.df, 0) == tidyr::replace_na(res.xml, 0)) - NROW(res.df) == 0) {
          ex <- elem.df[i,paste0(prefix.primary,xml2::xml_name(obj))]
          break
        }
      }
    }
  }
  return(ex)
}


# Mögliche Parameter: Prefix für IDs, IDs unique über alle Tabellen (check.all), Länge der Primärschlüssel
parseXMLNode <- function(parent, envir, first = FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim) {
  if(first == TRUE) {
    xml2relational <- new.env(parent = baseenv())
    rlang::env_bind(xml2relational, ldf=list())
    parseXMLNode(parent, xml2relational, FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim)
  }
  else {
    obj.name <- xml2::xml_name(parent)
    chdr <- xml2::xml_children(parent)
    # Does parent have children, i.e. is parent an object?
    if(length(chdr) > 0) {
      elem <- get.df(envir$ldf, obj.name)
      # Is there no dataframe for the parent?
      if(is.null(elem)) {
        # Create new dataframe
        envir$ldf[[length(envir$ldf)+1]] <- data.frame()
        names(envir$ldf)[length(envir$ldf)] <- obj.name
        # Create record in dataframe
        id.name <- paste0(prefix.primary, obj.name)
        envir$ldf[[length(envir$ldf)]][1,id.name] <- 0
        id.value <- create.id(envir$ldf, obj.name, keys.unique, prefix.primary)
        envir$ldf[[length(envir$ldf)]][1,id.name] <- id.value
        for(i in 1:length(chdr)) {
          if(length(xml2::xml_children(chdr[i])) > 0) envir$ldf[[get.df(envir$ldf, obj.name)]][1, paste0(prefix.foreign, xml2::xml_name(chdr[i]))] <- parseXMLNode(chdr[i], envir, FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim)$value
          else envir$ldf[[get.df(envir$ldf, obj.name)]][1, xml2::xml_name(chdr[i])] <- parseXMLNode(chdr[i], envir, FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim)$value
        }
        return(list(ldf=envir$ldf, value=id.value))
      }
      # dataframe for the object exists already
      else {
        res <- find.object(envir$ldf, parent, prefix.primary, prefix.foreign)
        elem <- get.df(envir$ldf, obj.name)
        # Is parent not yet captured in dataframe?
        if(is.null(res)) {
          id.name <- paste0(prefix.primary, obj.name)
          id.value <- create.id(envir$ldf, obj.name, TRUE, prefix.primary, keys.dim)
          new.index <- NROW(envir$ldf[[elem]]) + 1
          envir$ldf[[elem]][new.index,id.name] <- id.value
          for(i in 1:length(chdr)) {
            if(length(xml2::xml_children(chdr[i])) > 0) envir$ldf[[get.df(envir$ldf, obj.name)]][new.index, paste0(prefix.foreign, xml2::xml_name(chdr[i]))] <- parseXMLNode(chdr[i], envir, FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim)$value
            else envir$ldf[[get.df(envir$ldf, obj.name)]][new.index, xml2::xml_name(chdr[i])] <- parseXMLNode(chdr[i], envir, FALSE, prefix.primary, prefix.foreign, keys.unique, keys.dim)$value
          }
          return(list(ldf=envir$ldf, value=id.value))
        }
        # Return ID of existing parent entry in dataframe
        else return(list(ldf=envir$ldf, value=res))
      }
    }
    # parent is not an object
    else {
      res <- as.character(xml2::xml_contents(parent))
      if(length(res) > 0) return(list(ldf=envir$ldf, value=res))
      else return(list(ldf=envir$ldf, value=NA))
    }
  }
}


#' @title Converting an XML document into a relational data model
#'
#' @description Imports  an XML document and converts it into a set of
#'   dataframes each of which represents one table in the data model.
#'
#' @param file The XML document to be processed.
#' @param prefix.primary A prefix for the tables' primary keys (unique numeric
#'   identifier for a data record/row in the table) . Default is \code{"ID_"}.
#'   The primary key field name will consist of the prefix and the table name.
#' @param prefix.foreign A prefix for the tables' foreign keys (). Default is
#'   \code{"FKID_"}. The rest of the foreign key field name will consist of the
#'   prefix and the table name.
#' @param keys.unique Defines if the primary keys must be unique across all
#'   tables of the data model or only within the table of which it is the
#'   primary key. Default is \code{TRUE} (unique across all tables).
#' @param keys.dim Size of the 'key space' reserved for primary keys. Argument
#'   is a power of ten. Default is \code{6} which means the namespace for
#'   primary keys extends from \code{1} to \code{1 million}.
#'
#'
#' @details \code{toRelational()} converts the hierarchical XML structure into a
#'   flat tabular structure with one dataframe for each table in the data model.
#'   \code{toRelational()} determines automatically which XML elements need to
#'   be stored in a separate table. The relationship between the nested objects
#'   in the XML data is recreated in the dataframes with combinations of foreign
#'   and primary keys. The foreign keys refer to the primary keys that
#'   \code{toRelational()} creates automatically when adding XML elements to a
#'   table.
#'   \tabular{llll}{ Column \tab Type \tab Description \tab Example \cr
#'   \code{Style} \tab \code{character} \tab Name of the SQL flavor. \tab
#'   \code{"MySQL"}  \cr \code{NormalField} \tab \code{character} \tab Template
#'   string for a normal, nullable field. \tab \code{"\%FIELDNAME\% \%DATATYPE\%"}
#'   \cr \code{NormalFieldNotNull} \tab \code{character} \tab Template string
#'   for non-nullable field. \tab \code{"\%FIELDNAME\% \%DATATYPE\% NOT NULL"} \cr
#'   \code{PrimaryKey} \tab \code{character} \tab Template string for the
#'   definition of a primary key. \tab \code{"PRIMARY KEY (\%FIELDNAME\%)"} \cr
#'   \code{ForeignKey} \tab \code{character} \tab Template string for the
#'   definition of a foreign key. \tab \code{"FOREIGN KEY (\%FIELDNAME\%) REFERENCES
#'   \%REFTABLE\%(\%REFPRIMARYKEY\%)"}  \cr \code{PrimaryKeyDefSeparate} \tab
#'   \code{logical} \tab Indicates if primary key needs additional definition
#'   like a any other field.  \tab \code{TRUE}  \cr \code{ForeignKeyDefSeparate}
#'   \tab \code{logical} \tab Indicates if foreign key needs additional
#'   definition like a any other field. \tab \code{TRUE} \cr \code{Int} \tab
#'   \tab \code{character} \tab Name of integer data type. \code{"INT"}  \cr
#'   \code{Int.MaxSize} \tab \code{numeric} \tab Size limit of integer data
#'   type.  \tab \code{4294967295}  \cr \code{BigInt} \tab \code{character} \tab
#'   Name of data type for integers larger than the size limit of the normal
#'   integer data type. \tab \code{"BIGINT"} \cr \code{Decimal} \tab
#'   \code{character} \tab Name of data type for floating point numbers. \tab
#'   \code{"DECIMAL"}  \cr \code{VarChar} \tab \code{character} \tab Name of
#'   data type for variable-size character fields. \tab \code{"VARCHAR"}  \cr
#'   \code{VarChar.MaxSize} \tab \code{numeric} \tab Size limit of variable-size
#'   character data type.\tab \code{65535} \cr \code{Text} \tab \code{character}
#'   \tab Name of data type for string data larger than the size limit of the
#'   variable-size character data type. \tab \code{"TEXT"} \cr \code{Date}
#'   \tab \code{character} \tab Name of data type date data. \tab \code{"DATE"}
#'   \cr \code{Time} \tab \code{character} \tab Name of data type time data \tab
#'   \code{"TIME"} \cr \code{Date} \tab \code{character} \tab Name of data
#'   type for combined date and time data. \tab \code{"TIMESTAMP"}  \cr  }
#'
#'   In the template strings you can use the following placeholders, as you also
#'   see from the MySQL example in the table: \enumerate{ \item
#'   \code{\%FIELDNAME\%}: Name of the field to be defined. \item
#'   \code{\%DATATYPE\%}: Datatype of the field to be defined. \item
#'   \code{\%REFTABLE\%}: Table referenced by a foreign key. \item
#'   \code{\%REFPRIMARYKEY\%}: Name of the primary key field of the table
#'   referenced by a foreign key. } When you use your own defintion of an SQL
#'   flavor, then \code{sql.style} must be a one-row dataframe providing the
#'   fields described in the table above.
#'
#'   You can use the \code{datatype.func} argument to provide your own function
#'   to determine how the data type of a field is derived from the values in
#'   that field. In this case, the values of the columns \code{Int},
#'   \code{Int.MaxSize}, \code{VarChar}, \code{VarChar.MaxSize}, \code{Decimal}
#'   and \code{Text} in the \code{sql.style} dataframe are ignored. They are
#'   used by the built-in mechanism to determine data types. Providing your own
#'   function allows you to determine data types in a more differentiated way,
#'   if you like. The function that is provided needs to take a vectors of
#'   values as its argument and needs to provide the SQL data type of these
#'   values as a one-element character vector.
#'
#'
#' @return A list of standard R dataframes, one for each table of the data model. The
#'   tables are named for the elements in the XML document.
#'
#'
#' @examples
#'
#' # Find path to custmers.xml example file in package directory
#' path <- system.file("", "customers.xml", package = "xml2relational")
#' db <- toRelational(path)
#'
#' @family xml2relational
#'
#'
#' @export
toRelational <- function(file, prefix.primary = "ID_", prefix.foreign = "FKID_", keys.unique = TRUE, keys.dim = 6) {
  x <- xml2::read_xml(file)
  p <- xml2::xml_root(x)
  return(parseXMLNode(p, NULL, TRUE, prefix.primary, prefix.foreign, keys.unique, keys.dim)$ldf)
}


check.datetimeformats <- function(vec, funcs, return.convertfunc = FALSE, tz = "UTC") {
  conv <- list()
  for(i in 1:length(funcs)) {
    conv[[length(conv)+1]] <- suppressWarnings(funcs[[i]](vec, tz=tz))
  }
  if(return.convertfunc) {
    if(max(unlist(lapply(conv, function(x){sum(!is.na(x))}))) == NROW(vec[!is.na(vec)])) {
      return(funcs[unlist(lapply(conv, function(x){sum(!is.na(x))})) == NROW(vec[!is.na(vec)])][[1]])
    }
    else {
      return(NULL)
    }
  }
  else {
    return(max(unlist(lapply(conv, function(x){sum(!is.na(x))}))))
  }
}


convertible.datetime <- function(vec, return.convertfunc = FALSE, tz = "UTC") {
  res <- ""
  vec <- as.character(vec)
  has.time <- sum(stringr::str_detect(vec, ":"), na.rm=TRUE) == NROW(vec[!is.na(vec)])
  funcs <- list(lubridate::ymd_hms, lubridate::ymd_hm, lubridate::ymd_h, lubridate::dmy_hms, lubridate::dmy_hm, lubridate::dmy_h, lubridate::mdy_hms, lubridate::mdy_hm, lubridate::mdy_h)
  if(has.time & check.datetimeformats(vec, funcs, FALSE, tz) == NROW(vec[!is.na(vec)])) {
    if(return.convertfunc) res <- check.datetimeformats(vec, funcs, TRUE, tz)
    else res <- "DateTime"
  }
  else {
    funcs <- list(lubridate::ymd, lubridate::dmy, lubridate::mdy)
    if(check.datetimeformats(vec, funcs, FALSE, tz) == NROW(vec[!is.na(vec)])) {
      if(return.convertfunc) res <- check.datetimeformats(vec, funcs, TRUE, tz)
      else res <- "Date"
    }
    else {
      funcs <- list(lubridate::hms, lubridate::hm, lubridate::ms)
      if(has.time & check.datetimeformats(vec, funcs, FALSE, tz) == NROW(vec[!is.na(vec)])) {
        if(return.convertfunc) res <- check.datetimeformats(vec, funcs, TRUE, tz)
        else res <- "Time"
      }
    }
  }
  return(res)
}


convertible.num <- function(vec) {
  vec[vec == ""] <- NA
  return(sum(is.na(vec)) == sum(is.na(suppressWarnings(as.numeric(vec)))))
}


convertible.double <- function(vec) {
  vec <- vec[!is.na(vec)]
  return(!(sum((as.numeric(vec) %% 1 == 0)) == NROW(vec)))
}


convertible.enum <- function(vec, max.ratio = 0.25) {
  vec <- vec[!is.na(vec)]
  if(NROW(vec) > 0) return(NROW(unique(vec))/NROW(vec) <= max.ratio)
  else return(FALSE)
}


is.nullable <- function(vec) {
  return(sum(is.na(vec)) > 0)
}


infer.datatype <- function(vec, bib, sql.style, tz = "UTC") {
  convert.dt <- convertible.datetime(vec, FALSE, tz)
  if(convert.dt != "") {
    return(as.character(bib[bib$Style == sql.style, convert.dt]))
  }
  else {
    if(convertible.num(vec)) {
      # numeric
      if(!convertible.double(vec)) {
        # integer
        vec <- vec[!is.na(vec)]
        max.limit <- bib[bib$Style == sql.style, "Int.MaxSize"]
        if(max(vec) <= max.limit-1 & min(vec) >= (-1)*max.limit) return(as.character(bib[bib$Style == sql.style, "Int"]))
        else return(as.character(bib[bib$Style == sql.style, "BigInt"]))
      }
      else {
        # floating point number
        vec.char <- as.character(vec)
        dec.pos <- stringr::str_locate(vec.char, "\\.")[,2]
        dec.pos.before <- dec.pos - 1
        dec.pos.before[is.na(dec.pos.before)] <- stringr::str_length(vec.char[is.na(dec.pos.before)])
        vec.char.before <- stringr::str_sub(vec.char, 1, dec.pos.before)
        s <- max(stringr::str_length(stringr::str_sub(vec.char, dec.pos + 1, stringr::str_length(vec.char))), na.rm=TRUE)
        p <- max(nchar(vec.char.before)) + s
        return(paste0(bib[bib$Style == sql.style, "Decimal"], "(", p, ",", s, ")"))
      }
    }
    else {
      max.size <- max(nchar(as.character(vec)), na.rm = TRUE)
      if(max.size > bib[bib$Style == sql.style, "VarChar.MaxSize"]) return (as.character(bib[bib$Style == sql.style, "Text"]))
      else return(paste0(bib[bib$Style == sql.style, "VarChar"], "(",max.size, ")"))
    }
  }
}



#' @title Exporting the relational data model and data to a database
#'
#' @description Produces ready-to-run SQL \code{INSERT} statements to import the
#'   data transformed with \code{\link{toRelational}()} into a SQL database.
#'
#' @param ldf A \strong{l}ist of \strong{d}ata\strong{f}rames created by
#'   \code{\link{toRelational}()} (the data tables transformed from XML to a
#'   relational schema).
#' @param sql.style The SQL flavor that the produced \code{CREATE} statements
#'   will follow. The supported SQL styles are \code{"MySQL"},
#'   \code{"TransactSQL"} and \code{"Oracle"}. You can add your own SQL flavor
#'   by providing a dataframe with the required information instead of the name
#'   of one of the predefined SQL flavors as value for \code{sql.style}. See the
#'   Details section for more information on working with different SQL flavors.
#' @param tables A character vector with the names of the tables for whichs SQL
#'   \code{CREATE} statements will be produced. If null (default) \code{CREATE}
#'   statements will be produced for all tables in in the relational data model
#'   of \code{ldf}.
#' @param prefix.primary The prefix that is used in the relational data model of
#'   \code{ldf} to identify primary keys. \code{"ID_"} by default.
#' @param prefix.foreign The prefix that is used in the relational data model of
#'   \code{ldf} to identify foreign keys. \code{"FKID_"} by default.
#' @param line.break Line break character that is added to the end of each
#'   \code{CREATE} statement (apart from the semicolon that is added
#'   automatically). Default is \code{"\n"}.
#' @param datatype.func A function that is used to determine the data type of
#'   the table fields. The function must take the field/column from the data
#'   table (basically the result of \code{SELCT field FROM table})
#'   as its sole argument and return a character vector providing the data type.
#'   If null (default), the built-in mechanism will be used to determine the
#'   data type.
#' @param one.statement Determines whether all \code{CREATE} statements will be
#'   returned as one piece of SQL code (\code{one.statement = TRUE}) or if each
#'   \code{CREATE} statement will be stored in a separate element of the return
#'   vector.
#'
#' @details
#'   If you want to produce SQL \code{CREATE} statements that follow a
#'   different SQL dialect than one of the built-in SQL flavors (i.e. MySQL,
#'   TransactSQL and Oracle) you can provide the necessary information to
#'   \code{getCreateSQL()} via the \code{sql.style} argument. In this case the
#'   \code{sql.style} argument needs to be a dataframe with the folling fields:
#'   \tabular{llll}{ Column \tab Type \tab Description \tab Example \cr
#'   \code{Style} \tab \code{character} \tab Name of the SQL flavor. \tab
#'   \code{"MySQL"}  \cr \code{NormalField} \tab \code{character} \tab Template
#'   string for a normal, nullable field. \tab \code{"\%FIELDNAME\% \%DATATYPE\%"}
#'   \cr \code{NormalFieldNotNull} \tab \code{character} \tab Template string
#'   for non-nullable field. \tab \code{"\%FIELDNAME\% \%DATATYPE\% NOT NULL"} \cr
#'   \code{PrimaryKey} \tab \code{character} \tab Template string for the
#'   definition of a primary key. \tab \code{"PRIMARY KEY (\%FIELDNAME\%)"} \cr
#'   \code{ForeignKey} \tab \code{character} \tab Template string for the
#'   definition of a foreign key. \code{"FOREIGN KEY (\%FIELDNAME\%) REFERENCES
#'   \%REFTABLE\%(\%REFPRIMARYKEY\%)"}  \cr \code{PrimaryKeyDefSeparate} \tab
#'   \code{logical} \tab Indicates if primary key needs additional definition
#'   like a any other field.  \tab \code{TRUE}  \cr \code{ForeignKeyDefSeparate}
#'   \tab \code{logical} \tab Indicates if foreign key needs additional
#'   definition like a any other field. \tab \code{TRUE} \cr \code{Int} \tab
#'   \tab \code{character} \tab Name of integer data type. \code{"INT"}  \cr
#'   \code{Int.MaxSize} \tab \code{numeric} \tab Size limit of integer data
#'   type.  \tab \code{4294967295}  \cr \code{BigInt} \tab \code{character} \tab
#'   Name of data type for integers larger than the size limit of the normal
#'   integer data type. \tab \code{"BIGINT"} \cr \code{Decimal} \tab
#'   \code{character} \tab Name of data type for floating point numbers. \tab
#'   \code{"DECIMAL"}  \cr \code{VarChar} \tab \code{character} \tab Name of
#'   data type for variable-size character fields. \tab \code{"VARCHAR"}  \cr
#'   \code{VarChar.MaxSize} \tab \code{numeric} \tab Size limit of variable-size
#'   character data type.\tab \code{65535} \cr \code{Text} \tab \code{character}
#'   \tab Name of data type for string data larger than the size limit of the
#'   variable-size character data type. \tab \code{"TEXT"} \cr \cr \code{Date}
#'   \tab \code{character} \tab Name of data type date data. \tab \code{"DATE"}
#'   \cr \code{Time} \tab \code{character} \tab Name of data type time data \tab
#'   \code{"TIME"} \cr \code{Date} \tab \code{character} \tab Name of data
#'   type for combined date and time data. \tab \code{"TIMESTAMP"}  \cr  }
#'
#'   In the template strings you can use the following placeholders, as you also
#'   see from the MySQL example in the table: \enumerate{ \item
#'   \code{\%FIELDNAME\%}: Name of the field to be defined. \item
#'   \code{\%DATATYPE\%}: Datatype of the field to be defined. \item
#'   \code{\%REFTABLE\%}: Table referenced by a foreign key. \item
#'   \code{\%REFPRIMARYKEY\%}: Name of the primary key field of the table
#'   referenced by a foreign key. } When you use your own defintion of an SQL
#'   flavor, then \code{sql.style} must be a one-row dataframe providing the
#'   fields described in the table above.
#'
#'   You can use the \code{datatype.func} argument to provide your own function
#'   to determine how the data type of a field is derived from the values in
#'   that field. In this case, the values of the columns \code{Int},
#'   \code{Int.MaxSize}, \code{VarChar}, \code{VarChar.MaxSize}, \code{Decimal}
#'   and \code{Text} in the \code{sql.style} dataframe are ignored. They are
#'   used by the built-in mechanism to determine data types. Providing your own
#'   function allows you to determine data types in a more differentiated way,
#'   if you like. The function that is provided needs to take a vectors of
#'   values as its argument and needs to provide the SQL data type of these
#'   values as a one-element character vector.
#'
#' @return A character vector with exactly one element (if argument
#'   \code{one.statement = TRUE}) or with one element per \code{CREATE}
#'   statement.
#'
#' @examples
#' # Find path to custmers.xml example file in package directory
#' path <- system.file("", "customers.xml", package = "xml2relational")
#' db <- toRelational(path)
#'
#' sql.code <- getCreateSQL(db, "TransactSQL", "address")
#'
#' @family xml2relational
#'
#' @export
getCreateSQL <- function(ldf, sql.style = "MySQL", tables = NULL, prefix.primary = "ID_", prefix.foreign = "FKID_", line.break ="\n", datatype.func = NULL, one.statement = FALSE) {
  sql.stylebib <- data.frame(list(
    Style = c("MySQL", "TransactSQL", "Oracle"),
    NormalField = c("%FIELDNAME% %DATATYPE%","%FIELDNAME% %DATATYPE%","%FIELDNAME% %DATATYPE%"),
    NormalFieldNotNull = c("%FIELDNAME% %DATATYPE% NOT NULL", "%FIELDNAME% %DATATYPE% NOT NULL", "%FIELDNAME% %DATATYPE% NOT NULL"),
    PrimaryKey = c("PRIMARY KEY (%FIELDNAME%)", "%FIELDNAME% %DATATYPE% PRIMARY KEY", "%FIELDNAME% %DATATYPE% PRIMARY KEY"),
    ForeignKey = c("FOREIGN KEY (%FIELDNAME%) REFERENCES %REFTABLE%(%REFPRIMARYKEY%)", "%FIELDNAME% %DATATYPE% REFERENCES %REFTABLE%(%REFPRIMARYKEY%)", "%FIELDNAME% %DATATYPE% REFERENCES %REFTABLE%(%REFPRIMARYKEY%)"),
    PrimaryKeyDefSeparate = c(TRUE, FALSE, FALSE),
    ForeignKeyDefSeparate = c(TRUE, FALSE, FALSE),
    Int = c("INT", "int", "NUMBER"),
    Int.MaxSize = c(2147483648, 2147483648, 1),
    BigInt = c("BIGINT", "bigint", "NUMBER"),
    Decimal = c("DECIMAL", "decimal", "NUMBER"),
    VarChar = c("VARCHAR", "varchar", "VARCHAR2"),
    VarChar.MaxSize = c(65535, 8000, 4000),
    Text = c("TEXT", "varchar(max)", "LONG"),
    Date = c("DATE", "date", "DATE"),
    DateTime = c("TIMESTAMP", "datetime2", "TIMESTAMP"),
    Time = c("TIME", "TIME", "VARCHAR2(20)")
  ), stringsAsFactors = FALSE)

  if(is.data.frame(sql.style)) {
    sql.stylebib <- rbind(sql.stylebib, sql.style)
    sql.style = sql.stylebib[1,1]
  }
  else {
    if(!sql.style %in% sql.stylebib[,1]) stop(paste0("'", sql.style, "' is not a valid SQL flavor. Valid flavors are ",
                                                     paste0("'", sql.stylebib[,1], "'", collapse = ",")), ".\n")
  }

  if(is.null(tables)) tabs <- 1:length(ldf)
  else {
    tabs <- c()
    for(i in 1:NROW(tables)) {
      if(tables[i] %in% names(ldf)) {
        tabs <- append(tabs, get.df(ldf, tables[i]))
      }
      else warning(paste0("Table '", tables[i], "' does not exist in your data model. Valid tables names are ",
                          paste0("'", names(ldf), "'", collapse = ",")), ".\n")
    }
  }

  sql.code <- c()
  for(i in 1:NROW(tabs)) {
    df <- ldf[[get.df(ldf, names(ldf)[tabs[i]])]]
    df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
    sql.code[i] <- paste0("CREATE TABLE ", names(ldf)[tabs[i]], " (", line.break)
    for(f in 1:NCOL(df)) {
      if(f != 1) sql.code[i] <- paste0(sql.code[i], ", ")
      if(is.null(datatype.func)) datatype <- infer.datatype(df[,f], sql.stylebib, sql.style)
      else datatype = datatype.func(df[,f])
      field <- names(df)[f]
      reftable <- ""
      if(field==paste0(prefix.primary, names(ldf)[tabs[i]])) {
        # primary
        sql.code[i] <- paste0(sql.code[i], sql.stylebib[sql.stylebib$Style==sql.style, "PrimaryKey"])
        if(sql.stylebib[sql.stylebib$Style==sql.style, "PrimaryKeyDefSeparate"])
          sql.code[i] <- paste0(sql.code[i], line.break, ", ", sql.stylebib[sql.stylebib$Style==sql.style, "NormalField"])
      }
      else {
        if(stringr::str_sub(field, 1, nchar(prefix.foreign)) == prefix.foreign) {
          # foreign
          reftable <- stringr::str_replace_all(field, prefix.foreign, "")
          sql.code[i] <- paste0(sql.code[i], sql.stylebib[sql.stylebib$Style==sql.style, "ForeignKey"])
          if(sql.stylebib[sql.stylebib$Style==sql.style, "ForeignKeyDefSeparate"])
            sql.code[i] <- paste0(sql.code[i], line.break, ", ", sql.stylebib[sql.stylebib$Style==sql.style, "NormalField"])
        }
        else {
          # normal
          if(is.nullable(df[,f])) {
            # nullable
            sql.code[i] <- paste0(sql.code[i], sql.stylebib[sql.stylebib$Style==sql.style, "NormalField"])
          }
          else {
            # not nullable
            sql.code[i] <- paste0(sql.code[i], sql.stylebib[sql.stylebib$Style==sql.style, "NormalFieldNotNull"])
          }
        }
      }
      sql.code[i] <- stringr::str_replace_all(sql.code[i], "%FIELDNAME%", field)
      sql.code[i] <- stringr::str_replace_all(sql.code[i], "%DATATYPE%", datatype)
      sql.code[i] <- stringr::str_replace_all(sql.code[i], "%REFTABLE%", reftable)
      sql.code[i] <- stringr::str_replace_all(sql.code[i], "%REFPRIMARYKEY%", paste0(prefix.primary, reftable))
      sql.code[i] <- paste0(sql.code[i], line.break)
    }
    sql.code[i] <- paste0(sql.code[i], ");")
  }

  if(one.statement) sql.code <- paste0(sql.code, collapse = line.break)
  return(sql.code)
}



#' @title Exporting the relational data model and data to a database
#'
#' @description Produces ready-to-run SQL \code{INSERT} statements to import the
#'   data transformed with \code{\link{toRelational}()} into a SQL database.
#'
#' @param ldf A \strong{l}ist of \strong{d}ata\strong{f}rames created by
#'   \code{\link{toRelational}()} (the data tables transformed from XML to a
#'   relational schema).
#' @param table.name Name of the table from the data table list \code{ldf} for
#'   which \code{INSERT} statements are to be created.
#' @param line.break Line break character that is added to the end of each
#'   \code{INSERT} statement (apart from the semicolon that is added
#'   automatically). Default is \code{"\n"}.
#' @param one.statement Determines whether all \code{INSERT} statements will be
#'   returned as one piece of SQL code (\code{one.statement = TRUE}) or if each
#'   \code{INSERT} statement will be stored in a separate element of the return
#'   vector. In the former case the return vector will have just one element, in
#'   the latter case as many elements as there are data records to insert.
#'   Default is \code{FALSE} (return vector has one element per \code{INSERT}
#'   statement.
#' @param tz The code of the timezone used for exporting timestamp data. Default it
#'   \code{"UTC"} (Coordinated Universal Time).
#'
#' @return A character vector with exactly one element (if argument
#'   \code{one.statement = TRUE}) or with one element per \code{INSERT}
#'   statement.
#'
#' @examples
#' # Find path to custmers.xml example file in package directory
#' path <- system.file("", "customers.xml", package = "xml2relational")
#' db <- toRelational(path)
#'
#' sql.code <- getInsertSQL(db, "address")
#'
#' @family xml2relational
#'
#' @export
getInsertSQL <- function(ldf, table.name, line.break = "\n", one.statement = FALSE, tz = "UTC") {
  if(!table.name %in% names(ldf)) stop(paste0("Table '", table.name, "' does not exist in your data model. Valid tables names are ",
                                              paste0("'", names(ldf), "'", collapse = ",")), ".\n")
  tab <- ldf[[get.df(ldf, table.name)]]
  col.delimiter <- c()
  cols <- c()
  res <- c()
  for(f in 1:NCOL(tab)) {
    if(convertible.datetime(tab[,f], return.convertfunc = FALSE, tz=tz) != "") tab[,f] <- as.character(convertible.datetime(tab[,f], return.convertfunc = TRUE, tz=tz)(tab[,f]))

    if(!convertible.num(tab[,f])) col.delimiter[f] <- "'"
    else col.delimiter[f] <- ""
    cols <- append(cols, names(tab)[f])
  }
  cols <- paste0(names(tab), collapse = ", ")

  for(i in 1:NROW(tab)) {
    vals <- c()
    for(f in 1:NCOL(tab)) {
      if(!is.na(tab[i,f])) vals <- paste0(vals, ", ", col.delimiter[f], tab[i,f], col.delimiter[f])
      else vals <- paste0(vals, ", NULL")
    }
    vals <- stringr::str_sub(vals, 3, stringr::str_length(vals))
    res <- append(res, paste0("INSERT INTO ", table.name, "(", cols, ") VALUES (", vals, ");"))
  }
  if(one.statement) res <- paste0(res, collapse = line.break)
  return(res)
}



#' @title Saving the relational data
#'
#' @description Saves a list of dataframes created from an XML source with
#'   \code{\link{toRelational}()} to CSV files, one file per dataframe (i.e.
#'   table in the relational data model). File names are identical to the
#'   dataframe/table names.
#'
#' @param ldf A \strong{l}ist of \strong{d}ata\strong{f}rames created by
#'   \code{\link{toRelational}()} (the data tables transformed from XML to a
#'   relational schema).#' @param dir Directory where the files will be stored.
#'   Default is the current working directory.
#' @param dir The directory to save the CSV files in. Per default the working directory.
#' @param sep Character symbol to separate fields in the CSV fil, comma by
#'   default.
#' @param dec Decimal separator used for numeric fields in the CSV file, point
#'   by default.
#'
#' @return No return vaue.
#'
#' @examples
#' # Find path to custmers.xml example file in package directory
#' path <- system.file("", "customers.xml", package = "xml2relational")
#' db <- toRelational(path)
#'
#' savetofiles(db, dir = tempdir())
#'
#'
#' @family xml2relational
#'
#' @export
savetofiles <- function(ldf, dir, sep = ",", dec = ".") {
  if(dir != "") {
    if(!dir.exists(dir)) stop(paste0("Directory '", dir, "' does not exist."))
  }
  for(i in 1:length(ldf)) {
    tab <- ldf[[i]]
    for(f in 1:NCOL(tab)) {
      if(convertible.num(tab[,f])) tab[,f] <- as.numeric(tab[,f])
      else tab[,f] <- as.character(tab[,f])
    }
    utils::write.table(tab, file=fs::path(dir, paste0(names(ldf)[i], ".csv")), dec = dec, sep = sep)
  }
}
