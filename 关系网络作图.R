graph.adjacency <- function (adjmatrix, mode = c("directed", "undirected", "max", 
    "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE, 
    add.colnames = NULL, add.rownames = NA) {
    if (inherits(adjmatrix, "Matrix")) {                            #判断输入是否为矩阵
        res <- graph.adjacency.sparse(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    else {
        res <- graph.adjacency.dense(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    if (is.null(add.colnames)) {
        if (!is.null(colnames(adjmatrix))) {
            add.colnames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.colnames)) {
        if (is.null(colnames(adjmatrix))) {
            warning("No column names to add")
            add.colnames <- NA
        }
    }
    if (is.null(add.rownames)) {
        if (!is.null(rownames(adjmatrix))) {
            add.rownames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.rownames)) {
        if (is.null(rownames(adjmatrix))) {
            warning("No row names to add")
            add.rownames <- NA
        }
    }
    if (!is.na(add.rownames) && !is.na(add.colnames) && add.rownames == 
        add.colnames) {
        warning("Same attribute for columns and rows, row names are ignored")
        add.rownames <- NA
    }
    if (!is.na(add.colnames)) {
        res <- set_vertex_attr(res, add.colnames, value = colnames(adjmatrix))
    }
    if (!is.na(add.rownames)) {
        res <- set_vertex_attr(res, add.rownames, value = rownames(adjmatrix))
    }
    res
}

simplify <- function (graph, remove.multiple = TRUE, remove.loops = TRUE, 
    edge.attr.comb = igraph_opt("edge.attr.comb")) {
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    remove.multiple <- as.logical(remove.multiple)
    remove.loops <- as.logical(remove.loops)
    edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_simplify", graph, remove.multiple, 
        remove.loops, edge.attr.comb, PACKAGE = "igraph")
    res
}

V <- function (graph) {
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    update_vs_ref(graph)
    res <- seq_len(vcount(graph))
    if (is_named(graph)) 
        names(res) <- vertex_attr(graph)$name
    class(res) <- "igraph.vs"
    add_vses_graph_ref(res, graph)
}


fetch_db_data <- function(db_type=RMySQL::MySQL(), 
                          con_info = list(user = '',
                                          pd = '',
                                          dbname = '',
                                          host = ''), 
                          group_cnf=' ', db_name = '', sql.str, n_data = -1){
  require(DBI)
  if(group_cnf==' ') db.con <- dbConnect(db_type,  # for normal case
                                         user = con_info$user,
                                         password = con_info$pd,
                                         dbname = con_info$dbname,
                                         host = con_info$host)
  else db.con <- dbConnect(db_type, group=group_cnf, dbname=db_name) # for via ssh situation  
  dbSendQuery(db.con, 'SET NAMES utf8')
  res <- dbSendQuery(db.con, sql.str)
  result <- dbFetch(res, n = n_data)
  dbClearResult(res)
  dbDisconnect(db.con)
  return(result)
}
fetch_db_data(MySQL(),con_info=user1,group_cnf=" ",sql.str="SELECT * FROM df")
dbListConnections(MySQL())
get_db_data("pythontest","SELECT * FROM df")
dbListConnections(MySQL())
user1=list(user="analyzer",pd="analyzer@tbs2016",dbname="pythontest",host="127.0.0.1")
get_db_data<-function(dbname="pythontest",sql.str){
  db.con <- dbConnect(MySQL(),  
                   user = "shiny",
                   password = "shiny@tbs2016",
                   dbname = dbname,
                   host = "127.0.0.1")
  dbSendQuery(db.con, 'SET NAMES utf8')
  res <- dbSendQuery(db.con, sql.str)
  result <- dbFetch(res, n = -1)
  dbClearResult(res)
  dbDisconnect(db.con)
  return(result)
}


