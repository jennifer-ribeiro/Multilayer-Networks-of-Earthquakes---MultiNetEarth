library(igraph)

###### 1- CRIAR REDES A PARTIR DE DADOS DE ENTRADA DE VERTICES E ARESTAS ###### 
Layers = 6
years = c("2004","2005","2006","2007","2008","2009")  #Labels a serem usados nas camadas de anos
min_weight = 1  #limite minimo de peso das arestas a ser considerado

##### 1.1 - Ler dados dos Vertices #####
vertices <- list()
for (i in 1:Layers) {
  vertices[[i]] <- read.csv(file.choose(), header = TRUE)  # escolher com arquivo os dados dos vertices 
}

if(Layers == 1){
  vert_TOTAL <- vertices[[1]]
}else if(Layers == 2){
  vert_TOTAL <- unique(rbind(vertices[[1]], vertices[[2]]))  # Cria dataframe de vertices contendo informacoes
                                                             # de TODOS os vertices sem repeticao
}else if(Layers > 2){
    vert_TOTAL <- unique(rbind(vertices[[1]], vertices[[2]])) # Cria dataframe de vertices contendo informacoes
                                                                # de TODOS os vertices sem repeticao 
    for (i in 3:Layers) {
      vert_TOTAL <- unique(rbind(vert_TOTAL, vertices[[i]]))
    }
} 
#####

##### 1.1 - Ler dados das Arestas e Criar Rede #####
edges <- list()
g.list <- list()
for (i in 1:Layers) {
  edges[[i]] <- read.csv(file.choose(), header = TRUE)  # escolher com arquivo os dados de conexoes
  #vertices[[i]] <- read.csv(file.choose(), header = TRUE)  # escolher com arquivo os dados dos vertices 
  # edges
  # vertices
  g.list[[i]] <- graph_from_data_frame(edges[[i]], directed=FALSE, vertices=vert_TOTAL$ID)
  #net
  
  ## OUTRA FORMA (PIOR !)
  # g.list[[i]] <- graph.data.frame(edges,directed = TRUE) # cria a rede e escolhe direcionada ou nao 
  # g.list
  ####
  
  #### 1.1.1- Transformar arestas paralelas em pesos da aresta (rede ponderada) ####
  E(g.list[[i]])$weight <- 1  #cria uma coluna "weight" onde todos tem peso 1
  sub_net_weight <- igraph::simplify(g.list[[i]], remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb=list(weight="sum")) #soma os pesos para cada ligacao paralela
  # E(sub_net_weight)$weight
  # View(as_data_frame(sub_net_weight, what = c("edges")))  # "edges" ou "vertices" ou "both"
  # vcount(sub_net_weight)
  # ecount(sub_net_weight)
  ########
  
  #### 1.1.2- Deletar edges que possuam peso menor que "min_weight" ####
  g.list[[i]] <- delete_edges(sub_net_weight, which( (E(sub_net_weight)$weight) < min_weight) )
  ########
  
  ##### 1.1.3- Simplificar a Rede ####
  g.list[[i]] <- igraph::simplify(g.list[[i]], remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list("ignore"))
  ########
}

#vert_TOTAL <- unique(rbind(vertices[[1]], vertices[[2]])) # Cria dataframe de vertices contendo informacoes
                                                          # de TODOS os vertices sem repeticao
#vert_TOTAL <- unique(rbind(vert_TOTAL, vertices[[3]])) 

# vert_TOTAL <- vert_TOTAL[order(vert_TOTAL$ID),]


# vert_TOTAL <- list()
# vert_TOTAL[[1]] <- unique(rbind(vertices[[1]], vertices[[2]]))
# vert_TOTAL[[2]] <- unique(rbind(vertices[[2]], vertices[[1]]))

################################################################################


####### 2- ACHAR E ADICIONAR VERTICES QUE EXISTEM APENAS EM UMA CAMADA E NAO NAS OUTRAS #######

#### 2.1- Achar vertices que existem somente na Camada 1 e adiciona-los nas outras ####

all.equal(V(g.list[[1]])$name,V(g.list[[2]])$name)
# V(g.list[[1]])[which(!(names(V(g.list[[1]])) %in% names(V(g.list[[2]]))))]  # Fornece os nomes dos vertices que estao em g1 mas nao estao em g2

all.equal(V(g.list[[1]])$name,V(g.list[[3]])$name)

g.list[[2]] <- g.list[[2]] + vertices(V(g.list[[1]])$name[!(V(g.list[[1]])$name %in% V(g.list[[2]])$name)])

g.list[[3]] <- g.list[[3]] + vertices(V(g.list[[1]])$name[!(V(g.list[[1]])$name %in% V(g.list[[3]])$name)])

########

#### 2.2- Achar vertices que existem somente na Camada 2 e adiciona-los nas outras ####

all.equal(V(g.list[[2]])$name,V(g.list[[1]])$name)
# V(g.list[[2]])[which(!(names(V(g.list[[2]])) %in% names(V(g.list[[1]]))))]  # Fornece os nomes dos vertices que estao em g1 mas nao estao em g2

g.list[[1]] <- g.list[[1]] + vertices(V(g.list[[2]])$name[!(V(g.list[[2]])$name %in% V(g.list[[1]])$name)])

g.list[[3]] <- g.list[[3]] + vertices(V(g.list[[2]])$name[!(V(g.list[[2]])$name %in% V(g.list[[3]])$name)])

########

#### 2.3 - Achar vertices que existem somente na Camada 3 e adiciona-los nas outras ####

all.equal(V(g.list[[1]])$name,V(g.list[[3]])$name)

g.list[[1]] <- g.list[[1]] + vertices(V(g.list[[3]])$name[!(V(g.list[[3]])$name %in% V(g.list[[1]])$name)])

g.list[[2]] <- g.list[[2]] + vertices(V(g.list[[3]])$name[!(V(g.list[[3]])$name %in% V(g.list[[2]])$name)])


#### 2.4- TESTE: are nodes equal (same length and order) in all layers? ####

all.equal(V(g.list[[1]])$name,V(g.list[[2]])$name)

all.equal(V(g.list[[1]])$name,V(g.list[[3]])$name)

all.equal(V(g.list[[2]])$name,V(g.list[[3]])$name)
####
########

#### 2.5- Criar Tensores (Matriz Adjacencia) de Ligacoes para cada Camada contendo Todos os Vertices ####
nodeTensor <- list()
nodeTensor[[1]] <- igraph::get.adjacency(g.list[[1]])
nodeTensor[[1]] <- nodeTensor[[1]][order(rownames(nodeTensor[[1]])),order(colnames(nodeTensor[[1]]))]
nodeTensor[[2]] <- igraph::get.adjacency(g.list[[2]])
nodeTensor[[2]] <- nodeTensor[[2]][order(rownames(nodeTensor[[2]])),order(colnames(nodeTensor[[2]]))]
nodeTensor[[3]] <- igraph::get.adjacency(g.list[[3]])
nodeTensor[[3]] <- nodeTensor[[3]][order(rownames(nodeTensor[[3]])),order(colnames(nodeTensor[[3]]))]

####

#### 2.5.1: are colnames and rownames equal (same length and order) in all layers?

all.equal(rownames(nodeTensor[[1]]),rownames(nodeTensor[[2]]))
all.equal(colnames(nodeTensor[[1]]),colnames(nodeTensor[[2]]))

all.equal(rownames(nodeTensor[[1]]),rownames(nodeTensor[[3]]))
all.equal(colnames(nodeTensor[[1]]),colnames(nodeTensor[[3]]))
####
########


####### 3- CRIAR LAYOUT PARA AS REDES DAS CAMADAS #######
library(assertthat)
library(purrr)
library(ggplot2)
library(ggraph)
library(ggmap)

lay_def <- igraph::layout_with_fr(g.list[[1]])

lay_def <- create_layout(g.list[[1]], 
                     layout = 'manual',
                     #node.positions = node_pos,
                     x = vert_TOTAL$longitude, y = vert_TOTAL$latitude
                     )

assert_that(nrow(lay_def) == nrow(vert_TOTAL))

#########################################################

####### 4- CRIAR FUNCAO PARA GERAR VISUALIZACAO 3D DAS REDES DAS CAMADAS #######
library(muxViz)
library(rgl)

#' 3D plot of a multiplex
#'
#' @param g.list list of networks (representing the layers)
#' @param layer.colors colors of the layers (mandatory)
#' @param as.undirected default TRUE
#' @param layer.layout default "auto",
#' @param layer.labels default "auto",
#' @param layer.labels.cex default 2,
#' @param edge.colors default "auto",
#' @param edge.normalize default F,
#' @param edge.size.scale default 1,
#' @param node.colors default "auto",
#' @param node.size.values default 0.5,  # se colocar "auto": tamanho dos vertices = 3 x (node.size.scale x) x (strength)^1/2
#' @param node.size.scale default 1,
#' @param node.alpha default 1,
#' @param edge.alpha default 1,
#' @param layer.alpha default "auto",
#' @param layout default "fr", see \link[igraph]{layout_with_fr} for other options.
#' @param show.nodeLabels default F,
#' @param show.aggregate default F,
#' @param aggr.alpha default "auto",
#' @param aggr.color default to hex-color "#dadada",
#' @param node.colors.aggr default to hex-color "#dadada",
#' @param layer.scale default 2,
#' @param layer.shift.x default 0,
#' @param layer.shift.y default 0,
#' @param layer.space default 1.5,
#' @param FOV default 30
#' @param box default TRUE
# #' @param ggplot.format default FALSE
#' @return a plot
#' @importFrom rgl rgl.clear bg3d quads3d text3d par3d
#' @export 
plot_multiplex3Doug <-
  function(g.list,
           layer.colors,
           as.undirected = T,
           layer.layout = "auto",
           layer.labels = "auto",
           layer.labels.cex = 2,
           edge.colors = "auto",
           edge.normalize = F,
           edge.size.scale = 1,
           node.colors = "auto",
           node.size.values = 0.5,
           node.size.scale = 1,
           node.alpha = 1,
           edge.alpha = 1,
           layer.alpha = "auto",
           layout = "fr",
           show.nodeLabels = F,
           show.aggregate = F,
           aggr.alpha = "auto",
           aggr.color = "#dadada",
           node.colors.aggr = "#dadada",
           layer.scale = 2,
           layer.shift.x = 0,
           layer.shift.y = 0,
           layer.space = 1.5,
           FOV = 30,
           box = T,
           ggplot.format = T) {
    # Generate a 3D visualization of the multiplex network
    
    # Arguments can be either "auto" or NA in most cases
    
    #' If *node.colors* is a matrix Nodes x Layers, the color of each node can be assigned 
    #' If [node.size.scale] is a vector of size Layers, each layer will be scaled independently 
    #' If @edge.size.scale is a vector of size Layers, each layer will be scaled independently
    #' If *show.aggregate* is true, then *node.colors.aggr* could be set as well 
    
    
    mypal <- layer.colors
    
    Layers <- length(g.list)
    Nodes <- igraph::vcount(g.list[[1]])
    
    if (!is.matrix(layer.layout) && layer.layout == "auto") {
      lay <-
        layoutMultiplex(g.list,
                        layout = layout,
                        ggplot.format = F,
                        box = T)
    } else if(layer.layout == "manual"){
      #lay <- igraph::layout_with_fr(g.list[[1]])
      
      # lay <- create_layout(g.list[[1]], 
      #                      layout = 'manual',
      #                      #node.positions = node_pos,
      #                      x = vert_TOTAL$longitude, y = vert_TOTAL$latitude)

      lay <- lay_def
      
      layout <- list()
      for (l in 1:Layers) {
        layout[[l]] <- create_layout(g.list[[l]], layout = 'drl')
        layout[[l]]$x <- lay[, 1]
        layout[[l]]$y <- lay[, 2]
      }
      
      if (box) {
        lay[, 1] <-
          2 * (lay[, 1] - min(lay[, 1])) / (max(lay[, 1]) - min(lay[, 1])) - 1
        lay[, 2] <-
          2 * (lay[, 2] - min(lay[, 2])) / (max(lay[, 2]) - min(lay[, 2])) - 1
      }
      
      # if (ggplot.format) {
      #   layout.mux <- list()
      #   for (l in 1:length(g.list)) {
      #     #fictitious, we need it just to quickly format the data frame
      #     layout.df <-
      #       ggraph::create_layout(g.list[[l]], layout = 'circle')
      #     layout.df$x <- lay[, 1]
      #     layout.df$y <- lay[, 2]
      #     layout.mux[[l]] <- layout.df
      #   }
      #   #return(layout.mux)
      # } else {
      #   return(lay)
      # }
    }
    else {
      lay <- layer.layout
    }
    
    if (layer.alpha == "auto") {
      layer.alpha <- rep(0.5, Layers)
    }
    if (is.na(layer.labels) || is.null(layer.labels)) {
      layer.labels <- NA
    } else {
      if (layer.labels == "auto" || length(layer.labels) != Layers) {
        layer.labels <- paste("Layer", 1:Layers)
      }
      if (show.aggregate &&
          (!is.na(layer.labels) && !is.null(layer.labels))) {
        layer.labels <- c(layer.labels, "Aggregate")
      }
    }
    
    if (length(node.size.scale) == 1) {
      node.size.scale <- rep(node.size.scale, Layers)
    }
    
    if (length(edge.size.scale) == 1) {
      edge.size.scale <- rep(edge.size.scale, Layers)
    }
    
    LAYER_SCALE <- layer.scale
    LAYER_SHIFT_X <- layer.shift.x
    LAYER_SHIFT_Y <- layer.shift.y
    LAYER_SPACE <- layer.space
    
    PLOT_FOV <- FOV
    d <- 0
    
    rgl.clear()
    bg3d(col = "white")
    
    for (l in 1:Layers) {
      if (as.undirected) {
        g.list[[l]] <- igraph::as.undirected(g.list[[l]])
      }
      
      if (node.size.values == "auto") {
        igraph::V(g.list[[l]])$size <-
          3 * node.size.scale[l] * sqrt(igraph::strength(g.list[[l]]))
      } else {
        igraph::V(g.list[[l]])$size <- node.size.values * node.size.scale[l]
      }
      
      if (!is.matrix(node.colors)) {
        if (node.colors == "auto") {
          node.col <- layer.colors[l]
        } else {
          node.col <- node.colors
        }
        igraph::V(g.list[[l]])$color <- node.col
      } else {
        igraph::V(g.list[[l]])$color <- node.colors[, l]
      }
      
      if (show.nodeLabels) {
        igraph::V(g.list[[l]])$label <- 1:igraph::gorder(g.list[[l]])
      } else {
        igraph::V(g.list[[l]])$label <- NA
      }
      
      if (edge.colors == "auto") {
        edge.col <- layer.colors[l]
      } else {
        edge.col <- edge.colors
      }
      igraph::E(g.list[[l]])$color <- edge.col
      
      if (!is.null(igraph::E(g.list[[l]])$weight)) {
        igraph::E(g.list[[l]])$width <- igraph::E(g.list[[l]])$weight
      } else {
        igraph::E(g.list[[l]])$width <- 1
      }
      
      if (edge.normalize) {
        igraph::E(g.list[[l]])$width <-
          edge.size.scale[l] * log(1 + igraph::E(g.list[[l]])$width) / max(log(1 + igraph::E(g.list[[l]])$width))
      }
      
      if (show.aggregate) {
        d <- -1 + LAYER_SCALE * LAYER_SPACE * l / (Layers + 1)
      } else {
        d <- -1 + LAYER_SCALE * LAYER_SPACE * l / Layers
      }
      #print(d)
      
      layout.layer <- matrix(0, nrow = Nodes, ncol = 3)
      layout.layer[, 1] <- lay[, 1] + (l - 1) * LAYER_SHIFT_X
      layout.layer[, 2] <- lay[, 2] + (l - 1) * LAYER_SHIFT_Y
      layout.layer[, 3] <- d
      
      x <-
        c(-1, -1, -1 + LAYER_SCALE, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_X
      y <-
        c(-1 + LAYER_SCALE, -1, -1, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_Y
      z <- c(d, d, d, d)
      quads3d(x,
              y,
              z,
              alpha = layer.alpha[[l]],
              col = layer.colors[[l]],
              add = T)
      
      igraph::rglplot(g.list[[l]], layout = layout.layer,
                      rescale = F)
      
      if (!is.na(layer.labels) && !is.null(layer.labels)) {
        text3d(
          -1 + (l - 1) * LAYER_SHIFT_X,
          -1 + (l - 1) * LAYER_SHIFT_Y,
          d + 0.1,
          text = layer.labels[l],
          adj = 0.2,
          color = "black",
          family = "sans",
          cex = layer.labels.cex
        )
      }
    }
    
    if (show.aggregate) {
      g.aggr <- GetAggregateNetworkFromNetworkList(g.list)
      
      if (node.size.values == "auto") {
        igraph::V(g.aggr)$size <- 3 * node.size.scale[l] * sqrt(igraph::strength(g.aggr))
      } else {
        igraph::V(g.aggr)$size <- node.size.values * node.size.scale[l]
      }
      
      igraph::V(g.aggr)$color <- node.colors.aggr
      
      if (show.nodeLabels) {
        igraph::V(g.aggr)$label <- 1:igraph::gorder(g.aggr)
      } else {
        igraph::V(g.aggr)$label <- NA
      }
      
      igraph::E(g.aggr)$color <- aggr.color
      
      if (!is.null(igraph::E(g.aggr)$weight)) {
        igraph::E(g.aggr)$width <- igraph::E(g.aggr)$weight
      } else {
        igraph::E(g.aggr)$width <- 1
      }
      
      l <- Layers + 1
      d <- -1 + LAYER_SCALE * LAYER_SPACE * l / (Layers + 1)
      layout.layer <- matrix(0, nrow = Nodes, ncol = 3)
      layout.layer[, 1] <- lay[, 1] + (l - 1) * LAYER_SHIFT_X
      layout.layer[, 2] <- lay[, 2] + (l - 1) * LAYER_SHIFT_Y
      layout.layer[, 3] <- d
      
      x <-
        c(-1, -1, -1 + LAYER_SCALE, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_X
      y <-
        c(-1 + LAYER_SCALE, -1, -1, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_Y
      z <- c(d, d, d, d)
      
      if (aggr.alpha == "auto") {
        quads3d(x,
                y,
                z,
                alpha = 0.5,
                col = aggr.color,
                add = T)
      } else {
        quads3d(x,
                y,
                z,
                alpha = aggr.alpha,
                col = aggr.color,
                add = T)
      }
      
      igraph::rglplot(g.aggr, layout = layout.layer,
                      rescale = F)
      
      if (!is.na(layer.labels) && !is.null(layer.labels)) {
        text3d(
          -1 + (l - 1) * LAYER_SHIFT_X,
          -1 + (l - 1) * LAYER_SHIFT_Y,
          d + 0.1,
          text = "Aggregate",
          adj = 0.2,
          color = "black",
          family = "sans",
          cex = layer.labels.cex
        )
      }
      
    }
    
    
    M <- matrix(0, ncol = 4, nrow = 4)
    M[1, ] <- c(0.54, 0, 0.84, 0)
    M[2, ] <- c(0.33, 0.92, -0.22, 0)
    M[3, ] <- c(-0.77, 0.39, 0.5, 0)
    M[4, ] <- c(0, 0, 0, 1)
    
    par3d(FOV = PLOT_FOV, userMatrix = M)
  }

#########################################################

####### 5- PLOTAR REDES EM 3D DAS CAMADAS #######
library(RColorBrewer)
#display.brewer.all()
mypal <- brewer.pal(Layers, "Set2")

plot_multiplex3Doug(g.list,
                    layer.colors=mypal,
                    as.undirected = T,
                    layer.layout = "manual",
                    layer.labels = years,
                    layer.labels.cex = 2,
                    edge.colors = "auto",
                    edge.normalize = F,
                    edge.size.scale = 1,
                    node.colors = "auto",
                    node.size.values = "auto",
                    node.size.scale = 0.7,
                    node.alpha = 1,
                    edge.alpha = 1,
                    layer.alpha = "auto",
                    layout = "fr",
                    show.nodeLabels = F,
                    show.aggregate = F,
                    aggr.alpha = "auto",
                    aggr.color = "#dadada",
                    node.colors.aggr = "#dadada",
                    layer.scale = 2,
                    layer.shift.x = 0,
                    layer.shift.y = 0,
                    layer.space = 3.5,
                    FOV = 30,
                    #ggplot.format = T,
                    box = T
                    )

#################################################


