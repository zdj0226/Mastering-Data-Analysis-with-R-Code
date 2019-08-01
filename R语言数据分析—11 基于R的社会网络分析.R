library(tools)
pkgs<-available.packages()
str(pkgs)
head(package.dependencies(pkgs),2)
library(plyr)
edges<-ldply(
  c('Depends','Imports','Suggests'),function(depLevel){
  deps<-package.dependencies(pkgs,depLevel=depLevel)
  ldply(names(deps),function(pkg)
    if (!identical(deps[[pkg]],NA))
      data.frame(
        src=pkg,
        dep=deps[[pkg]][,1],
        label=depLevel,
        stringsAsFactors=FALSE))
})
str(edges)
nrow(edges)/(nrow(pkgs)*(nrow(pkgs)-1))
head(sort(table(edges$dep),decreasing = TRUE))
edges<-edges[edges$dep!='R',]
library(igraph)
g<-graph.data.frame(edges)
summary(g)
graph.density(g)
head(sort(degree(g),decreasing = TRUE))
head(sort(betweenness(g),decreasing = TRUE))
plot(degree(g),betweenness(g),type='n',
     main='Centrality of R package dependencies')
text(degree(g),betweenness(g),labels = V(g)$name)
edges<-edges[edges$label!='Suggests',]
deptree<-edges$dep[edges$src=='igraph']
while(!all(edges$dep[edges$src %in% deptree]%in% deptree))
  deptree<-union(deptree,edges$dep[edges$src %in% deptree])
deptree
g<-graph.data.frame(edges[edges$src %in% c('igraph',deptree),])
plot(g)
V(g)$label.color<-'orange'
V(g)$label.color[V(g)$name=='igraph']<-'darkred'
V(g)$label.color[V(g)$name %in% edges$dep[edges$src=='igraph']]<-'orangered'
E(g)$color<-c('blue','green')
plot(g,vertex.shape='none',edge.label=NA)
tkplot(g,edge.label=NA)
library(visNetwork)
nodes<-get.data.frame(g,'vertices')
names(nodes)<-c('id','color')
edges<-get.data.frame(g)
visNetwork(nodes,edges)
g<-dominator.tree(g,root = "igraph")$domtree
plot(g,layout=layout.reingold.tilford(g,root="igraph"),
     vertex.shapes='none')
library(miniCRAN)
pkgs<-pkgAvail()
pkgDep('igraph',availPkgs=pkgs,suggests=FALSE,
       includeBasePkgs=TRUE)
plot(makeDepGraph('igraph',pkgs,suggests=FALSE,
      includeBasePkgs=TRUE))
