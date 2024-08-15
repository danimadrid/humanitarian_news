library(pacman)
p_load(DiagrammeR, DiagrammeRsvg, rsvg)

graph <- DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR, compound=true]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

subgraph cluster_0 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Linen
    
        label = 'Corpus Building'
        node[margin = 0.2]
        data1 [label = 'GDELT', shape = tab, fillcolor = Beige]
        data2 [label = 'Google \n News API', shape = tab, fillcolor = Beige]
        data3 [label = 'Factiva', shape = tab, fillcolor = Beige]
        data4 [label = 'Lexis \n Nexis', shape = tab, fillcolor = Beige]
        data5 [label = 'Google \n Search', shape = tab, fillcolor = Beige]
}
 
subgraph cluster_1 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Linen
        
        label = 'Estimating Embeddings'
        node[margin = 0.2]
        embed1 [label = 'Generate word  \n embeddings (GloVe approach)', fillcolor = Beige]
        embed2 [label = 'Keywords identified \n by experts', fillcolor = Beige]
        embed2a [label = 'Compute cosine \n similarities between \n &lsquo;humanitarian&rsquo; and keywords', fillcolor = Beige]
        embed3 [label = 'Manual validation \n with AntConC', fillcolor = Beige]
}     
      
subgraph cluster_2 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Linen
        
        label = 'Clustering Countries'
        node[margin = 0.18]
        cluster1 [label = 'Estimate ideal \n number of clusters', fillcolor = Beige]
        cluster2 [label = 'Use cosine similarities \n to cluster countries into \n k = 3', fillcolor = Beige]
        cluster3 [label = 'Plot clusters on \n two-dimensional space \n using PCA', fillcolor = Beige]
}  

subgraph cluster_3 {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Linen
        
        label = 'Qualitative Analysis'
        node[margin = 0.2]
        quali1 [label = 'Analysis of collocations\nand keywords within clusters', fillcolor = Beige]
        quali2 [label = 'Collocate and keyness\nsearches across clusters', fillcolor = Beige]
        quali3 [label = 'N-gram and keyword-in-context\n (KWIC) analyses', fillcolor = Beige]
}  

# edge definitions with the node IDs

data1 -> embed1 [ltail=cluster_0 lhead=cluster_1 ]
embed1 -> cluster1 [ltail=cluster_1 lhead=cluster_2]
cluster1 -> quali1 [ltail=cluster_2 lhead=cluster_3]

}")

export_svg(graph) %>% 
  charToRaw %>% 
  rsvg_png(file = "plots/graph3.png", width = 1200)