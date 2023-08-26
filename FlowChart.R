library(DiagrammeR)

graph <- grViz("
digraph flowchart {

    # 设置所有节点的默认形状为方形
    node [shape=box];

    A [label='UCDP/PRIO Armed Conflict Dataset v23.1']
    B [label='V-Dem Full+Others v13']
    C [label='SIPRI Military Expenditure Database']
    D [label='Polity 5']
    E [label='GeoDataSource']
    F [label='Integrate and Add Alpha-3 code']
    G [label='Transform to dyadic Form']
    H [label='Calculate ratios']
    I [label='Final Dataset']

    A -> F
    B -> F
    C -> F
    D -> F
    E -> F
    F -> G
    G -> H
    H -> I
}
")

print(graph)
DiagrammeR::export_graph(graph, output_file = "flowchart.png", width = 1000, height = 800, 
                         file_type = "png", setwd("/Users/zoey1999/Desktop"))
html_output <- htmlwidgets::saveWidget(as_widget(graph), "temp_graph.html", selfcontained = FALSE)
