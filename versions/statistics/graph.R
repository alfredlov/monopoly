library(DiagrammeR)

grViz("
digraph boxes_and_circles {
      
      # a 'graph' statement
      graph [overlap = true, 
              fontsize = 45, 
              fontname = 'Comic Sans', 
              compound = true, 
              nodesep = .25, 
              anksep = .5,
              color = red,
              layout = dot]
      node [
              fontsize = 25, shape = circle,
            fixedsize = true,
      style = filled, 
      #fillcolor = deepskyblue,
      color = none, 
      width = 0.9]
      edge [color = gray70]

      node [fillcolor = palegreen]
      1; 3; 
      node [fillcolor = gold]
      2; 4; 5; 9; 10; 107;
      node [fillcolor = coral]
      7; 6; 11; 8;
      
      # several 'edge' statements
      1 -> {2 4 6 7 8 10 11 107}
      2 -> {6 7 8 11}
      3 -> {2 4 5 6 7 8 9 11}
      4 -> {2 11 5 6 7 8}
      5 -> {6 7 8 9 11}
      6 -> {7}
      7 -> {}
      8 -> {6}
      9 -> {6 7 8 11}
      10 -> {2 6 7 8 9 11}
      11 -> {6 7 8}
      107 -> {6 7 8}
      107 [label = '@@1']

}
[1]: 'AI'
      ")

