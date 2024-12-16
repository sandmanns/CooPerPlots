# CooPer plots - the next level of UpSet plots

<p align="center">
    <img height="850" src="https://uni-muenster.sciebo.de/s/5fKvnfGdbKPg94G/download">
</p>

Varghese J, Sandmann S, Ochs K, Schrempf IM, Frömmel C, Dugas M, Schmidt HH, Vollenberg R, Tepasse PR. Persistent symptoms and lab abnormalities in patients who recovered from COVID-19. Sci Rep. 2021 Jun 17;11(1):12775. doi: 10.1038/s41598-021-91270-8. PMID: 34140539; PMCID: PMC8211641.

Sandmann S, Dugas M and Varghese J. CooPer plots – the next level of UpSet plots. F1000Research 2021, 10(ISCB Comm J):714 (poster) https://doi.org/10.7490/f1000research.1118684.1


To visualize intersects between datasets, two main types of plots are available: Venn diagrams and UpSet plots. Although Venn diagrams provide a common solution to visualize the relation between 2-3 datasets, application in the context of >5 sets is usually not recommended. By contrast, UpSet plots are able to clearly visualize intersects between $\geq$5 datasets. However, the more sets and intersects are present, the more difficult it gets to visualize all information in a single plot. Furthermore, information on a second time point cannot be added. To overcome this current limitation, we developed CooPer plots (CO-Occurrence and PERsistence plots).

CooPer plots are based on the idea of interaction networks: We visualize every set as a node and every intersect between two sets as an edge. To illustrate the size of a set, it is correlated with the size of the node. Additionally, the exact numbers are printed within or next to the nodes. To illustrate the size of a specific intersect, the edges' thickness corresponds to the size of the intersects. If two sets do not share any intersect, they are not connected. Thereby, the complexity of the network is reduced.

In addition to the basic network, visualizing co-occurrence/intersects between sets, color coding allows us to add information on a second time point to the plot. If a user provides such information, the numbers characterizing the sets at the second time point are added to the nodes in parentheses. If a user opts for color coding, the nodes are colored from yellow (low number of persisting features) to dark red (high number of persisting features). The user may choose between coloring according to the relative or the absolute numbers. In addition, the edges are also color coded. Intersects at the first time point are visualized by gray edges, intersects at the second time point by black edges. 

CooPer plots are generated with the help of the R package `igraph' (Csardi G, Nepusz T, 2006). We developed a shiny app to generate basic CooPer plots. In addition to the actual CooPer plot, a table with information on the size of each category at the first and second time point and the percentage of persisting elements is reported. To illustrate the use of CooPer plots, a demo dataset is included.


## Requirements
To run appreci8R, you need R (Version 4.1.0 or higher).

## Running CooPer plots
CooPer plots is available as a shiny GUI. You can run CooPer plots either by RStudio or by using `shiny::runApp()` in an R console.

By default, the demo data is loaded and the demo tab is active. An initial network is displayed showing primary vs progredient Covid-19 symptoms (Varghese et al. 2021). You can choose between
* Including information on progredient symptomes for color-coding (default: yes; if no, all nodes are colored in orange).
* Base coloring on relative or absolute frequency (default: absolute).
* Re-shuffle visualization when starting the analysis again with different configuration (using igraph, it might happen that nodes appear too close to each other)

Generate your own network by just uploading a txt file: one line represents one case, initial symptomes have to be provided as comma-separated in the first column. Optionally, a second column providing information on persisting symptoms can be included. Of note: CooPer plots does not only allow to analyze symptoms, but also e.g. mutated genes per patient (check out our ISMB poster 2021: https://f1000research.com/posters/10-714).


## Contact
In case of errors or feature requests, do not hesitate to open an issue or contact Sarah Sandmann (sarah.sandmann@uni-muenster.de).
