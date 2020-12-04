# RNApp

---

RNApp is an application to analyse aligned RNA-seq data.</br>
</br>
- First proceed to a quality control, an alignment and a count (FeatureCounts for example).</br>
- Upload a count csv or tsv table where the first column is composed of genes' names.



<b>PART Ia : Upload data and visualization</b>
---

- Upload your data as described above </br>
- Visualize the data across :</br>
    - result table</br>
    - Count distribution (bar plot)</br>
    - Hierarchical clustering (heatmap)</br>
    - PCA ( 2D and 3D plots)</br>


<b>PART Ib : Upload data and visualization</b>
---

- Perform soft clustering on time series data using Mfuzz. </br> 
With help to choose the number of clusters.
- clusters table automatically downloaded.


<b>PART II : Analysis</b>
---
3 analysis methods are proposed, TCC, DESeq2 or edgeR </br>

<b>- Normalization</b> </br>

The normalization is made by the TCC package.The package allows to compare tag count data with robust normalization strategies.</br>
<i>https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219</i></br> 
Or by DESeq2 or by TMM, RLE, upperquartile (with a edgeR analysis) </br>
</br>
- Choose your method and parameters</br>
- Visualize the results</br>
- Download contents</br>

<b>- Analysis</b> </br>

For each result, downloading png images and results is available.</br>

- MA plot</br>
- Volcano plot</br>
- Heatmap and clustering </br>
- PCA ( 2D and 3D plots)</br>

<b> PART III : Enrichment & Conversion </b>
---

<b> - Enrichment : </b></br>

Giving a set of ids, this section provides you a GO Term enrichment, Kegg and Wiki Pathways with an associated graph. </br>

<b> - Orthology : </b></br>

Giving a set of  ids, input and target organism, this section provides an orthologs search using gprofiler2 package. </br>

<b> - Conversion : </b></br>

Giving a set of  ids of choice, this section provides a translation to other ids using gprofiler2 package. 

