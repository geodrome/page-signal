L3S-GN1 Dataset (Released: January 2010)
Christian Kohlschuetter, L3S Research Center
http://www.L3S.de/~kohlschuetter/boilerplate/

LICENSE

L3S grants to You a non-exclusive, non-transferable, worldwide, and
royalty-free license to use the provided dataset for your research use only.

If you use the data in publications, please cite the corresponding paper
that describes this dataset:

Christian Kohlschuetter, Peter Fankhauser, Wolfgang Nejdl
Boilerplate Detection using Shallow Text Features
WSDM 2010: Third ACM International Conference on Web Search and Data Mining
New York City, NY USA. 

The paper contains more information about the dataset. You can find it
online at:
http://www.l3s.de/~kohlschuetter/publications/wsdm187-kohlschuetter.pdf

ABOUT

The news collection consists of 621 manually assessed news articles from 408
different web sites. The news articles were sampled randomly from a larger
crawl of 254,000 articles from 7,854 web sites which we acquired by
monitoring the Google News search engine during the first half of 2008. We
monitored the news headlines of six different English-speaking Google News
portals (USA, Canada, UK, South Africa, India, Australia) and four categories
(World, Technology, Sports, Entertainment) and fetched the full text HTML of
the corresponding linked articles.

Using a Web browser based text annotator, for each HTML page in the GoogleNews
set seven human assessors labeled1 sequences of text as either headline,
fulltext, supplemental (text which belongs to the article but is not fulltext,
such as image captions etc.), user comments, related content (links to other
articles etc.). Unselected text is regarded not content (boilerplate). The
labels were then stored at the level of individual text blocks (i.e., any
character sequence that is not interrupted by an HTML tag, except the A tag).

STATISTICS

We are confident that this dataset is "Zipf-representative" for news
articles on the Web.  The ranked distribution of articles per web site
apparently is power law distributed (maximum number of articles per host:
3774, averge: 32.38, median: 5). The top-5 hosts are ap.google.com,
afp.google.com, reuters.com, iht.com, news. bbc.co.uk; at the break-even
between rank and frequency (200) is en.rian.ru whereas sites like
financeasia.com and photonicsonline.com appear at the bottom. In the examined
subset, the maximum number of articles per host is 12 (news.com.au) whereas
the average and median are 1.52 and 1 respectively.

DATA FORMAT

For the dataset we use the WARC File Format (ISO 28500), see
http://bibnum.bnf.fr/WARC/

The pages are stored as individual WARC records, one for the original HTML
page we crawled (WARC-Type: resource) and one for the human-assessed document
(WARC-Type: conversion).

The human-assessed documents contain annotations in the form of <SPAN> tags
with specific CSS classes that indicate the type of content:
x-nc-sel0	Not content
x-nc-sel1	Headline
x-nc-sel2	Full text
x-nc-sel3	Supplemental
x-nc-sel4	Related content
x-nc-sel5	Comments

Additionally, HTML elements whose contents are clearly invisible to the user
have been marked with the CSS class "x-nc-invisible".


