# page-signal

Originally intended as a Clojure library designed to extract article text from web pages presumed to be articles/blog posts etc. There are many approaches to this problem. This solution is based on this paper:
http://www.l3s.de/~kohlschuetter/boilerplate/

The authors' marked up the web page into atomic blocks, annotated the blocks with features, and classified each block as either boilerplate or content based on its features. Machine learning techniques were used to determine the most predictive features. The algorithm was learned on a dataset from 2008, so it may now be outdated. I found it still works surprisingly well, though not perfectly.

One of the authors wrote an open source Java library called Boilerpipe based on the paper. My original intention was to reimplement the algorithm in Clojure and then use the power of Clojure to experiment with additional ad hoc rules to enahance the basic algorithm. I no longer have a need for this functionality, but I am leaving this here in case anybody wants to pickup where I left off. 

This is alpha, but the foundation is there. The library also attempts to pickout the headline, but this functionality doesn't work as well.
