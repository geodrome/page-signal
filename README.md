# page-signal

Originally intended as a Clojure library designed to extract article text from web pages presumed to be articles/blog posts etc. There are many approaches to this problem. This solution is based on the paper "Boilerplate Detection using Shallow Text Features" available here: http://www.l3s.de/~kohlschuetter/boilerplate/

The authors' marked up the web page into atomic blocks, annotated the blocks with features, and classified each block as either boilerplate or content based on its features. analyze a small set of shallow text features for classifying the individual text elements in a Web page Machine learning techniques weere used to determine the most predictive features. The algorithm was learned on a dataset from 2008, so it may now be outdated. I found it still works surprisingly well, though not perfectly.

One of the authors wrote an open source Java library called Boilerpipe based on the paper. My original intention was to reimplement the algorithm in Clojure and then use the power of Clojure to experiment with additional ad hoc rules to enhance the basic algorithm. I no longer have a need for this functionality, but I am leaving this here in case anybody wants to pickup where I left off.

The algorithm was evaluated for precision and recall. More here: http://tomazkovacic.com/blog/74/evaluation-metrics-for-text-extraction-algorithms/

This is alpha, but the foundation is there. There is also code that attempts to pick out the headline, but it is still experimental and doesn't work consistently.
