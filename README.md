# page-signal

Originally intended as a Clojure library designed to extract content text from web pages presumed to be articles/blog posts etc. There are many approaches to this problem. This solution is based on the paper "Boilerplate Detection using Shallow Text Features" available here: http://www.l3s.de/~kohlschuetter/boilerplate/

The authors' marked up the web page into atomic 'blocks', annotated the blocks with features, and classified each block as either boilerplate or content based on its features. Machine learning techniques were used to determine the most predictive features and classify each block as either boilerplate or content. The algorithm was learned on a dataset from 2008, so it may now be outdated.

One of the authors wrote an open source Java library called Boilerpipe based on the paper. My original intention was to reimplement the algorithm in Clojure and experiment with additional ad hoc rules to enhance the basic algorithm. I no longer have a need for this functionality, but I am leaving this here in case anybody wants to pickup where I left off.

The algorithm was evaluated against the google news data set (see paper) that is in the 'L3S-GN1-20100130203947-00001' directory. Precision, recall, and F1-score were calculated. Precision measures how much of the retrieved text is relevant. Precision improves as less boilerplate is mistakenly extracted. Recall measures how much of the relevant text has been extracted. Recall improves as less relevant text is missed. F1-score is the harmonic mean of precision and recall. 

The array of words model is used for this calculation. Longest common substring algorithm (not to be confused with Longest common subsequence) is applied to words of a text rather than the typical characters of a string. This algorithm was written in pure Java for better performance.

More here: http://tomazkovacic.com/blog/74/evaluation-metrics-for-text-extraction-algorithms/

My implementation got an F1-score of 81.7. This is short of 93.9 achieved by the authors in the paper. Clearly there is room for improvement. In addition some files in the data set aren't handled well. There is also code that attempts to pick out the headline, but it is still experimental and doesn't work consistently.




