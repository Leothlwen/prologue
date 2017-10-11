8.12.17 Neural Networks Fall lecture 02
Summary of the introductions.
* machine learning (and deep learning and to some extent what is currently called AI) deals with making predictions based on a data set (sometimes also called a corpus):
<center>Data ---> Decision</center>
W have seen 3 kinds of those problems:
1. Supervised learning
<center> Data point ---> answer
image ---> 0,1,...,n (recognizing digitin an image)
speech ---> "thank you" (transcription)
"Howdy" ---> "Siema" (translation)</center>
In supervised learning the goal is to discover the relation between inputs "X" and targets "Y". What do we care about?
* generalizing past the dataset
* doing well on new data
* having low error rate (expected value of missclassification)
2. Unsupervised learning
$\text{Data set} \left\{ \begin{tabular}{l} \end{tabular} \right. $
