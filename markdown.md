8.12.17 Neural Networks Fall lecture 02
# Summary of the introductions.
Machine learning (and deep learning and to some extent what is currently called AI) deals with making predictions based on a data set (sometimes also called a corpus):  <p style="text-align: center;">Data ---> Decision</p>
  ## We have seen 3 kinds of those problems:

  1. Supervised learning
    Data point ---> answer
    image ---> 0,1,...,n (recognizing digitin an image)
    speech ---> "thank you" (transcription)
    "Howdy" ---> "Siema" (translation)
    In supervised learning the goal is to discover the relation between inputs "X" and targets "Y". What do we care about?

    * generalizing past the dataset
    * doing well on new data
    * having low error rate (expected value of missclassification)
    
  2. Unsupervised learning
    Possible uses:
    * generating more data
    * describing the data
    * recognizing if a new data point is similar to the data set
    
  3. Reinforcement learning
    With an environment to play with (game, life simulation etc.) we can answer questions like:
    * what to do to get high rewards?
    * what is my expected reward?
    * what actions should i try?
    
 We need a language to speak about our goals and objectives. The problems often involve a lot of uncertainty and we'll adopt the language of probability. \[ x=a^b \]
 * For supervised learning: we have data $\{ x^{(i)},y^{(i)}|i=1,\dots,N}\sim P(X,Y)$
