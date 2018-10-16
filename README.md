## Application of Recurrent Neural Networks in Toxic Comment Classification

### Abstract

Moderators of online discussion forums often struggle with controlling extremist comments on their platforms. To help provide an efficient and accurate tool to detect online toxicity, we apply word2vec’s Skip-Gram embedding vectors, Recurrent Neural Network models like Bidirectional Long Short-term Memory to tackle a toxic comment classification problem with a labeled dataset from Wikipedia Talk Page. We explore different pre-trained embedding vectors from larger corpora. We also assess the class imbalance issues associated with the dataset by employing sampling techniques and penalizing loss. Models we applied yield high overall accuracy with relatively low cost.

### Data Source
[Toxic Comment Classification Challenge](https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge) from Kaggle.

### Dependencies

- Python
  - NumPy
  - Pandas  
  - Keras
  - tensorflow-gpu
  - CUDA
  - cuDNN
  - gensim
  - NLTK
  - scikit-learn

- R

  - readr
  - tidyr
  - dplyr
  - stringr
  - stringi
  - ggplot2

