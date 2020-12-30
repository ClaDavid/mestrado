# Masters dissertion and codes - Detection of implicit textual structures by hard grouping

Hello dear viewer!

In this repository, I have all the codes that I used in my Masters dissertion, mostly done by me. 

## But what the heck is this about?

Well, you see, when we are working with data mining, mainly texting mining, we have so many variables and features, and it can damage your own model for classification or clustering. Sure, we have stopwords that can be removed, but not always this can be good for the model, it can have variety depending on the subject and, mainly, you can loose information. Like, if my context is about Vitamins, and then I have the words "Vitamin a", I do want to keep it, as it is important to me. Well, in here we may have a approach that can guide which words are relevant to the text and which are not, based in their syntax job.

In this project I use Natural Processing Language (like Pos-Tagging), K-means in the features (yes, I reduce the dimension of the dataset based on the similarity of the features, crazy huh?), then I give weights to these cluster of features and I optimize them with classification techniques and the help of PSO, with the fitness being the value of the accuracy of the classification. The dimension of the particles of the PSO is equal the number of the clusters of features.

I won't lie, it's a little confused, and the codes are not really organized, but I assure I will when I have the time.
