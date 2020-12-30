import pandas as pd
import math
from sklearn.metrics import silhouette_score
from sklearn.cluster import KMeans
import numpy as np
import matplotlib.pyplot as plt

from kneed import KneeLocator

def elbow_method(df):
    distortions = []
    K = range(2, math.floor(math.sqrt(len(df))))
    for number_cluster in K:
        kmeanModel = KMeans(n_clusters=number_cluster)
        kmeanModel.fit(df)
        distortions.append(kmeanModel.inertia_)

    kn = KneeLocator(K, distortions, curve='convex', direction='decreasing')
    print(kn.knee)
    plt.figure(figsize=(16,8))
    plt.plot(K, distortions, 'bx-')
    plt.xlabel('Número de Grupos')
    plt.ylabel('Valores de Inércia')
    # plt.title('The Elbow Method showing the optimal k')
    plt.vlines(kn.knee, plt.ylim()[0], plt.ylim()[1], linestyles='dashed')
    plt.style.use('ggplot')
    plt.show()


def silhouette_method(df):
    score_list = []
    for number_cluster in range(2, math.floor(math.sqrt(len(df)))):
        clusterer = KMeans(n_clusters=number_cluster)
        preds = clusterer.fit_predict(df)
        centers = clusterer.cluster_centers_
        score = silhouette_score(df, preds)
        score_list.append(score)
        print("For number_cluster = {}, silhouette score is {})".format(number_cluster, score))
    # np.savetxt("silhouette_score_reuters_trans.csv", score, delimiter=",", fmt=('%s'))


def kmeans(df, number_cluster):
    clusterer = KMeans(n_clusters=number_cluster).fit(df)
    cluster_map = pd.DataFrame()
    cluster_map['feature'] = df.index.values
    cluster_map['cluster'] = clusterer.labels_
    cluster_map.to_csv("hard_cluster_features_lsvt.csv", index = False)
    

if __name__ == '__main__':
    tav = pd.read_csv("lsvt.csv")
    tav = tav.drop(['L1'], axis=1).T
    
    # elbow_method(tav)
    # silhouette_method(tav)
    kmeans(tav, 2)
    