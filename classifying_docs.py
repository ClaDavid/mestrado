from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score
import pandas as pd
import numpy as np
import math
from sklearn.metrics import silhouette_score


class ClusteringDocs(object):

    def __init__(self):
        self.tav = pd.read_csv("reuters_tav.csv")
        self.features_cluster = pd.read_csv("hard_cluster_features.csv")

    def runOtimizaPesoAtributo(self, weight=None):
        print("Starting.......")
        if weight is None:
            weight=np.repeat(1, self.tav.shape[0], axis=0)
        # print(weight)
        list_features_cluster = list()
        # iterate over all columns but the first one, in my case, just one the cluster so it's fine
        for column in self.features_cluster.columns[1:]: 
            list_features_cluster.append(self.features_cluster[column])
        
        for clusters in range(2, math.floor(np.sqrt(self.tav.shape[0]))):
        #for clusters in range(2, 3):
            weight_array_features = np.empty_like(self.features_cluster[["feature"]])
            for cluster_identification, item in enumerate(list_features_cluster[0]):
                weight_array_features[cluster_identification] = weight[list_features_cluster[0][cluster_identification] - 1]
            # wminkowski with p = 2 is the same as euclidian distance
            knn = KNeighborsClassifier( metric = 'wminkowski', p = 2, metric_params = {'w': weight_array_features} )
        
        ### HAVE TO DEFINE THE ACC
        return(acc)


## knn ponderado ao inves de agrupamento, acuracia é o fitness, crossvalidation, definir os folds antes para calcular todos os individuos nos mesmos folds