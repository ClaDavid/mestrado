from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split
import pandas as pd
import numpy as np
import math


class ClusteringDocs(object):

    def __init__(self):
        self.tav = pd.read_csv("bbc_tav_full_final.csv")
        self.features_cluster = pd.read_csv("hard_cluster_features.csv")

    def runOtimizaPesoAtributo(self, weight=None):
        print("Starting.......")
        X = self.tav.drop(columns=['L1']).values
        # X = self.tav.drop(columns=['split'])
        y = self.tav['L1'].values

        if weight is None:
            weight=np.repeat(1, self.tav.shape[0], axis=0)
        # print(weight)
        list_features_cluster = list()
        # iterate over all columns but the first one, in my case, just one is the cluster so it's fine
        for column in self.features_cluster.columns[1:]: 
            list_features_cluster.append(self.features_cluster[column])
        
        weight_array_features = np.empty_like(self.features_cluster[["feature"]])
        for cluster_identification, item in enumerate(list_features_cluster[0]):
            weight_array_features[cluster_identification] = weight[list_features_cluster[0][cluster_identification] - 1]

        # print(list(map(list, weight_array_features)))
        weight_array_features_convert = (''.join([str(i[0]) for i in weight_array_features]))
        weight_array_features_convert_array = np.array(list(map(int, weight_array_features_convert)))
        # wminkowski with p = 2 is the same as euclidian distance
        knn = KNeighborsClassifier( n_neighbors=3, metric = 'wminkowski', p = 2, metric_params = {'w': np.asarray(weight_array_features_convert_array)} )
        # knn = KNeighborsClassifier( n_neighbors=3 )
        kFold = StratifiedKFold(n_splits = 3)
        accuracyScore = []
        confusionMatrix = []

        for train_index, test_index in kFold.split(X,y):
            # print("TRAIN:", train_index, "TEST:", test_index)
            # X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=0)
            X_train, X_test = X[train_index], X[test_index]
            y_train, y_test = y[train_index], y[test_index]
            knn.fit(X_train, y_train)
            y_pred = knn.predict(X_test)
            accuracyScore.append(accuracy_score(y_test, y_pred))
            confusionMatrix.append(confusion_matrix(y_test, y_pred))

        print(accuracyScore)

        ### HAVE TO DEFINE THE ACC
        return(accuracyScore)

running_clust = ClusteringDocs()
running_clust.runOtimizaPesoAtributo()


## knn ponderado ao inves de agrupamento, acuracia é o fitness, crossvalidation, definir os folds antes para calcular todos os individuos nos mesmos folds


##### HISTOGRAMA COM CORTES
##### CLASSIFICACAO 66% 33%