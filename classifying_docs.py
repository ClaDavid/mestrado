from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
import pandas as pd
import numpy as np
import math


class ClusteringDocs(object):

    def __init__(self):
        self.tav = pd.read_csv("reuters_tav.csv")
        self.features_cluster = pd.read_csv("hard_cluster_features.csv")

    def runOtimizaPesoAtributo(self, weight=None):
        print("Starting.......")
        X = self.tav.drop(columns=['L1'])
        X = self.tav.drop(columns=['split'])
        y = self.tav['L1'].values

        # X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=0)

        if weight is None:
            weight=np.repeat(1, self.tav.shape[0], axis=0)
        # print(weight)
        list_features_cluster = list()
        # iterate over all columns but the first one, in my case, just one the cluster so it's fine
        for column in self.features_cluster.columns[1:]: 
            list_features_cluster.append(self.features_cluster[column])
        
        weight_array_features = np.empty_like(self.features_cluster[["feature"]])
        for cluster_identification, item in enumerate(list_features_cluster[0]):
            weight_array_features[cluster_identification] = weight[list_features_cluster[0][cluster_identification] - 1]
        
        # wminkowski with p = 2 is the same as euclidian distance
        knn = KNeighborsClassifier( n_neighbors=3, metric = 'wminkowski', p = 2, metric_params = {'w': weight_array_features} )
        kf = StratifiedKFold(n_splits = 3)
        ac = []
        cm = []

        for train_index, test_index in kf.split(X,y):
        	X_train, X_test = X[train_index], X[test_index]
        	y_train, y_test = y[train_index], y[test_index]
        	print(X_train, X_test)
            knn.fit(X_train, y_train)
            y_pred = knn.predict(X_test)
            ac.append(accuracy_score(y_test, y_pred))
            cm.append(confusion_matrix(y_test, y_pred))

        ### HAVE TO DEFINE THE ACC
        return(ac)


## knn ponderado ao inves de agrupamento, acuracia é o fitness, crossvalidation, definir os folds antes para calcular todos os individuos nos mesmos folds