from fuzzy_clustering import FCM
from fuzzy_clustering_modificado import FCMMOD
import pandas as pd
import numpy as np
import math
from sklearn.metrics import silhouette_score


class ClusteringDocs(object):

    def __init__(self):
        self.tav = pd.read_csv("reuters_pos_tag_final.csv")
        self.features_cluster = pd.read_csv("hard_cluster_features.csv")

    def runOtimizaPesoAtributo(self, weight=None):
        print("Starting.......")
        if weight is None:
            weight=np.repeat(1, self.tav.shape[0], axis=0)
        # print(weight)
        silhouette = list()
        #for index in range(1, 10):
        list_features_cluster = list()
        for column in self.features_cluster.columns[1:]: #iterate over all columns but the first one, in my case, just one the cluster so it's fine
            list_features_cluster.append(self.features_cluster[column])
        
        for clusters in range(2, math.floor(np.sqrt(self.tav.shape[0]))):
        #for clusters in range(2, 3):
            weight_array_features = np.empty_like(self.features_cluster[["feature"]])
            for cluster_identification, item in enumerate(list_features_cluster[0]):
                weight_array_features[cluster_identification] = weight[list_features_cluster[0][cluster_identification] - 1]
            #fcm = FCM(n_clusters=clusters)
            #fcm.fit(self.tav)
            fcm_mod = FCMMOD(n_clusters=clusters)
            fcm_mod.fit(X = self.tav, weight = weight_array_features)
            membership_fuzzy = pd.DataFrame(data=fcm_mod.u[0:,0:],
                    index=[i for i in range(fcm_mod.u.shape[0])],
                    columns=['' + str(i) for i in range(fcm_mod.u.shape[1])])
            #membership_fuzzy = pd.DataFrame(data=fcm.u[0:,0:],
            #        index=[i for i in range(fcm.u.shape[0])],
            #        columns=['' + str(i) for i in range(fcm.u.shape[1])])
            hard_clustering = membership_fuzzy.idxmax(axis=1)
            silhouette.append(silhouette_score(self.tav, hard_clustering))
        return(max(silhouette))


