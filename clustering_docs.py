from fuzzy_clustering import FCM
from fuzzy_clustering_modificado import FCMMOD
import pandas as pd
import numpy as np
import math
from sklearn.metrics import silhouette_score


class ClusteringDocs(object):

    def __init__(self):
        self.tav = pd.read_csv("reuters_pos_tag_final.csv")

    def runOtimizaPesoAtributo(self, weight=None):
        if weight is None:
            weight=np.repeat(1, self.tav.shape[0], axis=0)
        silhouette = list()
        for index in range(1, 10):
            #for clusters in range(2, math.floor(np.sqrt(self.tav.shape[0]))):
            for clusters in range(2, 3):
                fcm = FCM(n_clusters=clusters)
                fcm.fit(self.tav)
                fcm_mod = FCMMOD(n_clusters=clusters)
                fcm_mod.fit(X = self.tav, weight = weight)
                membership_fuzzy = pd.DataFrame(data=fcm_mod.u[0:,0:],
                        index=[i for i in range(fcm_mod.u.shape[0])],
                        columns=['' + str(i) for i in range(fcm_mod.u.shape[1])])
                hard_clustering = membership_fuzzy.idxmax(axis=1)
                silhouette.append(silhouette_score(self.tav, hard_clustering))
        return(max(silhouette))


