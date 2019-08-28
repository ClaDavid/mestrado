import pyswarms as ps
######### running
from clustering_docs import ClusteringDocs

running_clust = ClusteringDocs()
options = {'c1': 1.49618, 'c2': 2, 'w':0.9}
optimizer = ps.single.GlobalBestPSO(n_particles=100, dimensions=1774, options=options)
best_cost, best_pos = optimizer.optimize(running_clust.runOtimizaPesoAtributo, iters=100)
cost_hist = optimizer.cost_history
cost_hist_string = list( map(str, cost_hist) )
np.savetxt("cost_hist.csv", cost_hist_string, delimiter=",", fmt=('%s'))
best_pos_string = best_pos.astype(str)
np.savetxt("best_pos_par.csv", best_pos_string, delimiter=",", fmt=('%s'))
