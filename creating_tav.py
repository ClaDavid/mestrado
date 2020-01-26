import pandas as pd


if __name__ == '__main__':
	listaToMatrix = pd.read_csv("listaToMatrix.csv")
	# column_name = listaToMatrix.Var1.unique()
	# index_name = listaToMatrix.L1.unique()
	# tav = pd.DataFrame(data=0,index=index_name,columns= column_name)
	
	tav = listaToMatrix.pivot_table(columns='Var1', index='L1', values='value').reset_index()
	tav = tav.dropna(thresh=500, axis=1)
	tav = tav.fillna(0)	

	tav.to_csv("reuters_tav.csv", index=False)
