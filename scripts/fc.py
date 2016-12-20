import pandas as pd
df = pd.read_csv("~/downloads/to_cluster.csv")
import fastcluster as fc
out = fc.ward(df)