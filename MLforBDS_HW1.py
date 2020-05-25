# -*- coding: utf-8 -*-
"""
@author: williamlee
"""

import os
path = "/Users/williamlee/Desktop/machine_learning_code"
os.chdir(path)

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

diabetes_df = pd.read_csv("diabetes.csv")

#########################################

diabetes_df_3c = diabetes_df

diabetes_df_3c = diabetes_df_3c.rename(columns={"DiabetesPedigreeFunction":"Diabetes\nPedigree\nFunction"})

diabetes_df_3c = diabetes_df_3c.sort_values(by="Outcome", ascending=False)
diabetes_df_3c = diabetes_df_3c.set_index('Outcome')

diabetes_df_3c = (diabetes_df_3c - diabetes_df_3c.mean())/diabetes_df_3c.std()

plt.subplots(figsize=(10,10))
hmap_3c = sns.heatmap(diabetes_df_3c, cmap="coolwarm", vmin=-0.6, vmax=1)
plt.title("Heatmap of All Attributes, Categorized by Class Label")

hmap_3c.figure.savefig("HW1_3c_heatmap.pdf")

#########################################

diabetes_df_3d = diabetes_df

diabetes_df_3d = diabetes_df_3d.sort_values(by="Outcome", ascending=False)
diabetes_df_3d = diabetes_df_3d.set_index('Outcome')

hmap_index = diabetes_df_3d.index

diabetes_df_3d = (diabetes_df_3d - diabetes_df_3d.mean())/diabetes_df_3d.std()

diabetes_corr = np.corrcoef(diabetes_df_3d)
diabetes_corr = pd.DataFrame(diabetes_corr, index=hmap_index, columns=hmap_index)

plt.subplots(figsize=(10,10))
hmap_3d = sns.heatmap(diabetes_corr, cmap="coolwarm", vmin=-0.6, vmax=1)
plt.title("Heatmap of Pearson’s Correlation between All Pairs\nof Data Points, Categorized by Their Class Label")

hmap_3d.figure.savefig("HW1_3d_heatmap.pdf")