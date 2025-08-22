# encoding: utf-8
# author: yujunhou
# contact: yujunguin@gmail.com

import pandas as pd
import os


def combine(read_folder, save_path):
    ls_df = []
    for name in os.listdir(read_folder):
        if name != '.DS_Store':
            read_path = os.path.join(read_folder, name)
            if os.path.isfile(read_path):
                df = pd.read_csv(read_path)
                ls_df.append(df)
    combined_df = pd.concat(ls_df).drop(
        columns=['Unnamed: 0']).reset_index(drop=True)
    save_path = os.path.join(save_path)
    combined_df.to_csv(save_path)


if __name__ == '__main__':

    # path to the folder with csvs to be combined
    read_folder = r'raw_download/output/all/cities'
    # output path for the combined csv
    save_path = r'raw_download/output/all/combined_points.csv'
    combine(read_folder, save_path)
    print('Done')
