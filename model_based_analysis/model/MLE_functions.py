import pandas as pd
import numpy as np
from joblib import Parallel, delayed
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.optimize import minimize
from scipy.stats import bernoulli
import os

def predict_model(error, params, is_good):
    # This function should be implemented based on the actual model
    pass

def fit_all_participants(df, model_obj, input_col, output_rule):
    def fit_participant(participant_df):
        errors = participant_df[input_col].abs().to_numpy()
        is_good = participant_df['DisplayScore'].to_numpy()
        behavior = (participant_df['EstRule'] == output_rule).to_numpy()
        result = model_obj.mle(input=errors, behavior=behavior, num_iter=10, ext_params=is_good)
        return [participant_df['PlayerID'].iloc[0], model_obj.num_params] + result + [calculate_aic(result[-1], model_obj.num_params)]

    unique_players = df['PlayerID'].unique()
    results = Parallel(n_jobs=-1)(delayed(fit_participant)(df[df['PlayerID'] == player]) for player in unique_players)
    results_df = pd.DataFrame(results, columns=['PlayerID', 'number_of_params'] + model_obj.param_names + ['log_likelihood', 'AIC'])
    return results_df

def calculate_aic(log_likelihood, num_params):
    return -2 * log_likelihood + 2 * num_params

def export(data, filename):
    data.to_pickle(filename)  # Save DataFrame in Python's pickle format

def main():
    # Load data, create model object, etc.
    pass

if __name__ == "__main__":
    main()
