import numpy as np
from scipy.optimize import minimize
from scipy.special import expit as inv_logit  # Logistic function

def clip_vectors(value, large=1e300):
    value = np.where(value == np.inf, large, value)
    value = np.where(value == -np.inf, -large, value)
    return value

def clip_probs(value, delta=1e-300):
    value = np.where(value == 0, delta, value)
    value = np.where(value == 1, 1 - delta, value)
    return value

def log_lik_bernoulli(params, input, behavior, pred):
    probs = pred(input, params)
    probs = clip_probs(probs)
    log_probs = np.log(probs)
    log_one_minus_probs = np.log(1 - probs)
    each_log_l = behavior * clip_vectors(log_probs) + (1 - behavior) * clip_vectors(log_one_minus_probs)
    result = np.sum(each_log_l)
    return -10000 if np.isnan(result) else result

def maximize_likelihood(input, behavior, pred, num_params, max_values, min_values, func_likelihood=log_lik_bernoulli, num_iter=10):
    bounds = [(low, high) for low, high in zip(min_values, max_values)]

    def objective(params):
        return -func_likelihood(params, input, behavior, pred)

    best_solution = None
    best_obj = float('inf')

    for _ in range(num_iter):
        initial_guess = np.random.uniform(min_values, max_values)
        result = minimize(objective, initial_guess, bounds=bounds, method='L-BFGS-B')
        if result.fun < best_obj:
            best_solution = result
            best_obj = result.fun

    return best_solution

def calc_aic(log_lik, num_params):
    return -2 * log_lik + 2 * num_params

class AccumulationModelClass:
    def __init__(self, num_of_params, upper_range, lower_range, predict_choice, param_name_list):
        self.num_of_params = num_of_params
        self.upper_range = upper_range
        self.lower_range = lower_range
        self.predict_choice = predict_choice
        self.param_name_list = param_name_list

    def calc_loglik(self, input, behavior, params):
        return log_lik_bernoulli(params, input, behavior, self.predict_choice)

    def mle(self, input, behavior, num_iter=10, ext_params=None):
        if ext_params is not None:
            def predict_choice(input, params):
                return self.predict_choice(input, params, ext_params)
        else:
            predict_choice = self.predict_choice

        return maximize_likelihood(input, behavior, predict_choice, self.num_of_params, self.upper_range, self.lower_range, num_iter=num_iter)

# Example of a predictive model function
def alpha_gb_model(error, params, is_good):
    alpha_G = params[0]
    alpha_B = params[1]

    z = np.empty_like(error)
    z[0] = alpha_B * error[0] if is_good[0] == 0 else alpha_G * error[0]

    for i in range(1, len(error)):
        z[i] = (alpha_B if is_good[i] == 0 else alpha_G) * error[i] + z[i - 1]

    probs = inv_logit(z)
    return probs
