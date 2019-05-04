# Method derived from: https://github.com/sethneel/GerryFair
# Reference paper: https://arxiv.org/pdf/1808.08166.pdf

import gerryfair
dataset = "./dataset/student-mat3.csv"
attributes = "./dataset/student_protected.csv"
centered = True
X, X_prime, y = gerryfair.clean.clean_dataset(dataset, attributes, centered)

C = 15
printflag = True
gamma = .01
fair_model = gerryfair.model.Model(C=C, printflag=printflag, gamma=gamma)
max_iters = 10
fair_model.set_options(max_iters=max_iters)

# Train Set
X_train = X.iloc[:X.shape[0]-50]
X_prime_train = X_prime.iloc[:X_prime.shape[0]-50]
y_train = y.iloc[:y.shape[0]-50]
# Test Set
X_test = X.iloc[-50:].reset_index(drop=True)
X_prime_test = X_prime.iloc[-50:].reset_index(drop=True)
y_test = y.iloc[-50:].reset_index(drop=True)

# Train the model
[errors, fp_difference] = fair_model.train(X_train, X_prime_train, y_train)

predictions = fair_model.predict(X_test)

auditor = gerryfair.model.Auditor()
[group, gamma_unfairness] = auditor.audit(predictions, X_prime_test, y_test)

gerryfair.fairness_plots.plot_single(errors, fp_difference, max_iters, gamma, C)
