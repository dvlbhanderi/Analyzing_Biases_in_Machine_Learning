"""
This script is the starter script that the user needs to run, which will set up
the dataset, call the corresponding models that the user chooses, and write the
results into the 'results' directory.
---------------------------
Author : Rutu Gandhi
"""


import argparse
import subprocess
from src.loan_status_prediction import Classifier
from src.utils.preproc_data import Postprocessing



parser = argparse.ArgumentParser(description='Team mangalyaan : Fairness in Machine Learning')
parser.add_argument('--model', dest='model', type=str, 
	default='logistic_regression', help='model to perform classification on a new dataset')
parser.add_argument('--trainpath', dest='trainpath',type=str, default='~/team-mangalyaan/datasets/loan_train.csv',
	help='local path of csv file')
parser.add_argument('--testpath', dest='testpath',type=str, default='~/team-mangalyaan/datasets/loan_test.csv',
	help='local path of csv file')
parser.add_argument('--dataset', dest='dataset', type=str, default='COMPAS', choices=['COMPAS','Loan_status', 'Student'],
	help='name dataset to try')
parser.add_argument('--y_train', dest='y_train', type=str, default='Loan_Status', help='Name of target column')
parser.add_argument('--postprocess', dest = 'postprocess', type=bool, default=False,
	choices=['Equalize AP by deferring on one','Equalize AP using min-PDF','Equalize PPV and NPV using 2 thresholds' ], help='soft classification to hard classification')
parser.add_argument('--test', dest='test', type=bool, default=False,
	choices=[True, False], help='to run the test suite')



args = parser.parse_args()
trainpath = args.trainpath
testpath = args.testpath
model = args.model
dataset = args.dataset
postprocess = args.postprocess



cl = Classifier(dataset, trainpath, testpath)

if test:
	print('Beginning Test Suite...')
	command = 'Rscript'
    	path_to_rscript = '/tests/run_tests.py'
else:
	if dataset == 'COMPAS':
		if model == 'logistic_regression':
			print("Classification not needed. Already done")
			print("Proceeding to postprocess")
			if postprocess == 'Equalize AP by deferring on one' :
				command = 'Rscript'
				path_to_rscript = '/src/compas_equalize.R'
			elif postprocess == 'Equalize AP using min-PDF':
				command = 'Rscript'
				path_to_rscript = '/src/compas_minPDF.R'
			elif postprocess == 'Equalize PPV and NPV using 2 thresholds':
				command = 'Rscript'
				path_to_rscript = '/src/compas_two_thresholds.R'
			cmd = [command, path_to_rscript]
			subprocess.checkout(cmd)
    	elif dataset == 'Loan_status':
		if model == 'logistic_regression':
			cl.log_reg()
			if postprocess == 'Equalize AP by deferring on one' :
				command = 'Rscript'
				path_to_rscript = '/src/loan_equalize.R'
			elif postprocess == 'Equalize AP using min-PDF':
				command = 'Rscript'
				path_to_rscript = '/src/loan_minPDF.R'
			elif postprocess == 'Equalize PPV and NPV using 2 thresholds':
				command = 'Rscript'
				path_to_rscript = '/src/loan_two_threshold.R'
			cmd = [command, path_to_rscript]
			subprocess.checkout(cmd)
	elif dataset == 'Student':
		if model == 'logistic_regression':
			print("Classification not needed. Already done")
			print("Proceeding to postprocess")
			if postprocess == 'Equalize AP by deferring on one' :
				command = 'Rscript'
				path_to_rscript = '/src/student_equalize.R'
			elif postprocess == 'Equalize AP using min-PDF':
				command = 'Rscript'
				path_to_rscript = '/src/student_minPDF.R'
			elif postprocess == 'Equalize PPV and NPV using 2 thresholds':
				command = 'Rscript'
				path_to_rscript = '/src/student_defer_threshold.R'
			cmd = [command, path_to_rscript]
			subprocess.checkout(cmd)
