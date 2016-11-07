In the folder "generate_the_data" we find the code dowloaded from the repository ... 

In order to generate the random data with drifst, inside the python script randomLHSGeneratorDrift.py we hae fixed center_num = 10 and changed the parameter distribution in the function:

stream = RandomLHSGeneratorDrift(file_name=fname, sample_limit=limit, center_num=10, dimensions=f, distribution= "triangular")

using: expovariate, triangular and gauss

Once you run randomLHSGeneratorDrift.py, a dataset is generated with the selected characteristics.