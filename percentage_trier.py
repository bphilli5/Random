"""
A script which attempts to answer the question, 'Is it better to do something 
with a 10% change of success ten times, or with a 100% chance of success once.'

--Author: swaggyb5
"""
import random
import statistics as stat

# By default, it will test 10%-100% at increments of 10% with 1000 trials each
percentages = [.1, .2, .3, .4, .5, .6, .7, .8, .9, 1]
trials = 1000

def percentage_trier(percentages, trials, graphing=False):
    """
    The main function of the script, if graphing is True, a seaborn boxplot
    will be created from a pandas dataframe.
    """

    # Creates a dictionary with the percentage and the results of each trial.
    results = {}
    print("Loading...")
    for percentage in percentages:
        print(f"Running for {percentage*100}%...")
        print()
        results[percentage] = []
        for trial in range(trials):
          tries = int(1000 / percentage)
          success_counter = 0
          for i in range(tries):
            if random.random() < percentage:
              success_counter += 1
          results[percentage].append(success_counter)

    # Prints the mean, standard deviation, maximum, and minimum for each percentage.
    for percentage in percentages:
        print(f"For {percentage*100}%...")
        print(f"We had an average of {stat.mean(results[percentage])} successes in {int(1000 / percentage)} tries")
        print(f"Over {trials} trials with a standard deviation of {stat.pstdev(results[percentage])}.")
        print(f"Maximum value: {max(results[percentage])}")
        print(f"Minimum value: {min(results[percentage])}")
        print()

    if graphing:
        import pandas as pd
        import seaborn as sns

        # Converts from dictionary to pandas df for graphing.
        data = []
        for percentage in percentages:
            for count in results[percentage]:
                data.append([percentage, count])
        df = pd.DataFrame(data, columns=["Percentage", "Successes"])

        # Plots data.
        sns.catplot(x="Percentage", y="Successes", kind="box", data=df)

if __name__ == "__main__":
    percentage_trier(percentages, trials, True)

"""
***SPOILER ALERT***
It depends on your risk tolerance
"""
