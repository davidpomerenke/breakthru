import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")

try:
    long_data = pd.read_csv(
        "results.csv",
        names=["Gold",
            "Silver",
            "Utility",
            "Utility (StdDev)",
            "Ply-Depth",
            "Ply-Depth (StdDev)"
            ]
    )

    long_data["Utility+"] = long_data["Utility"].astype(str) + long_data["Utility (StdDev)"].astype(str)

    # UTILITY

    wide_data_utility = pd.pivot_table(
        long_data,
        values='Utility',
        index=['Gold'],
        columns='Silver'
    )

    wide_data_utility_reverse = wide_data_utility[::-1]

    sns.heatmap(
        wide_data_utility_reverse,
        vmin=-1,
        vmax=1,
        center=0,
        cmap="coolwarm",
        square=True,
        annot=wide_data_utility
    )

    plt.savefig("utility.png")
    plt.close()

    # PLY DEPTH

    wide_data_depth = pd.pivot_table(
        long_data,
        values='Ply-Depth',
        index=['Gold'],
        columns='Silver'
    )

    wide_data_depth_reverse = wide_data_depth[::-1]

    sns.heatmap(
        wide_data_depth_reverse,
        square=True,
        annot=True,
        fmt=".2f"
    )

    plt.savefig("depth.png")
    plt.close()

    print("The figures have been saved to `evaluation/{utility,depth}.png`.")

except:
    print("The data is missing. Go to the project root directory and run `stack run evaluate` to create it.")