from argparse import ArgumentParser

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

from sklearn.model_selection import cross_val_predict, cross_validate, LeaveOneGroupOut

from model_zoo import load_model


def parse_args():
    """Parse main arguments."""
    parser = ArgumentParser(
        description="Arguments for benchmarking",
    )

    parser.add_argument(
        "-i",
        "--input",
        type=str,
        required=True,
        default="df_august.parquet",
        help="Input data",
    )

    parser.add_argument(
        "-m", "--model", type=str, required=True, default="xgboost", help="Model name"
    )

    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=16,
        help="The number of jobs to run in parallel",
    )

    parser.add_argument(
        "--verbose",
        type=int,
        default=2,
        help="Controls the verbosity when fitting and predicting.",
    )

    return parser.parse_args()


def prepare_data(input_file):
    df = pd.read_parquet(input_file).query("Interventions == 'No_Intervention'")

    print(f"Shape (raw data) : {df.shape}")

    # Prepare label
    y = df.mort30D.values

    print(f"Shape (y) : {y.shape}")

    # Prepare features
    cols = [
        "Age",
        "Sex",
        "Respiration_Rate",
        "Temperature",
        "Saturation",
        "Oxygen_Supplement",
        "Blood_Pressure.Sys",
        "Blood_Pressure.Dia",
        "Consciousness",
        "Previous_Hosp_Fac",
        "Hemoglobin",
        "Leukocytes",
        "Trombocytes",
        "Kreatinin",
        "ALAT",
        "LDH",
        "Albumin",
        "CRP",
        "Laktak_ab",
        "Troponin",
        "Laktat_vb",
    ]

    sub_df = df.loc[:, cols]

    # Binarization
    sub_df["Sex"] = sub_df.loc[:, "Sex"].eq("Female")

    sub_df["Oxygen_Supplement"] = sub_df.loc[:, "Oxygen_Supplement"].eq("Oxygen")

    sub_df["Consciousness"] = sub_df.loc[:, "Consciousness"].eq("A")

    # One-hot encoding
    sub_df = pd.concat(
        [
            sub_df,
            pd.get_dummies(sub_df["Previous_Hosp_Fac"], prefix="Previous_Hosp_Fac"),
        ],
        axis=1,
    ).drop("Previous_Hosp_Fac", axis=1)

    X = sub_df.values

    print(f"Shape (X) : {X.shape}")

    # Prepare groups
    groups = df.loc[:, "Hospital"]

    groups.value_counts()

    return X, y, groups


def plot_cv_results(cv_results):
    sns.pointplot(
        x="test_score",
        y="leftout",
        data=pd.DataFrame(cv_results),
        linestyles="none",
        color="k",
    )

    n_groups = len(cv_results["test_score"])

    mean_score = cv_results["test_score"].mean()
    plt.axvline(mean_score, linestyle="dashed", color="red")
    sns.despine()
    plt.ylabel("Group left out")
    plt.xlabel("ROC-AUC (unweighted)")
    plt.annotate(
        "Mean AUC (unweighted)",
        (mean_score, n_groups // 2),
        (mean_score + 0.01, n_groups // 2 + 1),
        arrowprops=dict(
            facecolor="black", shrink=0.01, width=0.5, headwidth=5, headlength=5
        ),
    )
    plt.show()


def main():
    args = parse_args()

    X, y, groups = prepare_data(args.input)

    logo = LeaveOneGroupOut()

    model = load_model(args.model, d_num=X.shape[1])

    cv_results = cross_validate(
        model, X, y, cv=logo, groups=groups, scoring="roc_auc", verbose=2, n_jobs=16
    )

    cv_results["leftout"] = sorted(groups.unique())

    plot_cv_results(cv_results)

    y_prob = cross_val_predict(
        model,
        X,
        y,
        cv=logo,
        groups=groups,
        verbose=args.verbose,
        n_jobs=args.jobs,
        method="predict_proba",
    )

    np.save(f"../private_results/{model.__class__.__name__}_loho_y_prob.npy", y_prob)


if __name__ == "__main__":
    main()
