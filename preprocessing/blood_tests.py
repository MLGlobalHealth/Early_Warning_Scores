"""Pre-processing blood test data."""

import os

from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser

import polars as pl


def parse_args():
    """Parse main arguments."""
    parser = ArgumentParser(
        description="Argumetns for pre-processing blood test data",
        formatter_class=ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        "-i",
        "--input",
        type=str,
        required=True,
        default="blood_tests_newest.parquet",
        help="Input data",
    )

    return parser.parse_args()


def impute(input_data):
    """Imputation of missing values in blood test data

    Parameters
    ----------
    input_data : str
        Path to blood test data
    """
    # Scan the file
    blpl = pl.scan_parquet(input_data)

    # Collect the relevant columns
    columns = blpl.collect_schema().names()[-11:]

    # Apply the functions through the relevant columns
    for col in columns:
        # Create a temporary column with only the "sure"/"true" numbers in that column
        blpl = blpl.with_columns(
            pl.col(col).cast(pl.Float64, strict=False).alias(f"{col}_sure")
        )

        # Collect all values prepended by ">" or "<" in a standard list
        old_vals = (
            blpl.select(col)
            .filter(pl.col(col).str.starts_with(">") | pl.col(col).str.starts_with("<"))
            .unique()
            .collect()
            .to_numpy()
            .squeeze()
            .tolist()
        )

        # Prepare a new list for old_vals
        new_vals = []

        for elm in old_vals:
            # Operator ">" or "<"
            operator = elm[0]
            # The bounding number
            bound = float(elm[1:])

            if operator == "<":
                # Collect lower median
                new_val = (
                    blpl.select(f"{col}_sure")
                    .filter(pl.col(f"{col}_sure") < bound)
                    .median()
                    .collect()
                    .item()
                )
            else:
                # Collect upper median
                new_val = (
                    blpl.select(f"{col}_sure")
                    .filter(pl.col(f"{col}_sure") > bound)
                    .median()
                    .collect()
                    .item()
                )
            new_vals.append(new_val)
            # Convert to series

        old_vals = pl.Series(old_vals)
        new_vals = pl.Series(new_vals)

        blpl = blpl.with_columns(
            pl.col(col)
            # Replace old values by new values
            .replace(old_vals, new_vals)
            # Force cast to float again now that we fixed all the problematic values
            .cast(pl.Float64, strict=False)
            # Create a new column
            .alias(f"{col}_imputed")
            # Drop the temporary column
        ).drop(f"{col}_sure")

    # Check the columns and drop the ones that are un-imputed
    all_columns = blpl.collect_schema().names()

    start_index = 5
    end_index = 15

    columns_to_keep = all_columns[:start_index] + all_columns[end_index + 1 :]

    blpl = blpl.select(columns_to_keep)

    # Save the final file
    blpl.sink_parquet(f"{os.path.splitext(input_data)[0]}_imputed.parquet")


if __name__ == "__main__":
    args = parse_args()

    impute(args.input)
