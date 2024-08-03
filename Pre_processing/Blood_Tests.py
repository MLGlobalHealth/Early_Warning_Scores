import polars as pl
import os

# Specifying directory

path = "/Users/jkv465/Desktop/Work_EWS/New_Data"
# Specify the working directory
os.chdir(path)
print("variable working directory: ", os.getcwd()) # And here we can check it


# Scan the file

blpl = pl.scan_parquet("blood_tests_newest.parquet")

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
    ).drop(f"{col}_sure")
    # Drop the temporary column

# Check the columns and drop the ones that are un-imputed

blpl.collect_schema().names()

# Columns to keep

all_columns = blpl.collect_schema().names()

start_index = 5
end_index = 15

columns_to_keep = all_columns[:start_index] + all_columns[end_index + 1:]

columns_to_keep

blpl = blpl.select(columns_to_keep)


# Save the final file

blpl.sink_parquet("blood_testS_imputed.parquet")
