from catboost import CatBoostClassifier
from catboost.utils import get_gpu_device_count
from lightgbm import LGBMClassifier
from xgboost import XGBClassifier


def load_model(name, random_state=42, **kwargs):
    if name in ["xgboost", "XGBClassifier"]:
        model = XGBClassifier(
            objective="binary:logistic", random_state=random_state, **kwargs
        )
    elif name in ["catboost", "CatBoostClassifier"]:
        task_type = "GPU" if get_gpu_device_count() > 0 else None
        model = CatBoostClassifier(
            silent=True,
            task_type=task_type,
            devices="0",
            random_seed=random_state,
            **kwargs,
        )
    elif name in ["lgbm", "LGBMClassifier"]:
        model = LGBMClassifier(random_state=random_state, verbosity=0, **kwargs)
    else:
        raise ValueError(f"No model found for `{name}`.")

    return model
