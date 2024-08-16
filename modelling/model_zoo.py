import warnings

import torch

from catboost import CatBoostClassifier
from catboost.utils import get_gpu_device_count
from lightgbm import LGBMClassifier
from xgboost import XGBClassifier


from pytorch_tabnet.tab_model import TabNetClassifier
from ResNet.resnet_ft import ResNet
from ResNet.resnext import ResNext


def load_model(name, d_num=None, category_count=None, random_state=42):
    if name in ["xgboost", "XGBClassifier"]:
        model = XGBClassifier(
            objective="binary:logistic",
            random_state=random_state,
        )
    elif name in ["catboost", "CatBoostClassifier"]:
        task_type = "GPU" if get_gpu_device_count() > 0 else None
        model = CatBoostClassifier(
            silent=True, task_type=task_type, devices="0", random_seed=random_state
        )
    elif name in ["lgbm", "LGBMClassifier"]:
        model = LGBMClassifier(random_state=random_state)
    elif name in ["resnet", "resnet_ft", "ResNet"]:
        model = ResNet(
            d_numerical=d_num,
            categories=category_count,  # get_categories(X_cat, outer_train_idx, outer_test_idx),
            d_out=1,
            d=128,
            n_layers=2,
            hidden_dropout=0.2,
            residual_dropout=0.2,
            d_embedding=64,
            d_hidden_factor=1.0,
            activation="relu",
            normalization="batchnorm",
        )
        warnings.warn(
            "ResNet is not supported due to NaN values in the input data.",
            DeprecationWarning,
        )
    elif name in ["resnext", "ResNext"]:
        model = ResNext(
            d_numerical=d_num,
            categories=category_count,  # get_categories(X_cat, outer_train_idx, outer_test_idx),
            d_out=1,
            d=128,
            n_layers=2,
            hidden_dropout=0.2,
            residual_dropout=0.2,
            d_embedding=64,
            d_hidden_factor=1.0,
            activation="relu",
            normalization="batchnorm",
            cardinality=4,
        )
        warnings.warn(
            "ResNext is not supported due to NaN values in the input data.",
            DeprecationWarning,
        )
    elif name in ["tabnet", "TabNet"]:
        device = "cuda" if torch.cuda.is_available() else "cpu"
        model = TabNetClassifier(seed=random_state, device_name=device)
        warnings.warn(
            "TabNet is not supported due to NaN values in the input data.",
            DeprecationWarning,
        )
    else:
        raise ValueError(f"No model found for `{name}`.")

    return model
