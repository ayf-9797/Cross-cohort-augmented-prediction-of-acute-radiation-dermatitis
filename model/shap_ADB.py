import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib

from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import StratifiedKFold, GridSearchCV, KFold
from sklearn.metrics import roc_auc_score, accuracy_score
from sklearn.preprocessing import OrdinalEncoder, LabelEncoder
from scipy.stats import kendalltau
from collections import Counter

import shap

matplotlib.rcParams["font.family"] = "Arial"


# ==================== 0. 配置 ====================
TRAIN_PATH = "proten-skin/data/202601/ccp_lasso2.xlsx"
TEST_PATH  = "proten-skin/data/202601/independent_test_more_ccp.xlsx"  # 外部独立测试集：不需要Control列
OUT_DIR = "proten-skin/data/202601/outputs_independent_test_shap"
os.makedirs(OUT_DIR, exist_ok=True)

TARGET_COL = "psoriasis"
CONTROL_COL = "Control"   # 仅训练集需要
threshold = 12
n_splits = 5


# ==================== 1. 读取数据（训练集含Control；测试集不含Control） ====================
df_train = pd.read_excel(TRAIN_PATH)
df_test  = pd.read_excel(TEST_PATH)

if TARGET_COL not in df_train.columns:
    raise ValueError(f"训练集缺少目标列 '{TARGET_COL}'")
if CONTROL_COL not in df_train.columns:
    raise ValueError(f"训练集缺少列 '{CONTROL_COL}'（用于分层与AUC计算）")

if TARGET_COL not in df_test.columns:
    raise ValueError(f"独立测试集缺少目标列 '{TARGET_COL}'")
# 测试集不强制要求Control列（按你的要求：默认全是目标数据）
# 如果测试集意外带了Control列，我们也忽略它
if CONTROL_COL in df_test.columns:
    df_test = df_test.drop(columns=[CONTROL_COL])

control_train_raw = df_train[CONTROL_COL].copy()
y_train_raw = df_train[TARGET_COL].copy()
X_train_raw = df_train.drop(columns=[TARGET_COL, CONTROL_COL]).copy()

y_test_raw = df_test[TARGET_COL].copy()
X_test_raw = df_test.drop(columns=[TARGET_COL]).copy()

# 列对齐（防止测试集列顺序不同/缺列/多列）
missing_cols = [c for c in X_train_raw.columns if c not in X_test_raw.columns]
extra_cols = [c for c in X_test_raw.columns if c not in X_train_raw.columns]
if missing_cols:
    raise ValueError(f"独立测试集缺少特征列: {missing_cols}")
if extra_cols:
    X_test_raw = X_test_raw.drop(columns=extra_cols)
X_test_raw = X_test_raw[X_train_raw.columns]

print(f"训练集样本数: {len(X_train_raw)} | 独立测试集样本数: {len(X_test_raw)}")


# ==================== 2. 特征类型划分 + 编码（仅在训练集拟合规则） ====================
categorical_features = []
continuous_features = []
for col in X_train_raw.columns:
    if X_train_raw[col].nunique(dropna=False) > threshold:
        continuous_features.append(col)
    else:
        categorical_features.append(col)

print(f"分类特征: {len(categorical_features)} | 连续特征: {len(continuous_features)}")

X_train = X_train_raw.copy()
X_test  = X_test_raw.copy()

# 连续特征：转数值 + 用训练集median填充
for col in continuous_features:
    med = pd.to_numeric(X_train[col], errors="coerce").median()
    X_train[col] = pd.to_numeric(X_train[col], errors="coerce").fillna(med)
    X_test[col]  = pd.to_numeric(X_test[col], errors="coerce").fillna(med)

# 分类特征：OrdinalEncoder（未知类别 -> -1）
for col in categorical_features:
    X_train[col] = X_train[col].astype(str).fillna("missing")
    X_test[col]  = X_test[col].astype(str).fillna("missing")

enc = OrdinalEncoder(handle_unknown="use_encoded_value", unknown_value=-1)
if categorical_features:
    X_train[categorical_features] = enc.fit_transform(X_train[categorical_features])
    X_test[categorical_features]  = enc.transform(X_test[categorical_features])

# 目标编码：若本身是0/1则保持，否则LabelEncoder
def encode_target(y_tr: pd.Series, y_te: pd.Series):
    y_tr_no_na = y_tr.dropna()
    if pd.api.types.is_numeric_dtype(y_tr_no_na) and set(pd.unique(y_tr_no_na)).issubset({0, 1}):
        return y_tr.astype(int), y_te.astype(int), None
    le = LabelEncoder()
    y_tr_enc = pd.Series(le.fit_transform(y_tr.astype(str)), index=y_tr.index)
    y_te_enc = pd.Series(le.transform(y_te.astype(str)), index=y_te.index)
    return y_tr_enc, y_te_enc, le

y_train, y_test, y_le = encode_target(y_train_raw, y_test_raw)

# Control 归一化到0/1（非0即1）
def normalize_control(s: pd.Series) -> pd.Series:
    s2 = pd.to_numeric(s, errors="coerce").fillna(0)
    return (s2 != 0).astype(int)

control_train = normalize_control(control_train_raw)

# DataFrame 强制数值型
X_train = X_train.astype(float)
X_test  = X_test.astype(float)

print(f"Control==1（训练集）: {int(control_train.sum())} / {len(control_train)}")
print(f"独立测试集默认全为目标数据（不需要Control列）")


# ==================== 3. 构建“按Control均匀分层”的CV切分器 ====================
def build_cv_splitter(X, y, control, n_splits=5, seed=42):
    """
    优先使用“Control + y”的组合分层（兼顾Control均匀 + 标签均匀），
    若组合分层样本太少导致无法分层，则退化为仅按Control分层；
    若Control分层仍不满足，则退化为普通KFold。
    """
    control = control.astype(int)
    y = y.astype(int)

    strata_combo = (control.astype(str) + "_" + y.astype(str))
    combo_counts = strata_combo.value_counts()
    control_counts = control.value_counts()

    if combo_counts.min() >= n_splits:
        print("CV分层策略：按 Control+y 组合分层（保证每折Control==1近似均匀，且尽量保持标签分布）")
        return StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=seed), strata_combo
    elif control_counts.min() >= n_splits:
        print("CV分层策略：仅按 Control 分层（保证每折Control==1近似均匀）")
        return StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=seed), control
    else:
        print("CV分层策略：退化为KFold（注意：可能无法保证每折Control==1均匀）")
        return KFold(n_splits=n_splits, shuffle=True, random_state=seed), None

cv_splitter, stratify_labels = build_cv_splitter(X_train, y_train, control_train, n_splits=n_splits, seed=42)


# ==================== 4. 训练：五折CV（AUC仅在验证折Control==1子集上计算）+ 外部测试集SHAP稳定性 ====================
def make_adaboost(base_estimator):
    """兼容不同sklearn版本：AdaBoostClassifier(estimator=...) vs (base_estimator=...)"""
    try:
        return AdaBoostClassifier(
            estimator=base_estimator,
            algorithm="SAMME.R",
            random_state=42
        )
    except TypeError:
        return AdaBoostClassifier(
            base_estimator=base_estimator,
            algorithm="SAMME.R",
            random_state=42
        )

fold_metrics = []
fold_best_params = []

# 每折：在同一外部独立测试集上算SHAP（正类），做稳定性
feature_importance_folds = []

print("\n" + "=" * 70)
print("开始五折交叉验证：验证AUC仅基于Control==1；SHAP基于独立测试集")
print("=" * 70)

split_iter = cv_splitter.split(X_train, stratify_labels) if stratify_labels is not None else cv_splitter.split(X_train, y_train)

for fold, (train_idx, val_idx) in enumerate(split_iter, 1):
    print(f"\n【第 {fold}/{n_splits} 折】")

    X_tr, X_val = X_train.iloc[train_idx], X_train.iloc[val_idx]
    y_tr, y_val = y_train.iloc[train_idx], y_train.iloc[val_idx]
    control_val = control_train.iloc[val_idx]

    base_estimator = DecisionTreeClassifier(random_state=42)
    ada = make_adaboost(base_estimator)

    # 动态兼容 max_depth 参数路径
    params_keys = set(ada.get_params().keys())
    depth_key = "estimator__max_depth" if "estimator__max_depth" in params_keys else "base_estimator__max_depth"

    param_grid = {
        "n_estimators": [30, 50, 80, 100],
        "learning_rate": [0.01, 0.1, 0.5, 1.0],
        depth_key: [1, 2, 3],
    }

    grid_search = GridSearchCV(
        estimator=ada,
        param_grid=param_grid,
        scoring="roc_auc",
        cv=3,
        n_jobs=-1,
        verbose=0
    )
    grid_search.fit(X_tr, y_tr)

    best_model = grid_search.best_estimator_
    best_params = grid_search.best_params_
    fold_best_params.append(best_params)

    # 验证折预测
    y_val_proba = best_model.predict_proba(X_val)[:, 1]
    y_val_pred = best_model.predict(X_val)

    # AUC：仅在 Control==1 的验证样本上算
    mask = (control_val.values == 1)
    y_val_control = y_val.values[mask]
    y_val_proba_control = y_val_proba[mask]

    if mask.sum() == 0:
        auc_control = np.nan
        auc_note = "（本折验证集Control==1为0，AUC=NaN）"
    elif len(np.unique(y_val_control)) < 2:
        auc_control = np.nan
        auc_note = "（Control==1子集仅单一类别，AUC=NaN）"
    else:
        auc_control = roc_auc_score(y_val_control, y_val_proba_control)
        auc_note = ""

    # 参考：全验证折准确率
    acc_all = accuracy_score(y_val, y_val_pred)

    fold_metrics.append({
        "fold": fold,
        "auc_control_only": auc_control,
        "acc_all_val": acc_all,
        "control1_in_val": int(mask.sum()),
        "val_size": len(val_idx),
        "train_size": len(train_idx),
    })

    print(f"  最佳参数: {best_params}")
    print(f"  验证AUC(Control==1): {auc_control if not np.isnan(auc_control) else 'NaN'} {auc_note}")
    print(f"  验证准确率(全验证折): {acc_all:.4f}")
    print(f"  本折验证集Control==1数量: {int(mask.sum())} / {len(val_idx)}")

    # SHAP：对“外部独立测试集”计算（正类）
    print("  计算SHAP（独立测试集）...")
    try:
        explainer = shap.TreeExplainer(best_model)
        sv = explainer.shap_values(X_test)
        shap_pos = sv[1] if isinstance(sv, list) and len(sv) >= 2 else sv
    except Exception:
        explainer = shap.Explainer(best_model.predict_proba, X_tr)
        exp = explainer(X_test)
        vals = exp.values
        shap_pos = vals[:, :, 1] if vals.ndim == 3 else vals

    feature_importance_folds.append(np.abs(shap_pos).mean(axis=0))


# ==================== 5. CV结果保存 ====================
metrics_df = pd.DataFrame(fold_metrics)
auc_series = metrics_df["auc_control_only"]

print("\n" + "=" * 70)
print("交叉验证结果汇总（验证AUC仅基于Control==1）")
print("=" * 70)
print(f"AUC(Control==1): {auc_series.mean(skipna=True):.4f} ± {auc_series.std(skipna=True):.4f}")
print(f"NaN折数: {int(auc_series.isna().sum())} / {len(auc_series)}")

cv_metrics_path = os.path.join(OUT_DIR, "cross_validation_results_control_only_auc.xlsx")
metrics_df.to_excel(cv_metrics_path, index=False)


# ==================== 6. 选最终超参并训练最终模型（全训练集） ====================
param_counter = Counter([tuple(sorted(p.items())) for p in fold_best_params])
final_params_tuple, _ = param_counter.most_common(1)[0]
final_params = dict(final_params_tuple)

print("\n" + "=" * 70)
print("最终模型训练（全训练集）")
print("=" * 70)
print("选用最终超参（CV中最常出现的best_params）:")
print(final_params)

final_base = DecisionTreeClassifier(random_state=42)
final_model = make_adaboost(final_base)
final_model.set_params(**final_params)
final_model.fit(X_train, y_train)

# 独立测试集指标（测试集没有Control，默认全是目标数据，全量计算）
y_test_proba = final_model.predict_proba(X_test)[:, 1]
y_test_pred = final_model.predict(X_test)

test_acc_all = accuracy_score(y_test, y_test_pred)
test_auc_all = np.nan if len(np.unique(y_test.values)) < 2 else roc_auc_score(y_test, y_test_proba)

print(f"\n独立测试集 AUC(全体): {test_auc_all if not np.isnan(test_auc_all) else 'NaN'}")
print(f"独立测试集 准确率(全体): {test_acc_all:.4f}")

pd.DataFrame([{
    "target_col": TARGET_COL,
    "test_auc_all": test_auc_all,
    "test_accuracy_all": test_acc_all,
    "final_params": str(final_params),
    "train_path": TRAIN_PATH,
    "test_path": TEST_PATH
}]).to_excel(os.path.join(OUT_DIR, "independent_test_results.xlsx"), index=False)


# ==================== 7. SHAP稳定性（跨5折：同一独立测试集上） ====================
feature_importance_df = pd.DataFrame(feature_importance_folds, columns=X_train.columns)
mean_importance = feature_importance_df.mean()
std_importance = feature_importance_df.std()

top_idx = mean_importance.argsort()[::-1][:10]
top_features = mean_importance.index[top_idx]

plt.figure(figsize=(12, 8))
plt.barh(
    range(len(top_features)),
    mean_importance[top_features],
    xerr=std_importance[top_features],
    capsize=5,
    color="steelblue",
    alpha=0.7
)
plt.yticks(range(len(top_features)), top_features, fontsize=12)
plt.xlabel("Mean |SHAP value| (Independent Test, across 5 CV-fold models)", fontsize=13)
plt.title("Feature Importance Stability (SHAP on Independent Test Set)", fontsize=15)
plt.gca().invert_yaxis()
plt.tight_layout()
plt.savefig(os.path.join(OUT_DIR, "feature_importance_stability_independent_test.png"), dpi=600)
plt.close()

# Kendall’s Tau：基于独立测试集SHAP重要性排序
rankings = [np.argsort(imp)[::-1] for imp in feature_importance_folds]
tau_values = [
    kendalltau(rankings[i], rankings[j])[0]
    for i in range(n_splits)
    for j in range(i + 1, n_splits)
]
print(f"\n特征排名一致性 (Kendall's Tau): {np.nanmean(tau_values):.4f} ± {np.nanstd(tau_values):.4f}")


# ==================== 8. 最终模型：独立测试集SHAP可视化与导出 ====================
print("\n" + "=" * 70)
print("最终模型：独立测试集SHAP解释与可视化")
print("=" * 70)

try:
    final_explainer = shap.TreeExplainer(final_model)
    final_sv = final_explainer.shap_values(X_test)
    if isinstance(final_sv, list) and len(final_sv) >= 2:
        shap_pos_final = final_sv[1]
        base_value_pos_final = (
            final_explainer.expected_value[1]
            if isinstance(final_explainer.expected_value, (list, np.ndarray))
            else final_explainer.expected_value
        )
    else:
        shap_pos_final = final_sv
        base_value_pos_final = final_explainer.expected_value
except Exception:
    final_explainer = shap.Explainer(final_model.predict_proba, X_train)
    final_exp = final_explainer(X_test)
    vals = final_exp.values
    base = final_exp.base_values
    if vals.ndim == 3:
        shap_pos_final = vals[:, :, 1]
        base_value_pos_final = base[:, 1] if np.ndim(base) == 2 else base
    else:
        shap_pos_final = vals
        base_value_pos_final = base

shap_explanation_final = shap.Explanation(
    values=shap_pos_final,
    base_values=np.array([base_value_pos_final] * len(X_test)) if np.isscalar(base_value_pos_final) else base_value_pos_final,
    data=X_test.values,
    feature_names=X_test.columns.tolist()
)

# Summary dot
shap.summary_plot(
    shap_explanation_final,
    X_test,
    max_display=min(len(X_test.columns), 10),
    plot_type="dot",
    show=False
)
plt.title("SHAP Summary Plot (Final Model, Independent Test)", fontsize=16)
plt.tight_layout()
plt.savefig(os.path.join(OUT_DIR, "shap_summary_independent_test.png"), dpi=600)
plt.close()

# Bar
shap.plots.bar(shap_explanation_final, max_display=10, show=False)
plt.tight_layout()
plt.savefig(os.path.join(OUT_DIR, "shap_bar_independent_test.png"), dpi=600)
plt.close()

# 分组对比（按 y_test）
y_test_array = y_test.values
shap_values_0 = shap_pos_final[y_test_array == 0]
shap_values_1 = shap_pos_final[y_test_array == 1]

mean_0 = np.mean(np.abs(shap_values_0), axis=0) if len(shap_values_0) else np.zeros(X_test.shape[1])
mean_1 = np.mean(np.abs(shap_values_1), axis=0) if len(shap_values_1) else np.zeros(X_test.shape[1])
std_0  = np.std(np.abs(shap_values_0), axis=0) if len(shap_values_0) else np.zeros(X_test.shape[1])
std_1  = np.std(np.abs(shap_values_1), axis=0) if len(shap_values_1) else np.zeros(X_test.shape[1])

feature_names = X_test.columns.to_numpy()
N = min(len(feature_names), 10)
top_feat_idx = np.argsort(mean_0 + mean_1)[::-1][:N]

plt.figure(figsize=(14, 10))
y_pos = np.arange(N)
height = 0.36
gap = 0.04
plt.barh(
    y_pos - height / 2 - gap / 2,
    mean_0[top_feat_idx],
    height,
    xerr=std_0[top_feat_idx],
    capsize=4,
    color="#1f77b4",
    alpha=0.85,
    label="Class 0"
)
plt.barh(
    y_pos + height / 2 + gap / 2,
    mean_1[top_feat_idx],
    height,
    xerr=std_1[top_feat_idx],
    capsize=4,
    color="#d62728",
    alpha=0.85,
    label="Class 1"
)
plt.yticks(y_pos, feature_names[top_feat_idx], fontsize=13)
plt.xlabel("Mean(|SHAP value|) ± Std (Independent Test)", fontsize=13)
plt.title("Feature Impact Comparison (Final Model, Independent Test)", fontsize=15, pad=10)
plt.legend(fontsize=11, loc="lower right")
plt.gca().invert_yaxis()
plt.grid(axis="x", linestyle="--", alpha=0.3)
plt.tight_layout()
plt.savefig(os.path.join(OUT_DIR, "group_shap_value_independent_test.png"), dpi=600)
plt.close()

# Waterfall 示例（独立测试集第0个样本）
try:
    shap.plots.waterfall(shap_explanation_final[0], show=False)
    plt.tight_layout()
    plt.savefig(os.path.join(OUT_DIR, "waterfall_independent_test_sample0.png"), dpi=300)
    plt.close()
except Exception as e:
    print(f"Waterfall 绘制失败（可能是SHAP版本差异）：{e}")

# 保存最终模型独立测试集SHAP重要性
importance_summary = pd.DataFrame({
    "Feature": X_test.columns,
    "Mean_Importance": np.abs(shap_pos_final).mean(axis=0),
    "Std_Importance": np.abs(shap_pos_final).std(axis=0),
})
importance_summary["CV"] = importance_summary["Std_Importance"] / (importance_summary["Mean_Importance"] + 1e-10)
importance_summary = importance_summary.sort_values("Mean_Importance", ascending=False)

importance_path = os.path.join(OUT_DIR, "feature_importance_independent_test_shap.xlsx")
importance_summary.to_excel(importance_path, index=False)

print("\n所有分析完成，输出文件包括：")
print(f" - {cv_metrics_path}")
print(f" - {os.path.join(OUT_DIR, 'independent_test_results.xlsx')}")
print(f" - {os.path.join(OUT_DIR, 'feature_importance_stability_independent_test.png')}")
print(f" - {os.path.join(OUT_DIR, 'shap_summary_independent_test.png')}")
print(f" - {os.path.join(OUT_DIR, 'shap_bar_independent_test.png')}")
print(f" - {os.path.join(OUT_DIR, 'group_shap_value_independent_test.png')}")
print(f" - {os.path.join(OUT_DIR, 'waterfall_independent_test_sample0.png')}")
print(f" - {importance_path}")


