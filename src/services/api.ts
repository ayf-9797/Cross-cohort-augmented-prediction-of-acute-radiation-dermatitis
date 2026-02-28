// ============================================================================
// API Service - Communicates with the R Plumber Backend
// ============================================================================
//
// IMPORTANT: R Plumber's JSON serializer may wrap scalar values in arrays.
// For example:  R:  list(probability = 0.72)
//               JSON: {"probability": [0.72]}   (not 0.72!)
//
// The normalizeResult() function below handles this by "unwrapping" any
// single-element arrays back to scalars, ensuring the frontend always
// receives clean JavaScript primitives.
// ============================================================================

import { API_ENDPOINTS } from "../config";

export interface PredictionResult {
  success: boolean;
  probability: number;
  threshold: number;
  classification: string;
  is_high_risk: boolean;
  metrics: {
    auc: number;
    sensitivity: number;
    specificity: number;
    accuracy: number;
    ppv: number;
    npv: number;
    training_cohort: string;
    validation_cohort: string;
    total_patients: number;
  };
  error?: string;
}

export interface ModelInfo {
  threshold: number;
  metrics: PredictionResult["metrics"];
}

// ============================================================================
// Unwrap helper: R Plumber may return [0.72] instead of 0.72
// ============================================================================

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function unwrap(val: any): any {
  if (Array.isArray(val) && val.length === 1) return val[0];
  if (Array.isArray(val) && val.length === 0) return undefined;
  return val;
}

function toNum(val: unknown, fallback: number = 0): number {
  const n = Number(unwrap(val));
  return isNaN(n) ? fallback : n;
}

function toStr(val: unknown, fallback: string = ""): string {
  const v = unwrap(val);
  if (v === null || v === undefined) return fallback;
  return String(v);
}

function toBool(val: unknown): boolean {
  const v = unwrap(val);
  return Boolean(v);
}

// ============================================================================
// Normalize the raw JSON response from R Plumber
// ============================================================================
// This is the critical function that prevents white-screen crashes.
// It ensures every field is the correct JavaScript type, regardless
// of how R Plumber serialized it (scalar, array, null, etc.)
// ============================================================================

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function normalizeResult(raw: any): PredictionResult {
  // Handle raw.metrics safely — it might be undefined, null, or nested
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const rawMetrics: any = raw?.metrics ?? {};

  return {
    success: toBool(raw?.success),
    probability: toNum(raw?.probability, 0),
    threshold: toNum(raw?.threshold, 0.5),
    classification: toStr(raw?.classification, "Unknown"),
    is_high_risk: toBool(raw?.is_high_risk),
    metrics: {
      auc: toNum(rawMetrics?.auc, 0),
      sensitivity: toNum(rawMetrics?.sensitivity, 0),
      specificity: toNum(rawMetrics?.specificity, 0),
      accuracy: toNum(rawMetrics?.accuracy, 0),
      ppv: toNum(rawMetrics?.ppv, 0),
      npv: toNum(rawMetrics?.npv, 0),
      training_cohort: toStr(rawMetrics?.training_cohort, "N/A"),
      validation_cohort: toStr(rawMetrics?.validation_cohort, "N/A"),
      total_patients: toNum(rawMetrics?.total_patients, 0),
    },
    error: raw?.error ? toStr(raw.error) : undefined,
  };
}

// ============================================================================
// API Functions
// ============================================================================

/**
 * Call the R backend to generate a prediction
 */
export async function fetchPrediction(
  features: Record<string, number>
): Promise<PredictionResult> {
  const response = await fetch(API_ENDPOINTS.predict, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(features),
  });

  if (!response.ok) {
    const text = await response.text().catch(() => "");
    throw new Error(
      `API error: ${response.status} ${response.statusText}${text ? " — " + text : ""}`
    );
  }

  const raw = await response.json();

  // Normalize: unwrap R arrays, cast types, fill defaults
  const data = normalizeResult(raw);

  if (!data.success) {
    throw new Error(data.error || "Prediction failed on the R backend");
  }

  return data;
}

/**
 * Check if the R backend is running
 */
export async function checkHealth(): Promise<boolean> {
  try {
    const response = await fetch(API_ENDPOINTS.health, {
      method: "GET",
      signal: AbortSignal.timeout(3000),
    });
    return response.ok;
  } catch {
    return false;
  }
}

/**
 * Fetch model metadata from the R backend
 */
export async function fetchModelInfo(): Promise<ModelInfo> {
  const response = await fetch(API_ENDPOINTS.modelInfo, {
    method: "GET",
  });

  if (!response.ok) {
    throw new Error(`API error: ${response.status}`);
  }

  const raw = await response.json();
  return {
    threshold: toNum(raw?.threshold, 0.5),
    metrics: {
      auc: toNum(raw?.metrics?.auc, 0),
      sensitivity: toNum(raw?.metrics?.sensitivity, 0),
      specificity: toNum(raw?.metrics?.specificity, 0),
      accuracy: toNum(raw?.metrics?.accuracy, 0),
      ppv: toNum(raw?.metrics?.ppv, 0),
      npv: toNum(raw?.metrics?.npv, 0),
      training_cohort: toStr(raw?.metrics?.training_cohort, "N/A"),
      validation_cohort: toStr(raw?.metrics?.validation_cohort, "N/A"),
      total_patients: toNum(raw?.metrics?.total_patients, 0),
    },
  };
}
