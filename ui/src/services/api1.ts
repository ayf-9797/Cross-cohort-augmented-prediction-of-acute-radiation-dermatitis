// ============================================================================
// API Service - Communicates with the R Plumber Backend
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
    throw new Error(`API error: ${response.status} ${response.statusText}`);
  }

  const data: PredictionResult = await response.json();

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

  return response.json();
}
