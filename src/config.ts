// ============================================================================
// Frontend Configuration
// ============================================================================
// Set the R Plumber API base URL here.
// During development, the R API typically runs on localhost:8000
// For production, change this to your deployed server URL.
// ============================================================================

export const API_BASE_URL = "http://127.0.0.1:8000";

// API Endpoints
export const API_ENDPOINTS = {
  predict: `${API_BASE_URL}/api/predict`,
  health: `${API_BASE_URL}/api/health`,
  modelInfo: `${API_BASE_URL}/api/model-info`,
};
