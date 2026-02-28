// ============================================================================
// FEATURE DEFINITIONS
// ============================================================================
// This file defines the input features for the UI only.
// All prediction logic resides in the R backend (r_backend/plumber.R).
// ============================================================================

// ---------------------------------------------------------------------------
// Type Definitions
// ---------------------------------------------------------------------------

export interface FeatureOption {
  value: number;
  label: string;
}

// A categorical (dropdown) feature
export interface CategoricalFeature {
  key: string;
  label: string;
  type: 'categorical';
  options: FeatureOption[];
}

// A numeric (free-input) feature
export interface NumericFeature {
  key: string;
  label: string;
  type: 'numeric';
  min?: number;
  max?: number;
  step?: number;
  unit?: string;
  placeholder?: string;
}

// Union type for any feature
export type FeatureDefinition = CategoricalFeature | NumericFeature;

export interface FeatureCategory {
  id: string;
  name: string;
  description: string;
  features: FeatureDefinition[];
}

// ---------------------------------------------------------------------------
// Feature Categories and Definitions
// ---------------------------------------------------------------------------

export const FEATURE_CATEGORIES: FeatureCategory[] = [
  {
    id: 'demographics',
    name: 'Patient Demographics',
    description: 'Basic demographic and lifestyle information',
    features: [
      {
        key: 'Sex',
        label: 'Sex',
        type: 'categorical',
        options: [
          { value: 0, label: 'Male' },
          { value: 1, label: 'Female' },
        ],
      },
      {
        key: 'Age',
        label: 'Age',
        type: 'numeric',
        min: 0,
        max: 120,
        step: 1,
        unit: 'years',
        placeholder: 'e.g., 55',
      },
      {
        key: 'Career',
        label: 'Career',
        type: 'categorical',
        options: [
          { value: 0, label: 'Retiree' },
          { value: 1, label: 'Farmer' },
          { value: 2, label: 'Staff' },
          { value: 3, label: 'Professional/Technical Personnel' },
          { value: 4, label: 'Worker' },
          { value: 5, label: 'Government Employee' },
          { value: 6, label: 'Enterprise Manager' },
          { value: 7, label: 'Freelancer' },
          { value: 8, label: 'Self-employed' },
          { value: 9, label: 'Other' },
          { value: 10, label: 'Student' },
        ],
      },
      {
        key: 'Wedding',
        label: 'Marital Status',
        type: 'categorical',
        options: [
          { value: 0, label: 'Married' },
          { value: 1, label: 'Unmarried' },
          { value: 2, label: 'Widowed' },
          { value: 3, label: 'Divorced' },
        ],
      },
      {
        key: 'BMI_level',
        label: 'BMI Level',
        type: 'categorical',
        options: [
          { value: 0, label: 'Normal' },
          { value: 1, label: 'Overweight' },
          { value: 2, label: 'Obese' },
          { value: 3, label: 'Underweight' },
        ],
      },
      {
        key: 'NRS2002',
        label: 'NRS2002 Score',
        type: 'categorical',
        options: [
          { value: 0, label: '0' },
          { value: 1, label: '1' },
          { value: 2, label: '2' },
          { value: 3, label: '3' },
        ],
      },
      {
        key: 'Diabetes',
        label: 'Diabetes',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
      {
        key: 'Drinking',
        label: 'Drinking',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
    ],
  },
  {
    id: 'diagnosis',
    name: 'Diagnosis & Treatment',
    description: 'Cancer diagnosis, target volume, and fixation device',
    features: [
      {
        key: 'Diagnosis',
        label: 'Diagnosis',
        type: 'categorical',
        options: [
          { value: 0, label: 'Lung Cancer' },
          { value: 1, label: 'Head and Neck Tumor' },
          { value: 2, label: 'Breast Cancer' },
          { value: 3, label: 'Esophageal Cancer' },
          { value: 4, label: 'Nasopharyngeal Cancer' },
          { value: 5, label: 'Pancreatic Cancer' },
          { value: 6, label: 'Liver Cancer' },
          { value: 7, label: 'Rectal Cancer' },
          { value: 8, label: 'Prostate Cancer' },
          { value: 9, label: 'Thymic Cancer' },
          { value: 10, label: 'Cervical Cancer' },
          { value: 11, label: 'Other' },
        ],
      },
      {
        key: 'Target_Volume',
        label: 'Target Volume',
        type: 'categorical',
        options: [
          { value: 0, label: 'Lung' },
          { value: 1, label: 'Brain' },
          { value: 2, label: 'Head and Neck' },
          { value: 3, label: 'Esophagus' },
          { value: 4, label: 'Left Breast' },
          { value: 5, label: 'Nasopharynx' },
          { value: 6, label: 'Right Breast' },
          { value: 7, label: 'Lymph Nodes' },
          { value: 8, label: 'Mediastinum' },
          { value: 9, label: 'Chest Wall' },
          { value: 10, label: 'Liver' },
          { value: 11, label: 'Other' },
        ],
      },
      {
        key: 'Assistive_Devices',
        label: 'Assistive Devices',
        type: 'categorical',
        options: [
          { value: 0, label: 'Thermoplastic Positioning Mask for Head and Neck' },
          { value: 1, label: 'Vacuum cushion' },
          { value: 2, label: 'Thermoplastic Positioning Mask for Head and Neck + Foam Adhesive' },
          { value: 3, label: 'Breast Bracket(Supine)' },
          { value: 4, label: 'Thermoplastic Positioning Mask for Head' },
          { value: 5, label: 'Custom Thermoplastic Positioning Mask' },
          { value: 6, label: 'Foam Adhesive + Custom Thermoplastic Positioning Mask' },
          { value: 7, label: 'Thermoplastic Positioning Mask for Whole Body' },
          { value: 8, label: 'Abdominal-Pelvic Fixator' },
          { value: 9, label: 'Foam Adhesive' },
          { value: 10, label: 'Vacuum cushion + Abdominal Compression' },
          { value: 11, label: 'Other' },
        ],
      },
    ],
  },
  {
    id: 'treatment_params',
    name: 'Treatment Parameters',
    description: 'Radiation dose and treatment modalities',
    features: [
      {
        key: 'Dose',
        label: 'Prescribed Dose',
        type: 'numeric',
        min: 0,
        max: 200,
        step: 0.1,
        unit: 'Gy',
        placeholder: 'e.g., 60.0',
      },
      {
        key: 'Average_dose',
        label: 'Average Dose',
        type: 'numeric',
        min: 0,
        max: 200,
        step: 0.1,
        unit: 'Gy',
        placeholder: 'e.g., 45.5',
      },
      {
        key: 'Hormone_therapy',
        label: 'Hormone Therapy',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
      {
        key: 'Chemotherapy',
        label: 'Chemotherapy',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
      {
        key: 'Targeted_therapy',
        label: 'Targeted Therapy',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
      {
        key: 'Endocrine_therapy',
        label: 'Endocrine Therapy',
        type: 'categorical',
        options: [
          { value: 0, label: 'No' },
          { value: 1, label: 'Yes' },
        ],
      },
    ],
  },
  {
    id: 'laboratory',
    name: 'Laboratory Results',
    description: 'Baseline hematological parameters',
    features: [
      {
        key: 'Red_blood_cell',
        label: 'Red Blood Cell Count',
        type: 'categorical',
        options: [
          { value: 0, label: 'Normal' },
          { value: 1, label: 'Decreased' },
          { value: 2, label: 'Increased' },
        ],
      },
      {
        key: 'Hemoglobin',
        label: 'Hemoglobin',
        type: 'categorical',
        options: [
          { value: 0, label: 'Normal' },
          { value: 1, label: 'Decreased' },
          { value: 2, label: 'Increased' },
        ],
      },
      {
        key: 'Platelets',
        label: 'Platelets',
        type: 'categorical',
        options: [
          { value: 0, label: 'Normal' },
          { value: 1, label: 'Decreased' },
          { value: 2, label: 'Increased' },
        ],
      },
    ],
  },
];

// ---------------------------------------------------------------------------
// Helper: Get all features flattened
// ---------------------------------------------------------------------------
export function getAllFeatures(): FeatureDefinition[] {
  return FEATURE_CATEGORIES.flatMap(cat => cat.features);
}

// ---------------------------------------------------------------------------
// Get default feature values
//   - Categorical: 0 (reference level)
//   - Numeric: null (user must fill in)
// ---------------------------------------------------------------------------
export function getDefaultFeatures(): Record<string, number | null> {
  const defaults: Record<string, number | null> = {};
  for (const cat of FEATURE_CATEGORIES) {
    for (const feature of cat.features) {
      if (feature.type === 'numeric') {
        defaults[feature.key] = null;
      } else {
        defaults[feature.key] = 0;
      }
    }
  }
  return defaults;
}

// ---------------------------------------------------------------------------
// Example patient for demonstration
// ---------------------------------------------------------------------------
export function getExamplePatient(): Record<string, number | null> {
  return {
    Sex: 1,
    Age: 55,
    Career: 0,
    Wedding: 0,
    BMI_level: 1,
    NRS2002: 1,
    Diabetes: 0,
    Drinking: 0,
    Diagnosis: 2,
    Target_Volume: 4,
    Assistive_Devices: 3,
    Dose: 50.0,
    Average_dose: 42.5,
    Hormone_therapy: 0,
    Chemotherapy: 1,
    Targeted_therapy: 0,
    Endocrine_therapy: 0,
    Red_blood_cell: 0,
    Hemoglobin: 1,
    Platelets: 0,
  };
}

// ---------------------------------------------------------------------------
// Check if all required numeric fields are filled
// ---------------------------------------------------------------------------
export function getNumericFeatureKeys(): string[] {
  return getAllFeatures()
    .filter(f => f.type === 'numeric')
    .map(f => f.key);
}

export function validateFeatures(features: Record<string, number | null>): { valid: boolean; missing: string[] } {
  const numericKeys = getNumericFeatureKeys();
  const missing: string[] = [];
  for (const key of numericKeys) {
    if (features[key] === null || features[key] === undefined) {
      const feat = getAllFeatures().find(f => f.key === key);
      missing.push(feat ? feat.label : key);
    }
  }
  return { valid: missing.length === 0, missing };
}
