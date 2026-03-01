import { useState, useCallback, useEffect, useRef, Component, type ReactNode } from 'react';
import {
  FEATURE_CATEGORIES,
  getDefaultFeatures,
  getExamplePatient,
  getAllFeatures,
  validateFeatures,
  type FeatureDefinition,
  type CategoricalFeature,
  type NumericFeature,
} from './data/featureConfig';
import { fetchPrediction, checkHealth, type PredictionResult } from './services/api';
import { API_BASE_URL } from './config';

// ============================================================================
// Error Boundary — Prevents white screen on rendering errors
// ============================================================================

interface ErrorBoundaryProps {
  children: ReactNode;
  fallback?: ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

class ErrorBoundary extends Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return { hasError: true, error };
  }

  render() {
    if (this.state.hasError) {
      return this.props.fallback || (
        <div className="bg-red-50 border border-red-200 rounded-xl p-6 text-center">
          <div className="text-red-600 font-bold mb-2">Rendering Error</div>
          <div className="text-sm text-red-500 mb-3">
            {this.state.error?.message || 'An unexpected error occurred while displaying results.'}
          </div>
          <button
            onClick={() => this.setState({ hasError: false, error: null })}
            className="px-4 py-2 text-sm bg-red-600 text-white rounded-lg hover:bg-red-700 cursor-pointer"
          >
            Retry
          </button>
        </div>
      );
    }
    return this.props.children;
  }
}

// Safe number formatting helper
function safeFixed(value: unknown, digits: number = 3): string {
  const n = Number(value);
  if (isNaN(n) || value === null || value === undefined) return "—";
  return n.toFixed(digits);
}

// ============================================================================
// SVG Icons
// ============================================================================

function IconUser() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" />
      <circle cx="12" cy="7" r="4" />
    </svg>
  );
}

function IconHospital() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M3 21h18" />
      <path d="M5 21V7l8-4v18" />
      <path d="M19 21V11l-6-4" />
      <path d="M9 9h1" /><path d="M9 13h1" /><path d="M9 17h1" />
    </svg>
  );
}

function IconRadiation() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <circle cx="12" cy="12" r="2" />
      <path d="M12 2v4" /><path d="M12 18v4" />
      <path d="m4.93 4.93 2.83 2.83" /><path d="m16.24 16.24 2.83 2.83" />
      <path d="M2 12h4" /><path d="M18 12h4" />
      <path d="m4.93 19.07 2.83-2.83" /><path d="m16.24 4.93 2.83 2.83" />
    </svg>
  );
}

function IconMicroscope() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round">
      <path d="M6 18h8" /><path d="M3 22h18" /><path d="M14 22a7 7 0 1 0 0-14h-1" />
      <path d="M9 14h2" /><path d="M9 12a2 2 0 0 1-2-2V6h6v4a2 2 0 0 1-2 2Z" />
      <path d="M12 6V3a1 1 0 0 0-1-1H9a1 1 0 0 0-1 1v3" />
    </svg>
  );
}

function IconChevronDown({ open }: { open: boolean }) {
  return (
    <svg
      width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor"
      strokeWidth="2" strokeLinecap="round" strokeLinejoin="round"
      className={`transition-transform duration-200 ${open ? 'rotate-180' : ''}`}
    >
      <path d="m6 9 6 6 6-6" />
    </svg>
  );
}

function IconWarning() {
  return (
    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <path d="m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3Z" />
      <path d="M12 9v4" /><path d="M12 17h.01" />
    </svg>
  );
}

function IconShield() {
  return (
    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z" />
      <path d="m9 12 2 2 4-4" />
    </svg>
  );
}

function IconInfo() {
  return (
    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <circle cx="12" cy="12" r="10" /><path d="M12 16v-4" /><path d="M12 8h.01" />
    </svg>
  );
}

function IconServer() {
  return (
    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <rect width="20" height="8" x="2" y="2" rx="2" ry="2" />
      <rect width="20" height="8" x="2" y="14" rx="2" ry="2" />
      <line x1="6" x2="6.01" y1="6" y2="6" />
      <line x1="6" x2="6.01" y1="18" y2="18" />
    </svg>
  );
}

function IconLoader() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="animate-spin">
      <path d="M21 12a9 9 0 1 1-6.219-8.56" />
    </svg>
  );
}

const CATEGORY_ICONS: Record<string, React.ReactNode> = {
  demographics: <IconUser />,
  diagnosis: <IconHospital />,
  treatment_params: <IconRadiation />,
  laboratory: <IconMicroscope />,
};

// ============================================================================
// SelectField Component (for categorical features)
// ============================================================================

interface SelectFieldProps {
  label: string;
  featureKey: string;
  options: { value: number; label: string }[];
  value: number | null;
  onChange: (key: string, value: number | null) => void;
}

function SelectField({ label, featureKey, options, value, onChange }: SelectFieldProps) {
  return (
    <div className="flex flex-col gap-1.5">
      <label className="text-xs font-semibold text-slate-600 uppercase tracking-wider">
        {label}
      </label>
      <select
        value={value ?? 0}
        onChange={(e) => onChange(featureKey, parseInt(e.target.value, 10))}
        className="w-full rounded-lg border border-slate-200 bg-white px-3 py-2.5 text-sm text-slate-800 shadow-sm transition-all duration-150 hover:border-slate-300 focus:border-blue-500 focus:ring-2 focus:ring-blue-500/20 focus:outline-none appearance-none cursor-pointer"
        style={{
          backgroundImage: `url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e")`,
          backgroundPosition: 'right 0.5rem center',
          backgroundRepeat: 'no-repeat',
          backgroundSize: '1.5em 1.5em',
          paddingRight: '2.5rem',
        }}
      >
        {options.map((opt) => (
          <option key={opt.value} value={opt.value}>
            {opt.label}
          </option>
        ))}
      </select>
    </div>
  );
}

// ============================================================================
// NumericField Component (for continuous features)
// ============================================================================

interface NumericFieldProps {
  feature: NumericFeature;
  value: number | null;
  onChange: (key: string, value: number | null) => void;
}

function NumericField({ feature, value, onChange }: NumericFieldProps) {
  const { key, label, min, max, step, unit, placeholder } = feature;
  const isEmpty = value === null || value === undefined;

  return (
    <div className="flex flex-col gap-1.5">
      <label className="text-xs font-semibold text-slate-600 uppercase tracking-wider flex items-center gap-1.5">
        {label}
        {unit && <span className="text-xs font-normal text-slate-400 normal-case">({unit})</span>}
      </label>
      <div className="relative">
        <input
          type="number"
          value={isEmpty ? '' : value}
          min={min}
          max={max}
          step={step ?? 'any'}
          placeholder={placeholder ?? ''}
          onChange={(e) => {
            const raw = e.target.value;
            if (raw === '') {
              onChange(key, null);
            } else {
              onChange(key, parseFloat(raw));
            }
          }}
          className={`w-full rounded-lg border bg-white px-3 py-2.5 text-sm text-slate-800 shadow-sm transition-all duration-150 hover:border-slate-300 focus:border-blue-500 focus:ring-2 focus:ring-blue-500/20 focus:outline-none ${
            isEmpty ? 'border-amber-300 bg-amber-50/30' : 'border-slate-200'
          }`}
        />
        {unit && (
          <span className="absolute right-3 top-1/2 -translate-y-1/2 text-xs text-slate-400 pointer-events-none">
            {unit}
          </span>
        )}
      </div>
      {isEmpty && (
        <span className="text-xs text-amber-600 flex items-center gap-1">
          <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><circle cx="12" cy="12" r="10" /><path d="M12 8v4" /><path d="M12 16h.01" /></svg>
          Required
        </span>
      )}
    </div>
  );
}

// ============================================================================
// FeatureField — dispatches to SelectField or NumericField
// ============================================================================

interface FeatureFieldProps {
  feature: FeatureDefinition;
  value: number | null;
  onChange: (key: string, value: number | null) => void;
}

function FeatureField({ feature, value, onChange }: FeatureFieldProps) {
  if (feature.type === 'numeric') {
    return <NumericField feature={feature} value={value} onChange={onChange} />;
  }
  const cat = feature as CategoricalFeature;
  return (
    <SelectField
      label={cat.label}
      featureKey={cat.key}
      options={cat.options}
      value={value}
      onChange={onChange}
    />
  );
}

// ============================================================================
// CategorySection Component
// ============================================================================

interface CategorySectionProps {
  category: typeof FEATURE_CATEGORIES[0];
  features: Record<string, number | null>;
  onChange: (key: string, value: number | null) => void;
  defaultOpen?: boolean;
}

function CategorySection({ category, features, onChange, defaultOpen = true }: CategorySectionProps) {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  // Check if this category has unfilled numeric fields
  const hasUnfilled = category.features.some(
    f => f.type === 'numeric' && (features[f.key] === null || features[f.key] === undefined)
  );

  return (
    <div className="border border-slate-200 rounded-xl overflow-hidden bg-white shadow-sm">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between px-5 py-4 bg-slate-50/80 hover:bg-slate-100/80 transition-colors duration-150 cursor-pointer"
      >
        <div className="flex items-center gap-3">
          <div className="text-blue-700">
            {CATEGORY_ICONS[category.id]}
          </div>
          <div className="text-left">
            <h3 className="text-sm font-bold text-slate-800 flex items-center gap-2">
              {category.name}
              {hasUnfilled && (
                <span className="w-2 h-2 rounded-full bg-amber-400 animate-pulse" title="Has unfilled required fields"></span>
              )}
            </h3>
            <p className="text-xs text-slate-500 mt-0.5">{category.description}</p>
          </div>
        </div>
        <IconChevronDown open={isOpen} />
      </button>
      {isOpen && (
        <div className="px-5 py-4 border-t border-slate-100">
          <div className={`grid gap-4 ${
            category.features.some(f => f.type === 'categorical' && (f as CategoricalFeature).options.length > 4)
              ? 'grid-cols-1 sm:grid-cols-2'
              : 'grid-cols-2 sm:grid-cols-3'
          }`}>
            {category.features.map((feature) => (
              <FeatureField
                key={feature.key}
                feature={feature}
                value={features[feature.key]}
                onChange={onChange}
              />
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

// ============================================================================
// ProbabilityGauge Component
// ============================================================================

function ProbabilityGauge({ probability, threshold }: { probability: number; threshold: number }) {
  const pct = Number(probability) * 100 || 0;
  const thresholdPct = Number(threshold) * 100 || 50;
  const isHighRisk = Number(probability) >= Number(threshold);

  const getBarColor = () => {
    if (pct < 30) return 'from-emerald-400 to-emerald-500';
    if (pct < 50) return 'from-amber-400 to-amber-500';
    if (pct < 70) return 'from-orange-400 to-orange-500';
    return 'from-red-400 to-red-500';
  };

  return (
    <div className="space-y-4">
      <div className="text-center">
        <div className={`text-5xl font-bold tracking-tight ${isHighRisk ? 'text-red-600' : 'text-emerald-600'}`}>
          {pct.toFixed(1)}%
        </div>
        <div className="text-xs text-slate-500 mt-1 font-medium uppercase tracking-wider">
          Predicted Probability
        </div>
      </div>

      <div className="relative pt-2 pb-6">
        <div className="w-full h-4 bg-slate-100 rounded-full overflow-hidden relative">
          <div
            className={`h-full rounded-full bg-gradient-to-r ${getBarColor()} transition-all duration-700 ease-out`}
            style={{ width: `${Math.min(pct, 100)}%` }}
          />
        </div>

        <div
          className="absolute top-0 flex flex-col items-center"
          style={{ left: `${thresholdPct}%`, transform: 'translateX(-50%)' }}
        >
          <div className="w-0.5 h-8 bg-slate-800" />
          <div className="mt-1 text-xs font-semibold text-slate-700 whitespace-nowrap bg-white px-1.5 py-0.5 rounded border border-slate-200 shadow-sm">
            Cutoff: {thresholdPct.toFixed(1)}%
          </div>
        </div>

        <div className="flex justify-between mt-1 text-xs text-slate-400 font-medium">
          <span>0%</span>
          <span>25%</span>
          <span>50%</span>
          <span>75%</span>
          <span>100%</span>
        </div>
      </div>
    </div>
  );
}

// ============================================================================
// ClassificationResult Component
// ============================================================================

function ClassificationResult({ result }: { result: PredictionResult }) {
  const probability = Number(result?.probability) || 0;
  const threshold = Number(result?.threshold) || 0.5;
  const is_high_risk = Boolean(result?.is_high_risk);
  const classification = String(result?.classification || 'Unknown');

  return (
    <div className={`rounded-xl p-5 border-2 ${
      is_high_risk
        ? 'bg-red-50 border-red-200'
        : 'bg-emerald-50 border-emerald-200'
    }`}>
      <div className="flex items-center gap-3">
        <div className={`flex-shrink-0 w-10 h-10 rounded-full flex items-center justify-center ${
          is_high_risk ? 'bg-red-100 text-red-600' : 'bg-emerald-100 text-emerald-600'
        }`}>
          {is_high_risk ? <IconWarning /> : <IconShield />}
        </div>
        <div>
          <div className={`text-lg font-bold ${is_high_risk ? 'text-red-700' : 'text-emerald-700'}`}>
            {classification}
          </div>
          <div className={`text-xs ${is_high_risk ? 'text-red-600' : 'text-emerald-600'}`}>
            Acute Radiation Dermatitis
          </div>
        </div>
      </div>
      <div className={`mt-3 pt-3 border-t text-sm leading-relaxed ${
        is_high_risk ? 'border-red-200 text-red-700' : 'border-emerald-200 text-emerald-700'
      }`}>
        {is_high_risk ? (
          <>
            The predicted probability (<strong>{safeFixed(probability * 100, 1)}%</strong>) exceeds the
            optimal classification threshold (<strong>{safeFixed(threshold * 100, 1)}%</strong>),
            indicating an elevated risk of developing acute radiation dermatitis
            during proton beam therapy.
          </>
        ) : (
          <>
            The predicted probability (<strong>{safeFixed(probability * 100, 1)}%</strong>) is below the
            optimal classification threshold (<strong>{safeFixed(threshold * 100, 1)}%</strong>),
            suggesting a lower risk of developing acute radiation dermatitis
            during proton beam therapy.
          </>
        )}
      </div>
    </div>
  );
}

// ============================================================================
// MetricsTable Component
// ============================================================================

function MetricsTable({ metrics }: { metrics: PredictionResult['metrics'] }) {
  if (!metrics) return null;
  const items = [
    { label: 'AUC', value: safeFixed(metrics.auc, 3) },
    { label: 'Sensitivity', value: safeFixed(metrics.sensitivity, 3) },
    { label: 'Specificity', value: safeFixed(metrics.specificity, 3) },
    { label: 'Accuracy', value: safeFixed(metrics.accuracy, 3) },
    { label: 'PPV', value: safeFixed(metrics.ppv, 3) },
    { label: 'NPV', value: safeFixed(metrics.npv, 3) },
  ];

  return (
    <div className="grid grid-cols-3 gap-3">
      {items.map((m) => (
        <div key={m.label} className="text-center p-3 bg-slate-50 rounded-lg border border-slate-100">
          <div className="text-lg font-bold text-slate-800">{m.value}</div>
          <div className="text-xs text-slate-500 mt-0.5 font-medium">{m.label}</div>
        </div>
      ))}
    </div>
  );
}

// ============================================================================
// FeatureSummaryTable Component
// ============================================================================

function FeatureSummaryTable({ features }: { features: Record<string, number | null> }) {
  const allFeatures = getAllFeatures();

  const getDisplayValue = (feat: FeatureDefinition, value: number | null): string => {
    if (value === null || value === undefined) return '—';
    if (feat.type === 'numeric') {
      const nf = feat as NumericFeature;
      return nf.unit ? `${value} ${nf.unit}` : String(value);
    }
    const cf = feat as CategoricalFeature;
    const opt = cf.options.find(o => o.value === value);
    return opt ? opt.label : String(value);
  };

  const getEncodedValue = (feat: FeatureDefinition, value: number | null): string => {
    if (value === null || value === undefined) return 'null';
    if (feat.type === 'numeric') return String(value);
    return String(value);
  };

  const getTypeTag = (feat: FeatureDefinition) => {
    if (feat.type === 'numeric') {
      return <span className="text-xs px-1.5 py-0.5 bg-purple-100 text-purple-700 rounded font-medium">Numeric</span>;
    }
    return <span className="text-xs px-1.5 py-0.5 bg-blue-100 text-blue-700 rounded font-medium">Factor</span>;
  };

  return (
    <div className="border border-slate-200 rounded-lg overflow-hidden">
      <table className="w-full text-sm">
        <thead>
          <tr className="bg-slate-50">
            <th className="text-left px-4 py-2.5 font-semibold text-slate-700 border-b border-slate-200">Feature</th>
            <th className="text-left px-4 py-2.5 font-semibold text-slate-700 border-b border-slate-200">Type</th>
            <th className="text-left px-4 py-2.5 font-semibold text-slate-700 border-b border-slate-200">Value</th>
            <th className="text-right px-4 py-2.5 font-semibold text-slate-700 border-b border-slate-200">Sent</th>
          </tr>
        </thead>
        <tbody>
          {allFeatures.map((feat, idx) => {
            const value = features[feat.key];
            return (
              <tr key={feat.key} className={idx % 2 === 0 ? 'bg-white' : 'bg-slate-50/50'}>
                <td className="px-4 py-2 text-slate-600 border-b border-slate-100">{feat.label}</td>
                <td className="px-4 py-2 border-b border-slate-100">{getTypeTag(feat)}</td>
                <td className="px-4 py-2 text-slate-800 font-medium border-b border-slate-100">{getDisplayValue(feat, value)}</td>
                <td className="px-4 py-2 text-slate-400 text-right font-mono text-xs border-b border-slate-100">{getEncodedValue(feat, value)}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

// ============================================================================
// Backend Status Banner
// ============================================================================

function BackendStatusBanner({ isConnected, isChecking }: { isConnected: boolean | null; isChecking: boolean }) {
  if (isChecking) {
    return (
      <div className="bg-blue-50 border border-blue-200 rounded-xl p-4 flex items-center gap-3">
        <IconLoader />
        <div>
          <div className="text-sm font-semibold text-blue-800">Connecting to R Backend...</div>
          <div className="text-xs text-blue-600 mt-0.5">Checking <code className="bg-blue-100 px-1 rounded">{API_BASE_URL}</code></div>
        </div>
      </div>
    );
  }

  if (isConnected === false) {
    return (
      <div className="bg-amber-50 border border-amber-200 rounded-xl p-5">
        <div className="flex items-start gap-3">
          <div className="flex-shrink-0 mt-0.5 text-amber-600"><IconWarning /></div>
          <div className="flex-1">
            <div className="text-sm font-semibold text-amber-800">R Backend Not Connected</div>
            <div className="text-xs text-amber-700 mt-1 leading-relaxed">
              The R Plumber API at <code className="bg-amber-100 px-1 rounded">{API_BASE_URL}</code> is not reachable.
            </div>
            <div className="mt-4 space-y-3">
              <div className="text-xs font-bold text-amber-800 uppercase tracking-wider">Quick Start (Windows / RStudio)</div>
              <div className="flex gap-3 p-3 bg-white/60 rounded-lg">
                <div className="flex-shrink-0 w-6 h-6 rounded-full bg-amber-600 text-white flex items-center justify-center text-xs font-bold">1</div>
                <div className="text-xs text-amber-900">
                  Place <code className="bg-amber-100 px-1 rounded">model.rds</code> into <code className="bg-amber-100 px-1 rounded">r_backend\</code>
                </div>
              </div>
              <div className="flex gap-3 p-3 bg-white/60 rounded-lg">
                <div className="flex-shrink-0 w-6 h-6 rounded-full bg-amber-600 text-white flex items-center justify-center text-xs font-bold">2</div>
                <div className="text-xs text-amber-900">
                  In RStudio: open <code className="bg-amber-100 px-1 rounded">r_backend\deploy_frontend.R</code> → click <strong>Source</strong>
                </div>
              </div>
              <div className="flex gap-3 p-3 bg-white/60 rounded-lg">
                <div className="flex-shrink-0 w-6 h-6 rounded-full bg-amber-600 text-white flex items-center justify-center text-xs font-bold">3</div>
                <div className="text-xs text-amber-900">
                  In RStudio: open <code className="bg-amber-100 px-1 rounded">r_backend\run_api.R</code> → click <strong>Source</strong>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-emerald-50 border border-emerald-200 rounded-xl p-4 flex items-center gap-3">
      <div className="flex-shrink-0 w-8 h-8 rounded-full bg-emerald-100 flex items-center justify-center text-emerald-600">
        <IconServer />
      </div>
      <div>
        <div className="text-sm font-semibold text-emerald-800">R Backend Connected</div>
        <div className="text-xs text-emerald-600 mt-0.5">
          API endpoint: <code className="bg-emerald-100 px-1 rounded">{API_BASE_URL}</code> — Model loaded and ready
        </div>
      </div>
    </div>
  );
}

// ============================================================================
// Architecture Diagram
// ============================================================================
function ArchitectureDiagram() {
  return (
    <div className="bg-white border border-slate-200 rounded-xl p-6 sm:p-8 shadow-sm">
      <h2 className="text-lg font-bold text-slate-800 mb-2">System Architecture</h2>
      <p className="text-sm text-slate-500 mb-6">
        This application uses a decoupled architecture: the React frontend handles user interaction,
        while all prediction logic executes on the R Plumber backend.
      </p>

      {/* Architecture Flow */}
      <div className="flex flex-col md:flex-row items-stretch gap-4 mb-8">
        <div className="flex-1 border-2 border-blue-200 rounded-xl p-5 bg-blue-50/50">
          <div className="flex items-center gap-2 mb-3">
            <div className="w-8 h-8 rounded-lg bg-blue-600 text-white flex items-center justify-center">
              <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><rect x="2" y="3" width="20" height="14" rx="2" /><line x1="8" x2="16" y1="21" y2="21" /><line x1="12" x2="12" y1="17" y2="21" /></svg>
            </div>
            <h3 className="text-sm font-bold text-blue-800">Frontend (React)</h3>
          </div>
          <ul className="text-xs text-blue-700 space-y-1.5">
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-blue-400 flex-shrink-0"></span>
              Dropdown selectors for categorical features
            </li>
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-blue-400 flex-shrink-0"></span>
              Numeric inputs for continuous features (Age, Dose, etc.)
            </li>
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-blue-400 flex-shrink-0"></span>
              Send JSON to R API & display results
            </li>
          </ul>
        </div>

        <div className="flex items-center justify-center md:flex-col gap-1 text-slate-400">
          <span className="hidden md:block text-xs font-mono">POST /api/predict</span>
          <svg className="w-8 h-8 hidden md:block" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M5 12h14M12 5l7 7-7 7" /></svg>
          <svg className="w-8 h-8 hidden md:block rotate-180" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M5 12h14M12 5l7 7-7 7" /></svg>
          <span className="hidden md:block text-xs font-mono">JSON Response</span>
          <svg className="w-6 h-6 md:hidden rotate-90" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M5 12h14M12 5l7 7-7 7" /></svg>
        </div>

        <div className="flex-1 border-2 border-emerald-200 rounded-xl p-5 bg-emerald-50/50">
          <div className="flex items-center gap-2 mb-3">
            <div className="w-8 h-8 rounded-lg bg-emerald-600 text-white flex items-center justify-center text-xs font-bold">R</div>
            <h3 className="text-sm font-bold text-emerald-800">Backend (R Plumber)</h3>
          </div>
          <ul className="text-xs text-emerald-700 space-y-1.5">
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-emerald-400 flex-shrink-0"></span>
              Load trained model (.rds)
            </li>
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-emerald-400 flex-shrink-0"></span>
              Factor conversion + dummy encoding (categorical)
            </li>
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-emerald-400 flex-shrink-0"></span>
              Numeric passthrough (Age, Dose, Average_dose)
            </li>
            <li className="flex items-start gap-1.5">
              <span className="mt-1 w-1 h-1 rounded-full bg-emerald-400 flex-shrink-0"></span>
              Execute predict() and return probability
            </li>
          </ul>
        </div>
      </div>

      {/* Data Flow Detail */}
      <div className="bg-slate-50 rounded-lg p-4 border border-slate-200">
        <h4 className="text-xs font-bold text-slate-600 mb-3 uppercase tracking-wider">Data Flow by Feature Type</h4>
        <div className="space-y-2 font-mono text-xs text-slate-600">
          <div className="flex items-start gap-2">
            <span className="flex-shrink-0 px-1.5 py-0.5 bg-blue-100 text-blue-700 rounded text-xs font-bold">Factor</span>
            <span>
              <span className="text-blue-600">UI: "Female"</span>
              <span className="text-slate-400"> → </span>
              <span className="text-blue-600">{"JSON {Sex: 1}"}</span>
              <span className="text-slate-400"> → </span>
              <span className="text-emerald-600">R: factor("Female") → model.matrix() → SexFemale=1</span>
            </span>
          </div>
          <div className="flex items-start gap-2">
            <span className="flex-shrink-0 px-1.5 py-0.5 bg-purple-100 text-purple-700 rounded text-xs font-bold">Numeric</span>
            <span>
              <span className="text-blue-600">UI: "55"</span>
              <span className="text-slate-400"> → </span>
              <span className="text-blue-600">{"JSON {Age: 55}"}</span>
              <span className="text-slate-400"> → </span>
              <span className="text-emerald-600">R: as.numeric(55) → pass directly to model</span>
            </span>
          </div>
        </div>
      </div>
    </div>
  );
}

// ============================================================================
// Main App Component
// ============================================================================

export function App() {
  const [features, setFeatures] = useState<Record<string, number | null>>(getDefaultFeatures());
  const [result, setResult] = useState<PredictionResult | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [backendConnected, setBackendConnected] = useState<boolean | null>(null);
  const [isCheckingBackend, setIsCheckingBackend] = useState(true);
  const [showSummary, setShowSummary] = useState(false);
  const [validationError, setValidationError] = useState<string | null>(null);
  const resultsPanelRef = useRef<HTMLDivElement>(null);

  // Check backend health on mount
  useEffect(() => {
    let cancelled = false;
    setIsCheckingBackend(true);
    checkHealth().then((ok) => {
      if (!cancelled) {
        setBackendConnected(ok);
        setIsCheckingBackend(false);
      }
    });
    return () => { cancelled = true; };
  }, []);

  const handleChange = useCallback((key: string, value: number | null) => {
    setFeatures((prev) => ({ ...prev, [key]: value }));
    setResult(null);
    setError(null);
    setValidationError(null);
  }, []);

  const handlePredict = async () => {
    // Validate numeric fields
    const validation = validateFeatures(features);
    if (!validation.valid) {
      setValidationError(`Please fill in required fields: ${validation.missing.join(', ')}`);
      return;
    }

    setIsLoading(true);
    setError(null);
    setValidationError(null);
    setResult(null);

    try {
      // Convert null values shouldn't happen after validation, but safe-cast
      const payload: Record<string, number> = {};
      for (const [k, v] of Object.entries(features)) {
        payload[k] = v ?? 0;
      }
      const prediction = await fetchPrediction(payload);
      setResult(prediction);
      setShowSummary(true);
      setTimeout(() => {
        resultsPanelRef.current?.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }, 100);
    } catch (err) {
      const msg = err instanceof Error ? err.message : 'Unknown error occurred';
      setError(msg);
      checkHealth().then(setBackendConnected);
    } finally {
      setIsLoading(false);
    }
  };

  const handleReset = () => {
    setFeatures(getDefaultFeatures());
    setResult(null);
    setError(null);
    setValidationError(null);
    setShowSummary(false);
  };

  const handleLoadExample = () => {
    setFeatures(getExamplePatient());
    setResult(null);
    setError(null);
    setValidationError(null);
    setShowSummary(false);
  };

  const handleRetryConnection = async () => {
    setIsCheckingBackend(true);
    const ok = await checkHealth();
    setBackendConnected(ok);
    setIsCheckingBackend(false);
  };

  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-50 to-slate-100">
      {/* ================================================================ */}
      {/* Header */}
      {/* ================================================================ */}
      <header className="bg-gradient-to-r from-slate-800 via-slate-900 to-slate-800 text-white">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between py-3 border-b border-white/10 text-xs text-slate-400">
            <span>Clinical Decision Support Tool</span>
            <div className="flex items-center gap-3">
              <span className={`flex items-center gap-1.5 ${backendConnected ? 'text-emerald-400' : backendConnected === false ? 'text-amber-400' : 'text-slate-400'}`}>
                <span className={`w-1.5 h-1.5 rounded-full ${backendConnected ? 'bg-emerald-400' : backendConnected === false ? 'bg-amber-400' : 'bg-slate-400'}`}></span>
                {backendConnected ? 'R Backend Online' : backendConnected === false ? 'R Backend Offline' : 'Checking...'}
              </span>
            </div>
          </div>
          <div className="py-8 sm:py-10">
            <div className="flex items-start gap-5">
              <div className="hidden sm:flex flex-shrink-0 w-14 h-14 rounded-xl bg-gradient-to-br from-blue-500 to-cyan-400 items-center justify-center shadow-lg shadow-blue-500/20">
                <svg width="28" height="28" viewBox="0 0 24 24" fill="none" stroke="white" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
                  <path d="M12 2a10 10 0 1 0 10 10" />
                  <path d="M12 12 12 2" />
                  <path d="M12 12l7.07 7.07" />
                  <circle cx="12" cy="12" r="3" />
                </svg>
              </div>
              <div className="flex-1">
                <h1 className="text-2xl sm:text-3xl font-bold tracking-tight leading-tight">
                  Cross-Cohort Prediction Model for Acute Radiation Dermatitis
                  <br />
                  <span className="text-blue-300">in Proton Beam Therapy</span>
                </h1>
                <p className="mt-3 text-sm sm:text-base text-slate-300 leading-relaxed max-w-3xl">
                  A machine learning-based clinical prediction tool for estimating the risk of
                  acute radiation dermatitis at treatment completion, based on early-stage patient
                  characteristics. Powered by an R backend for model inference.
                </p>
                <div className="mt-4 flex flex-wrap items-center gap-x-5 gap-y-2 text-xs text-slate-400">
                  <span className="flex items-center gap-1.5">
                    <span className="w-2 h-2 rounded bg-emerald-500/80 flex-shrink-0 flex items-center justify-center text-white" style={{ fontSize: '8px', fontWeight: 'bold' }}>R</span>
                    Backend: R Plumber API
                  </span>
                  <span className="flex items-center gap-1.5">
                    <span className="w-1.5 h-1.5 rounded-full bg-blue-400"></span>
                    20 Features (16 categorical + 4 numeric)
                  </span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </header>

      {/* ================================================================ */}
      {/* Main Content */}
      {/* ================================================================ */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Backend Status */}
        <div className="mb-6">
          <BackendStatusBanner isConnected={backendConnected} isChecking={isCheckingBackend} />
          {backendConnected === false && !isCheckingBackend && (
            <button
              onClick={handleRetryConnection}
              className="mt-2 px-4 py-2 text-xs font-medium text-amber-700 bg-amber-50 hover:bg-amber-100 rounded-lg border border-amber-200 transition-colors duration-150 cursor-pointer"
            >
              Retry Connection
            </button>
          )}
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-12 gap-8">
          {/* ============================================================ */}
          {/* Left Panel: Input Form */}
          {/* ============================================================ */}
          <div className="lg:col-span-7 space-y-4">
            <div className="flex items-center justify-between">
              <div>
                <h2 className="text-lg font-bold text-slate-800">Input Features</h2>
                <p className="text-sm text-slate-500 mt-0.5">
                  Enter patient characteristics for risk prediction
                </p>
              </div>
              <div className="flex items-center gap-2">
                <button
                  onClick={handleLoadExample}
                  className="px-3 py-1.5 text-xs font-medium text-blue-700 bg-blue-50 hover:bg-blue-100 rounded-lg border border-blue-200 transition-colors duration-150 cursor-pointer"
                >
                  Load Example
                </button>
                <button
                  onClick={handleReset}
                  className="px-3 py-1.5 text-xs font-medium text-slate-600 bg-white hover:bg-slate-50 rounded-lg border border-slate-200 transition-colors duration-150 cursor-pointer"
                >
                  Reset All
                </button>
              </div>
            </div>

            {/* Feature type legend */}
            <div className="flex items-center gap-4 px-4 py-2.5 bg-white border border-slate-200 rounded-lg">
              <span className="text-xs text-slate-500 font-medium">Feature Types:</span>
              <span className="flex items-center gap-1.5 text-xs">
                <span className="w-3 h-3 rounded bg-blue-100 border border-blue-300"></span>
                <span className="text-slate-600">Categorical (dropdown)</span>
              </span>
              <span className="flex items-center gap-1.5 text-xs">
                <span className="w-3 h-3 rounded bg-purple-100 border border-purple-300"></span>
                <span className="text-slate-600">Numeric (free input)</span>
              </span>
            </div>

            {FEATURE_CATEGORIES.map((cat) => (
              <CategorySection
                key={cat.id}
                category={cat}
                features={features}
                onChange={handleChange}
              />
            ))}

            {/* Validation error */}
            {validationError && (
              <div className="bg-amber-50 border border-amber-200 rounded-xl p-4 flex items-start gap-3">
                <div className="flex-shrink-0 text-amber-500 mt-0.5"><IconWarning /></div>
                <div>
                  <div className="text-sm font-semibold text-amber-800">Missing Required Fields</div>
                  <div className="text-xs text-amber-600 mt-1">{validationError}</div>
                </div>
              </div>
            )}

            {/* Predict button */}
            <button
              onClick={handlePredict}
              disabled={isLoading || backendConnected === false}
              className={`w-full py-4 font-bold text-base rounded-xl shadow-lg transition-all duration-200 cursor-pointer flex items-center justify-center gap-2 ${
                isLoading || backendConnected === false
                  ? 'bg-slate-300 text-slate-500 cursor-not-allowed shadow-none'
                  : 'bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white shadow-blue-500/25 active:scale-[0.98]'
              }`}
            >
              {isLoading ? (
                <>
                  <IconLoader />
                  Predicting via R Backend...
                </>
              ) : backendConnected === false ? (
                'R Backend Not Connected'
              ) : (
                <>
                  <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
                    <path d="M5 12h14M12 5l7 7-7 7" />
                  </svg>
                  Generate Prediction
                </>
              )}
            </button>

            {/* Error display */}
            {error && (
              <div className="bg-red-50 border border-red-200 rounded-xl p-4 flex items-start gap-3">
                <div className="flex-shrink-0 text-red-500 mt-0.5"><IconWarning /></div>
                <div>
                  <div className="text-sm font-semibold text-red-800">Prediction Error</div>
                  <div className="text-xs text-red-600 mt-1">{error}</div>
                  <div className="text-xs text-red-500 mt-2">
                    Please verify the R backend is running and the model is loaded correctly.
                  </div>
                </div>
              </div>
            )}
          </div>

          {/* ============================================================ */}
          {/* Right Panel: Results */}
          {/* ============================================================ */}
          <div ref={resultsPanelRef} className="lg:col-span-5 space-y-5">
            <div>
              <h2 className="text-lg font-bold text-slate-800">Prediction Result</h2>
              <p className="text-sm text-slate-500 mt-0.5">
                Model output from R backend
              </p>
            </div>

            <ErrorBoundary>
            {!result ? (
              <div className="bg-white border-2 border-dashed border-slate-200 rounded-xl p-10 text-center">
                <div className="w-16 h-16 rounded-full bg-slate-100 flex items-center justify-center mx-auto mb-4">
                  <svg width="32" height="32" viewBox="0 0 24 24" fill="none" stroke="#94a3b8" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
                    <path d="M12 2a10 10 0 1 0 10 10" />
                    <path d="M12 12 12 2" />
                    <path d="M12 12l7.07 7.07" />
                  </svg>
                </div>
                <h3 className="text-base font-semibold text-slate-600">No Prediction Yet</h3>
                <p className="text-sm text-slate-400 mt-2 max-w-xs mx-auto leading-relaxed">
                  Select patient feature values on the left and click
                  <span className="font-semibold text-blue-600"> "Generate Prediction" </span>
                  to send data to the R backend for risk estimation.
                </p>
                <div className="mt-4 inline-flex items-center gap-1.5 text-xs text-slate-400 bg-slate-50 px-3 py-1.5 rounded-full">
                  <IconServer />
                  Prediction powered by R
                </div>
              </div>
            ) : (
              <>
                {/* Probability gauge */}
                <div className="bg-white border border-slate-200 rounded-xl p-6 shadow-sm">
                  <ProbabilityGauge probability={result.probability} threshold={result.threshold} />
                </div>

                {/* Classification */}
                <ClassificationResult result={result} />

                {/* Threshold information */}
                <div className="bg-white border border-slate-200 rounded-xl p-5 shadow-sm">
                  <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                    <IconInfo />
                    Classification Details
                  </h3>
                  <div className="space-y-3">
                    <div className="flex items-center justify-between py-2 border-b border-slate-100">
                      <span className="text-sm text-slate-600">Predicted Probability</span>
                      <span className={`text-sm font-bold ${result.is_high_risk ? 'text-red-600' : 'text-emerald-600'}`}>
                        {safeFixed(Number(result.probability) * 100, 3)}%
                      </span>
                    </div>
                    <div className="flex items-center justify-between py-2 border-b border-slate-100">
                      <span className="text-sm text-slate-600">Optimal Cutoff Threshold</span>
                      <span className="text-sm font-bold text-slate-800">
                        {safeFixed(Number(result.threshold) * 100, 1)}%
                      </span>
                    </div>
                    <div className="flex items-center justify-between py-2 border-b border-slate-100">
                      <span className="text-sm text-slate-600">Classification</span>
                      <span className={`text-sm font-bold px-2.5 py-1 rounded-full ${
                        result.is_high_risk
                          ? 'bg-red-100 text-red-700'
                          : 'bg-emerald-100 text-emerald-700'
                      }`}>
                        {result.classification}
                      </span>
                    </div>
                    <div className="flex items-center justify-between py-2">
                      <span className="text-sm text-slate-600">Distance from Threshold</span>
                      <span className="text-sm font-bold text-slate-800">
                        {Number(result.probability) >= Number(result.threshold) ? '+' : ''}{safeFixed((Number(result.probability) - Number(result.threshold)) * 100, 2)} pp
                      </span>
                    </div>
                  </div>
                </div>

                {/* Model performance */}
                <div className="bg-white border border-slate-200 rounded-xl p-5 shadow-sm">
                  <h3 className="text-sm font-bold text-slate-700 mb-3 flex items-center gap-2">
                    <IconInfo />
                    Model Performance at Optimal Threshold
                  </h3>
                  <MetricsTable metrics={result.metrics} />
                  <div className="mt-3 flex flex-wrap gap-2 text-xs text-slate-500">
                    <span className="bg-slate-50 px-2 py-1 rounded border border-slate-100">
                      {result.metrics.training_cohort}
                    </span>
                    <span className="bg-slate-50 px-2 py-1 rounded border border-slate-100">
                      {result.metrics.validation_cohort}
                    </span>
                    <span className="bg-slate-50 px-2 py-1 rounded border border-slate-100">
                      N = {result.metrics.total_patients}
                    </span>
                  </div>
                </div>

                {/* R Backend Info */}
                <div className="bg-slate-50 border border-slate-200 rounded-xl p-4 flex items-center gap-3">
                  <div className="w-8 h-8 rounded-lg bg-emerald-600 text-white flex items-center justify-center text-xs font-bold flex-shrink-0">R</div>
                  <div className="text-xs text-slate-600">
                    This prediction was computed by the R Plumber backend at{' '}
                    <code className="bg-white px-1 py-0.5 rounded border border-slate-200 text-slate-700">{API_BASE_URL}</code>.
                    All model loading, data preprocessing, and prediction logic are defined in{' '}
                    <code className="bg-white px-1 py-0.5 rounded border border-slate-200 text-slate-700">r_backend/plumber.R</code>.
                  </div>
                </div>

                {/* Feature summary toggle */}
                <button
                  onClick={() => setShowSummary(!showSummary)}
                  className="w-full text-left bg-white border border-slate-200 rounded-xl p-4 shadow-sm hover:bg-slate-50 transition-colors duration-150 cursor-pointer"
                >
                  <div className="flex items-center justify-between">
                    <h3 className="text-sm font-bold text-slate-700 flex items-center gap-2">
                      <IconInfo />
                      Input Summary (Sent to R Backend)
                    </h3>
                    <IconChevronDown open={showSummary} />
                  </div>
                </button>
                {showSummary && (
                  <FeatureSummaryTable features={features} />
                )}
              </>
            )}
            </ErrorBoundary>
          </div>
        </div>
      </main>

      {/* ================================================================ */}
      {/* Architecture Section */}
      {/* ================================================================ */}
      <section className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 pb-8">
        <ArchitectureDiagram />
      </section>

      {/* ================================================================ */}
      {/* Footer */}
      {/* ================================================================ */}
      <footer className="bg-slate-800 text-slate-400">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h3 className="text-sm font-bold text-slate-200 mb-2 flex items-center gap-2">
                <IconWarning />
                Disclaimer
              </h3>
              <p className="text-xs leading-relaxed">
                This prediction tool is intended for research and educational purposes only.
                It should not be used as the sole basis for clinical decision-making.
                Always consult qualified healthcare professionals for treatment decisions.
              </p>
            </div>
            <div>
              <h3 className="text-sm font-bold text-slate-200 mb-2">Citation</h3>
              <p className="text-xs leading-relaxed italic">
                [Author names]. Cross-Cohort Machine Learning Model for Predicting Acute
                Radiation Dermatitis in Patients Receiving Proton Beam Therapy.
                [Journal Name], [Year]. DOI: [to be assigned]
              </p>
              <p className="text-xs mt-3 text-slate-500">
                &copy; {new Date().getFullYear()} All rights reserved. For academic and research use.
              </p>
            </div>
          </div>
        </div>
      </footer>
    </div>
  );
}
