# NA

## Micro-DSL v3.4 Source Format Grammar

This is the HUMAN-READABLE source format for v3.4. It extends v3.3 with
semantic annotations and constrained types while maintaining clarity and
completeness. A separate compilation step produces the compressed
format.

**Budget**: Target ≤ 250 lines or ≤ 2,500 tokens for optimal LLM
processing.

**1. Document Structure:**

    @pkg package_name | description
    [Type Aliases section]
    [Constraint Definitions section]
    [Legend section if needed]
    # Package Name
    [Sections with entries]
    [Dependencies section]
    [Meta-Footer]

**2. Sigils (Same as v3.3):**

    @pkg - Package declaration
    @f   - Function
    @d   - Data object
    @x   - Re-export from another package

    S3 System:
    @s3g - S3 generic (UseMethod)
    @s3m - S3 method (generic.class)
    @s3c - S3 class definition

    S4 System:
    @s4g - S4 generic (setGeneric)
    @s4m - S4 method (setMethod)
    @s4c - S4 class (setClass)

    S7 System:
    @s7g - S7 generic (new_generic)
    @s7m - S7 method
    @s7c - S7 class (new_class)

    R6 System:
    @r6c - R6 class (R6Class)

**3. Type System (v3.4 Enhanced with Constraints):**

    Scalars (default): int, dbl, chr, lgl, raw, cpl
    Vectors: vec<type> or type[]
    Matrices: mat<type> or mat<type,rows,cols>
    Arrays: arr<type,dims>
    Lists: lst<type> or lst{field:type, ...} (structured)
    Data frames: df, tbl, data.table
    Factors: fct, ord
    Dates: Date, POSIXct, POSIXlt

    Union types: type1|type2|type3
    Nullable: type? (shorthand for type|NULL)
    Any type: any
    Ellipsis: ... or ...:type (e.g., ...:expr for NSE)

    Class types: s3:classname, s4:classname, r6:classname, s7:classname

    CONSTRAINED TYPES (v3.4):
    Enums: chr["opt1"|"opt2"|"opt3"]
    Ranges: int[min..max], dbl[min..max]
    Patterns: chr[/regex/]
    Exclusions: int[1..100]&!=[13,17]
    References: @ref:constraint_name

**4. Entry Format (v3.4 Enhanced):**

    @sigil name (param1:type1[constraint]?=default, param2:type2, ...) | Description -> return_type | return_schema
      +tag:value +tag:value
      !cov [Class1, Class2] (for generics)
      - param1 : Additional description
        @annotation:value @annotation:value
      - param2 : (constants: "a", "b", CONST) Valid values
        @requires:condition @affects:target
      - param3 : (key_funcs: func1, func2) Related functions
        @lifecycle:init @units:measurement
      ```verbatim
      # Optional verbatim R code block

    **5. Type Aliases Section (v3.4 Enhanced):**
    ```markdown
    ## Type Aliases:
    DF = df|tbl|data.table              # Standard data frame types
    V<T> = vec<T>                       # Vector shorthand
    Fml = s3:formula                    # Formula objects
    Gg = s3:ggplot                      # ggplot2 objects
    Config = lst{method:chr, opts:lst}  # Structured config
    ValidPort = int[1024..65535]       # Constrained port range

**Standard aliases** (use these by default): - `DF` for data frame
arguments - `Fml` for formula arguments (not `fml`) - `V<T>` for vectors
when brevity helps

**6. Constraint Definitions (v3.4 New):**

``` markdown
## Constraint Definitions:
@constraint positive_weights | Positive numeric weights
  type: vec<dbl>
  validates: all(. > 0)
  length: @env:nrow(data)
  
@constraint valid_identifier | Valid R identifier
  type: chr
  pattern: /^[a-zA-Z_][a-zA-Z0-9_.]*$/
  not_reserved: TRUE
```

**7. Class Documentation (v3.4 Enhanced):**

    @s4c ClassName | One-line description
      - slots: name1 (type1[constraint]) @annotation:value
               name2 (type2) @lifecycle:init @immutable
      - extends: ParentClass
      - validity: Description of validity rules

    @r6c ClassName | One-line description
      - fields: field1 (type1[constraint]) @purpose:role
                field2 (type2) @lazy @cached
      - methods: method1 (args) -> ret_type
                 method2 (args) -> ret_type
      - inherits: ParentClass

**8. Metadata Tags (v3.3 + v3.4 additions):**

    +family:group_name           # Function family
    +pipe:in|out                # Pipe compatibility (in, out, or both)
    +nse:param1,param2          # Parameters using NSE
    +side:effect[details]       # Side effects with sub-facets
      - fs[read|write|delete]   # File system operations
      - plot[device|file]       # Graphics output
      - console[print|message|warning]  # Console output
      - network[http|socket|download]   # Network operations
      - options[get|set|env]    # Global options/environment
      - db[read|write|query]    # Database operations
    +perf:O(complexity)         # Performance complexity
    +mem:usage                  # Memory usage pattern
    +compute:intensity          # Computational intensity
    +deprecated:replacement     # Deprecation with suggested alternative
    +wraps:function            # This function wraps another
    +calls:func1,func2         # Functions called internally
    +see:related1,related2     # Related functions to consider
    +parallel:capable          # Can use parallel processing (v3.4)
    +deterministic:false       # Non-deterministic results (v3.4)
    +pure:false               # Has side effects (v3.4)

**9. Semantic Annotations (v3.4 New):**

    BEHAVIORAL:
    @controls:aspect          # Parameter controls specific behavior
    @affects:target          # Changes affect another component
    @modifies:target         # Directly modifies target

    DEPENDENCY:
    @requires:condition      # Prerequisite condition
    @conflicts:parameter     # Mutually exclusive with
    @extends:base           # Extends functionality

    VALIDATION:
    @validates:constraint    # Validation rule
    @range:[min,max]        # Numeric range
    @length:constraint      # Length requirement
    @pattern:regex          # Pattern matching

    SEMANTIC ROLE:
    @purpose:role           # Semantic purpose
    @units:measurement      # Physical/logical units
    @example:value          # Example values
    @default-reason:why     # Why this default

    LIFECYCLE:
    @lifecycle:stage        # When relevant (init|config|runtime|cleanup)
    @immutable             # Cannot be modified
    @cached                # Result is cached
    @lazy                  # Evaluated on demand

    CONDITIONAL:
    @when:condition        # Conditional applicability
    @implies:consequence   # Logical implication
    @if:cond @then:result  # If-then constraints

**10. Structured Return Types (v3.4 New):**

    -> lst{
      field1: type1 @annotation,
      field2: type2[constraint] @annotation,
      nested: lst{
        subfield: type
      }
    }

**11. Example Entry (v3.4):**

``` markdown
@f analyze_model (
  model:s3:lm,
  type:chr["summary"|"anova"|"diagnostics"]?="summary",
  conf.level:dbl[0.5..0.99]?=0.95
) | Analyze fitted model -> lst{
  statistics: df @purpose:results,
  plots: lst<s3:ggplot>? @when:type="diagnostics",
  interpretation: chr @purpose:summary
}
  +family:analysis +compute:light
  - model : @requires:fitted @validates:has-residuals
  - type : @controls:output-format @affects:return-structure
  - conf.level : @purpose:confidence @affects:statistics.ci
```

**12. Conditional Constraints (v3.4):**

``` markdown
@f process_data (
  data:df,
  method:chr["scale"|"center"|"none"]?="none",
  scale.center:lgl?=TRUE,
  scale.scale:lgl?=TRUE
) | Process data with scaling options -> df
  - method : @controls:processing
  - scale.center : @when:method="scale" @requires:TRUE
                   @when:method="center" @implies:scale.scale=FALSE
  - scale.scale : @when:method="scale" @default:TRUE
                  @conflicts:method="center"
```

**13. Best Practices (v3.4):** - Use specific sigils (@s3g not @g) -
Always specify vector vs scalar types - Use standard type aliases (DF,
Fml, V) - Add constraints from match.arg/stopifnot/checks - Keep !cov
lists short (3-6 classes max) - Document semantic relationships
concisely - Use structured types for complex returns - Define reusable
constraints with @constraint - Include conditional logic with
@when/@implies - Group related functions with +family tags - Mark side
effects with detailed sub-facets - Stay within budget (≤250 lines)

**14. Meta-Footer:**

``` markdown
---
## Meta-Footer
- Micro-DSL Version: v3.4-source
- Package: {pkg} (Version: X.Y.Z)
- Generated: [ISO-8601 timestamp]
- Features: types[constrained] sigils[specific] metadata[rich] semantics[annotated]
- Coverage: {n_documented_exports} / {n_total_exports} exports
- Provenance: exports[NAMESPACE], enums[match.arg/switch], constraints[assertions/checks]
```

**15. Export Detection Priority:** 1. NAMESPACE file: `export()`,
`S3method()`, `exportClasses()`, `exportMethods()`, `exportPattern()` 2.
Roxygen tags: `@export` in documentation 3. If neither present: skip the
symbol (do not guess or include)

**16. Inference Heuristics (apply silently):** - Type from defaults:
TRUE/FALSE → lgl, “text” → chr, 1L → int, 1.0 → dbl - Common patterns:
data/df/tbl → DF, formula → Fml, weights → vec - Enums: match.arg(x,
c(“a”,“b”)) → chr\[“a”\|“b”\] - Ranges: stopifnot(x \>= 0 && x \<= 1) →
dbl\[0..1\] - Side effects: file.\* → fs, plot/ggplot → plot,
message/cat → console - Determinism: runif/sample/rnorm →
+deterministic:false

------------------------------------------------------------------------

``` markdown
@pkg neuroatlas | Neuroimaging Atlases and Parcellations

## Type Aliases:
DF = df|tbl|data.table
V<T> = vec<T>
Fml = s3:formula
Gg = s3:ggplot

## Constraint Definitions:
@constraint positive_weights | Positive numeric weights
  type: vec<dbl>
  validates: all(. > 0)

# neuroatlas

## 1. Core

@f check_templateflow () | Check TemplateFlow installation status -> invisible<NULL>
  +family:templateflow +side:console[print]

@f clear_templateflow_cache (confirm:lgl?=TRUE) | Clear TemplateFlow cache -> invisible<lgl>
  +family:templateflow +side:fs[delete]
  - confirm : @default:TRUE

@f create_templateflow (cache_dir:chr?=NULL, verbosity:int?=0, default_template:chr?=NULL) | Create TemplateFlow interface -> s3:templateflow
  +family:templateflow +side:options[set]
  - cache_dir : @default:NULL
  - verbosity : @default:0
  - default_template : @default:NULL

@f dilate_atlas (atlas:s3:atlas, mask:s3:NeuroVol, radius:dbl?=4, maxn:int?=50) | Dilate atlas parcellation -> s3:ClusteredNeuroVol
  +family:atlas +compute:moderate
  - atlas : @requires:atlas
  - mask : @requires:NeuroVol
  - radius : @default:4
  - maxn : @default:50

@f get_aseg_atlas (outspace:s3:NeuroSpace?=NULL) | Get FreeSurfer ASEG atlas -> lst{name:chr, atlas:s3:NeuroVol, cmap:DF, ids:vec<int>, labels:vec<chr>, hemi:vec<chr>}
  +family:atlas +side:fs[read]
  - outspace : @default:NULL

@f get_glasser_atlas (outspace:s3:NeuroSpace?=NULL) | Get Glasser atlas -> lst{name:chr, atlas:s3:ClusteredNeuroVol, cmap:DF, ids:vec<int>, labels:vec<chr>, hemi:vec<chr>}
  +family:atlas +side:fs[read]
  - outspace : @default:NULL

@f get_hipp_atlas (outspace:s3:NeuroSpace?=NULL, apsections:int?=1) | Get hippocampal atlas -> lst{name:chr, atlas:s3:NeuroVol, ids:vec<int>, labels:vec<chr>, hemi:vec<chr>, cmap:DF}
  +family:atlas +side:fs[read]
  - outspace : @default:NULL
  - apsections : @default:1

@f get_olsen_mtl (outspace:s3:NeuroSpace?=NULL) | Get Olsen MTL atlas -> lst{name:chr, atlas:s3:NeuroVol, labels:vec<chr>, orig_labels:vec<chr>, ids:vec<int>, hemi:vec<chr>}
  +family:atlas +side:fs[read]
  - outspace : @default:NULL

@f get_roi (x:s3:atlas, label:chr?=NULL, id:int?=NULL, hemi:chr?=NULL) | Extract ROI from atlas -> lst<s3:ROIVol>
  +family:atlas +pipe:in +nse:label,id
  - x : @requires:atlas
  - label : @default:NULL
  - id : @default:NULL
  - hemi : @default:NULL

@f get_schaefer_atlas (parcels:chr["100"|"200"|"300"|"400"|"500"|"600"|"700"|"800"|"900"|"1000"]?="400", networks:chr["7"|"17"]?="17", resolution:chr["1"|"2"]?="1", outspace:s3:NeuroSpace?=NULL, smooth:lgl?=FALSE, use_cache:lgl?=TRUE) | Get Schaefer atlas -> lst{name:chr, atlas:s3:ClusteredNeuroVol, cmap:DF, ids:vec<int>, labels:vec<chr>, orig_labels:vec<chr>, network:vec<chr>, hemi:vec<chr>}
  +family:atlas +side:fs[read]
  - parcels : @default:400
  - networks : @default:17
  - resolution : @default:1
  - outspace : @default:NULL
  - smooth : @default:FALSE
  - use_cache : @default:TRUE

@f get_template (space:chr?="MNI152NLin2009cAsym", variant:chr?="brain", modality:chr?="T1w", resolution:int?=1, cohort:chr?=NULL, desc:chr?=NULL, label:chr?=NULL, atlas:chr?=NULL, suffix:chr?=NULL, extension:chr?=".nii.gz", path_only:lgl?=FALSE, use_cache:lgl?=TRUE, api_handle:s3:templateflow?=NULL, ...:expr) | Fetch template from TemplateFlow -> s3:NeuroVol|chr
  +family:templateflow +side:fs[read]
  - space : @default:MNI152NLin2009cAsym
  - variant : @default:brain
  - modality : @default:T1w
  - resolution : @default:1
  - cohort : @default:NULL
  - desc : @default:NULL
  - label : @default:NULL
  - atlas : @default:NULL
  - suffix : @default:NULL
  - extension : @default:.nii.gz
  - path_only : @default:FALSE
  - use_cache : @default:TRUE
  - api_handle : @default:NULL

@f install_templateflow (method:chr?="auto", conda:chr?="auto", envname:chr?=NULL, force_reinstall:lgl?=FALSE) | Install TemplateFlow Python package -> invisible<NULL>
  +family:templateflow +side:console[print]
  - method : @default:auto
  - conda : @default:auto
  - envname : @default:NULL
  - force_reinstall : @default:FALSE

@f map_atlas (x:s3:atlas, vals:vec<dbl>, thresh:vec<dbl>?=c(0,0), pos:lgl?=FALSE, ...:expr) | Map values to atlas -> s3:atlas
  +family:atlas +pipe:in
  - x : @requires:atlas
  - vals : @requires:length(vals) == length(x$orig_labels)
  - thresh : @default:c(0,0)
  - pos : @default:FALSE

@f merge_atlases (atlas1:s3:atlas, atlas2:s3:atlas) | Merge two atlases -> s3:atlas
  +family:atlas +compute:moderate
  - atlas1 : @requires:atlas
  - atlas2 : @requires:atlas

@f plot_glasser (vals:DF?=NULL, value_col:chr?="value", position:chr["dispersed"|"stacked"]?="dispersed") | Plot Glasser atlas values -> Gg
  +family:plot +side:plot[device]
  - vals : @default:NULL
  - value_col : @default:value
  - position : @default:dispersed

@f reduce_atlas (atlas:s3:atlas, data_vol:s3:NeuroVol, stat_func:fn, ...:expr, format:chr["wide"|"long"]?=NULL) | Reduce atlas by summary function -> DF
  +family:atlas +pipe:in
  - atlas : @requires:atlas
  - data_vol : @requires:NeuroVol
  - stat_func : @requires:function
  - format : @default:NULL

@f show_templateflow_cache_path () | Show TemplateFlow cache path -> chr
  +family:templateflow +side:console[print]

@f tflow_files (space:chr, query_args:lst?=list(), api_handle:s3:templateflow?=NULL) | List TemplateFlow files -> vec<chr>
  +family:templateflow +side:fs[read]
  - space : @requires:space
  - query_args : @default:list()
  - api_handle : @default:NULL

@f tflow_spaces (pattern:chr?=NULL, api_handle:s3:templateflow?=NULL, ...:expr) | List TemplateFlow template spaces -> vec<chr>
  +family:templateflow +side:fs[read]
  - pattern : @default:NULL
  - api_handle : @default:NULL

## Dependencies
- Imports: assertthat, cli, crayon, downloader, dplyr, ggiraph, ggplot2, ggseg, lifecycle, magrittr, memoise, methods, neuroim2, neurosurf, reticulate, Rnanoflann, scales, scico, sf, stringr, tibble, tidyr, tools, utils
- Suggests: echarts4r, geojsonio, geojsonsf, ggsegGlasser, ggsegSchaefer, knitr, rmarkdown, testthat (>= 3.0.0)

---
## Meta-Footer
- Micro-DSL Version: v3.4-source
- Package: neuroatlas (Version: 0.1.0)
- Generated: 2023-10-05T12:00:00Z
- Features: types[constrained] sigils[specific] metadata[rich] semantics[annotated]
- Coverage: 30 / 30 exports
- Provenance: exports[NAMESPACE], enums[match.arg/switch], constraints[assertions/checks]
```
