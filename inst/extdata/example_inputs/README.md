# Example inputs

These example files let you run affirmoTM straight away and learn how it works. `tm_example_inputs()` copies them
into the `inputs/` folder of your analysis folder. Replace them with your own files, using the same names and layout.

| File | What it is |
|---|---|
| `transition_matrix_usual_care.csv` | One-year transition probabilities under usual care, from a multinomial model fitted to the Swedish National Patient Register (SNPR), including the rows of the six death states. |
| `transition_matrix_iabc.csv` | One-year transition probabilities under iABC, in the same layout as the usual-care matrix. **Invented example, not real data:** it only shows what a real iABC file looks like. |
| `cohort.csv` | The patients to simulate, one per row: `id`, `age` (whole years, in the patient's entry cycle, not at cycle 0), `sex`, `mm_cluster`, and `entry_cycle` (the cycle in which the patient enters; 0 = 2013; patients entering in cycle `horizon` or later are left out, with a warning). The example holds 5,000 patients aged 65+ with atrial fibrillation, sampled from an IMPACT_AF synthetic population, entering between 2013 and 2022. An `agegrp` column is optional: if it's missing, it is worked out from `age`. |
| `entry_states.csv` | The probability of each state in a patient's entry year (their first AF year), one row per age group, sex and cluster; every row sums to 1. Shared by both arms. The example comes from the IMPACT_AF entry table. |
| `costs.csv` | Cost of a year spent in each state, one column per arm (`state, usual_care, iabc`). Every state needs a row, death states included (the cost of the year of death). The `iabc` column is for differences in care that depend on the state, such as medication; the programme cost itself goes in `intervention_cost.csv`. In a patient's entry year both arms are charged the `usual_care` cost. **Invented example values** in euros, the same in both arms. |
| `intervention_cost.csv` | The cost of delivering iABC to one patient in each year after entry (`year, cost`; `year` 1 is the first year after entry), one row for every year from 1 to `horizon - 1`. It is the same whatever the patient's state, and is charged to iABC patients alive in that year: never in the entry year, the year of death, or usual care. **Invented example values:** a one-year programme, €50 in year 1 and €0 in years 2 to 9. |
| `utilities.csv` | QALY weight of a year spent in each living state, one column per arm. Death states count 0 in the year of death and can be left out. In the entry year both arms take the `usual_care` value. **Invented example values**, the same in both arms. |
| `settings.yaml` | Run settings: `strata` (stratifier columns), `seed` (random draws), `horizon` (years, counted from cycle 0), `age_limit_death_state` (who dies after the oldest age group), `discount_rate` (0.03 = 3%), `n_runs` (number of runs), `events` (states counted as each event), `currency` (symbol for results), `wtp` (willingness-to-pay thresholds per QALY, drawn as lines on the CE plane). |

Both matrices use the same layout:

- one row per age group (`agegrp`), sex (`sex`), multimorbidity cluster (`mm_cluster`) and current state (`from`);
- one column per state, holding the probability of moving there within one year;
- every row sums to 1;
- every state has its own rows, death states included: the rows of a death state (`CurrentDeath`, `AB_Death`,
  `AHS_Death`, `AIS_Death`, `AHS_AB_Death`, `AIS_AB_Death`) go back to the same state with probability 1. This is how
  the package tells death states from living ones.

Your own matrices may use other stratifiers or none, but usual care and iABC must have the same stratification and the
same states. The age-group column must be called `agegrp`: it is the only stratifier that changes as patients age. A
column with any other name (for example `age_band`) is read as a fixed stratifier, so patients would keep their entry
age group for the whole horizon.

Apart from the intervention cost, the example files apply in the same way in every year. A matrix, `costs.csv` or
`utilities.csv` may also have a `year` column (years since entry, 1 = the first year after entry), so that
probabilities, costs or utilities change over time, for example an effect that lasts one year. Every year from 1 to
`horizon - 1` then needs a full set of rows (the rows for year `k` drive the move into year `k` after entry). The
entry year itself takes the usual-care cost and utility of year 1 in both arms.

The example costs and utilities are the same for every patient in a state. `costs.csv` and `utilities.csv` may also
have stratifier columns (any of the `strata` in `settings.yaml`), for example utilities by `agegrp` and `sex` or costs
by `mm_cluster`; each patient-year then uses the patient's stratum in that year, and every stratum needs a row for every
state.

The package checks the layout; the probabilities themselves are your responsibility.
