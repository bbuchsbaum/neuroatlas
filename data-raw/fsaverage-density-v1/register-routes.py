#!/usr/bin/env python3
"""Admit eight exact directed routes only after all frozen numerical gates pass."""
import csv
import hashlib
import json
import sys
from pathlib import Path

base = Path(sys.argv[1])
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
contract = Path('data-raw/fsaverage-density-v1/contract-v1.json')
consumer = json.loads((base/'consumer-receipt.json').read_text())
scripts = {'oracle-receipt.json': ('oracle_sha256', 'qualify-oracle-precise.py'),
           'full-array-receipt.json': ('script_sha256', 'check-full-arrays.py'),
           'policy-receipt.json': ('script_sha256', 'check-policies.R')}
for file, (key, script) in scripts.items():
    receipt = json.loads((base/file).read_text())
    assert receipt['status'] == 'PASS' and receipt['contract_sha256'] == sha(contract)
    assert receipt['consumer_receipt_sha256'] == sha(base/'consumer-receipt.json')
    assert receipt[key] == sha(Path('data-raw/fsaverage-density-v1')/script)
assert json.loads((base/'oracle-receipt.json').read_text())['refinement_sha256'] == sha('data-raw/fsaverage-density-v1/oracle-refinement-v1.json')
for path, expected in consumer['consumer_source_sha256'].items():
    assert sha(path) == expected
old = json.loads(Path('inst/extdata/surface-domains-v1.json').read_text())
new = json.loads(Path('inst/extdata/surface-density-domains-v1.json').read_text())
assert old['input_lock_sha256'] == 'efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0'
assert new['input_lock_sha256'] == sha('inst/extdata/surface-density-inputs-v1.json')
domains = {**old['domains'], **new['domains']}
ids = {entry['domain']['id']: entry['domain'] for entry in domains.values()}
path = Path('inst/extdata/transform_registry.csv')
with path.open(newline='') as f:
    reader = csv.DictReader(f)
    columns = list(reader.fieldnames)
    rows = list(reader)
for field in ['from_density', 'to_density']:
    if field not in columns:
        columns.append(field)
for row in rows:
    for side in ['from', 'to']:
        row[side+'_density'] = ids.get(row[side+'_domain_id'], {}).get('density', 'NA')
    if row['backend'] == 'sphere_nn':
        row['notes'] = 'Broad nearest-neighbour roadmap proposal; exact native barycentric density routes are registered separately'
existing = [row for row in rows if row['artifact_version'] == 'surface-density-transforms-v1']
rows = [row for row in rows if row['artifact_version'] != 'surface-density-transforms-v1']
new_rows = []
assert len(consumer['routes']) == 8
for name, measured in consumer['routes'].items():
    source, target = name.split('_to_')
    src, dst = domains[source]['domain'], domains[target]['domain']
    assert measured['from_domain_id'] == src['id'] and measured['to_domain_id'] == dst['id']
    space = lambda d: {'164k': 'fsaverage', '41k': 'fsaverage6', '10k': 'fsaverage5'}[d['density']]
    row = dict.fromkeys(columns, 'NA')
    row.update(from_space=space(src), to_space=space(dst), transform_type='sphere_resample',
        backend='neurotransform_native', confidence='approximate', reversible='FALSE', status='available',
        notes='Exact pinned domains; independent numerical qualification. Lossy directed resampling; no anatomical accuracy, conservation or inverse claim',
        artifact_id=f'native_density_{src["hemisphere"]}_{src["density"]}_to_{dst["density"]}',
        artifact_version='surface-density-transforms-v1', provider='neuroatlas', format='native_barycentric',
        convention='sphere_correspondence_fsaverage', qualification='passed',
        qualification_scope='native_closest_barycentric_exact_density_domains_continuous_label_probability',
        qa_url='https://github.com/bbuchsbaum/neuroatlas/blob/master/data-raw/fsaverage-density-v1/README.md',
        license='upstream_download_only; see surface-assets-LICENSES.md',
        from_domain_id=src['id'], to_domain_id=dst['id'], method='native_closest_barycentric',
        engine_revision='933edddda462593941e167726e8aaa7168ff103a', input_lock_sha256=new['input_lock_sha256'],
        hemisphere=src['hemisphere'], from_density=src['density'], to_density=dst['density'])
    new_rows.append(row)
if existing:
    assert existing == new_rows, 'Existing density route identities differ; refuse replacement'
    print('Verified 8 existing exact directed density routes; registry unchanged')
    sys.exit(0)
rows.extend(new_rows)
with path.open('w', newline='') as f:
    writer = csv.DictWriter(f, columns, lineterminator='\n')
    writer.writeheader()
    writer.writerows(rows)
print('Admitted 8 exact directed density routes')
