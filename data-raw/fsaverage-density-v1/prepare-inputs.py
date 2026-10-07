#!/usr/bin/env python3
"""Fetch pinned upstream bytes, verify annex/archive MD5, lock SHA-256 identities.

Run with a local input directory argument. No raw inputs are distributed.
"""
import hashlib
import json
import re
import sys
import tarfile
import urllib.request
from pathlib import Path

work = Path(sys.argv[1])
work.mkdir(parents=True, exist_ok=True)
revision = '8e53ba4f2e438758f69d11436fe0cd291a28bec6'
neuromaps = 'ffcc2e0f657943ce00a1b6a968396f32250e495c'
manifest_url = f'https://raw.githubusercontent.com/netneurolab/neuromaps/{neuromaps}/neuromaps/datasets/data/osf.json'
sha = lambda b: hashlib.sha256(b).hexdigest()
lock = dict(schema='neuroatlas.surface-inputs.v1', artifact_version='surface-density-inputs-v1',
            qualification='checksum_locked_inputs_only', redistribution='upstream_download_only',
            upstream_revisions=dict(fsaverage=revision, neuromaps=neuromaps), archives=[], assets=[])
for density, ident, md5 in [('10k', '60b684ab9096b7021b63cf6b', 'c61384c271ee2e6b5449222281137414'),
                           ('41k', '60b684aecb2a5e01fc68b7e1', '0cc48e9d5d5bb0216502888c954805fd')]:
    archive_id = 'neuromaps-fsaverage'+density
    url = 'https://files.osf.io/v1/resources/4mw3a/providers/osfstorage/'+ident
    path = work / (archive_id+'.tar.gz')
    # Previously fetched files may use the shorter fsaverage<density> name.
    old = work / ('fsaverage'+density+'.tar.gz')
    if not path.exists():
        if old.exists():
            path.write_bytes(old.read_bytes())
        else:
            urllib.request.urlretrieve(url, path)
    data = path.read_bytes()
    assert hashlib.md5(data).hexdigest() == md5
    lock['archives'].append(dict(id=archive_id, url=url, size_bytes=len(data), sha256=sha(data),
                                 upstream_md5=md5, upstream_revision=neuromaps, upstream_manifest=manifest_url))
    with tarfile.open(path) as tar:
        for hemi in ('L', 'R'):
            sphere = f'tpl-fsaverage_hemi-{hemi}_den-{density}_sphere.surf.gii'
            raw = f'https://raw.githubusercontent.com/templateflow/tpl-fsaverage/{revision}/{sphere}'
            annex = urllib.request.urlopen(raw).read().decode().strip().split('/')[-1]
            match = re.fullmatch(r'MD5E-s(\d+)--([0-9a-f]+).*', annex)
            assert match is not None
            url = 'https://templateflow.s3.amazonaws.com/tpl-fsaverage/'+sphere
            file = work / sphere
            if not file.exists():
                urllib.request.urlretrieve(url, file)
            data = file.read_bytes()
            assert len(data) == int(match[1]) and hashlib.md5(data).hexdigest() == match[2]
            lock['assets'].append(dict(path=sphere, template='fsaverage', density=density,
                hemisphere=hemi, role='sphere', url=url, upstream_revision=revision,
                upstream_annex_key=annex, upstream_md5=match[2], size_bytes=len(data), sha256=sha(data)))
            for role, suffix in [('mask', 'desc-nomedialwall_dparc.label.gii'),
                                 ('area', 'desc-vaavg_midthickness.shape.gii'), ('ordering_reference', 'sphere.surf.gii')]:
                member = f'atlases/fsaverage/tpl-fsaverage_den-{density}_hemi-{hemi}_{suffix}'
                data = tar.extractfile(member).read()
                filename = member.split('/')[-1]
                (work / filename).write_bytes(data)
                lock['assets'].append(dict(path=filename, template='fsaverage', density=density,
                    hemisphere=hemi, role=role, archive=archive_id, member=member,
                    url=lock['archives'][-1]['url'], size_bytes=len(data), sha256=sha(data)))
            print('Locked', density, hemi, flush=True)
Path('inst/extdata/surface-density-inputs-v1.json').write_text(json.dumps(lock, indent=2)+'\n')
