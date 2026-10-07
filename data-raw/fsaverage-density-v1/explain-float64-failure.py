"""Explain the retained float64 failure; this diagnostic never admits a route."""
import hashlib
import sys
import numpy as np,mpmath as mp,json
from pathlib import Path
mp.mp.dps=80
base=Path(sys.argv[1])/'fsaverage-10k-L_to_fsaverage-164k-L'
records=[]
v=np.fromfile(base/'source.bin','<f8').reshape(-1,3);f=np.fromfile(base/'faces.bin','<i4').reshape(-1,3);q=np.fromfile(base/'target.bin','<f8').reshape(-1,3)[72203]
v=100*v/np.linalg.norm(v,axis=1)[:,None];q=100*q/np.linalg.norm(q)
w=np.fromfile(base/'weights.bin','<f8').reshape(-1,3);native=w[w[:,0]==72203];cs=native[:,1].astype(int)
native_list=native.tolist()
faces=np.where(np.sum(np.isin(f,cs),axis=1)>=2)[0]
Q=mp.matrix([mp.mpf(float(x)) for x in q])
for fi in faces:
 T=[mp.matrix([mp.mpf(float(x)) for x in a]) for a in v[f[fi]]]
 E=mp.matrix(3,2)
 for i in range(3):E[i,0]=T[1][i]-T[0][i];E[i,1]=T[2][i]-T[0][i]
 uv=mp.lu_solve(E.T*E,E.T*(Q-T[0]));b=mp.matrix([1-uv[0]-uv[1],uv[0],uv[1]])
 P=T[0]+E*uv;dist=mp.fdot(Q-P,Q-P)
 records.append(dict(kind='interior', face=int(fi), columns=f[fi].tolist(),
  weights=[str(x) for x in b], squared_distance=str(dist), valid=bool(min(b)>=0)))
 for a,z in [(0,1),(1,2),(2,0)]:
  D=T[z]-T[a];u=max(mp.mpf(0),min(mp.mpf(1),mp.fdot(Q-T[a],D)/mp.fdot(D,D)))
  P=T[a]+u*D;distance=mp.fdot(Q-P,Q-P)
  if set([int(f[fi,a]),int(f[fi,z])])==set([4511,7459]):
   records.append(dict(kind='edge', columns=[int(f[fi,a]),int(f[fi,z])],
    squared_distance=str(distance), parameter=str(u)))

result=dict(target_zero_based=72203, decimal_digits=mp.mp.dps, native=native_list,
 case_manifest_sha256=hashlib.sha256((base/'case.json').read_bytes()).hexdigest(),
 script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
 calculations=records)
print(json.dumps(result,indent=2))
